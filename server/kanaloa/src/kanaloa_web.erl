%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc Web server for kanaloa. Starts up the mochiweb_http module.

-module(kanaloa_web).
-author('Stephen Schwink <kanaloa@schwink.net>').

%% @headerfile "../include/kanaloa.hrl"
-include("../include/kanaloa.hrl").

-export([start_link/2, stop/0, loop/2]).

%% External API

%% @spec start_link(MochiOptions::proplist(), Settings::kanaloa_settings()) -> {ok, Mochiweb::pid()}
%% @doc Starts mochiweb_http with the appropriate configuration.
start_link(MochiwebOptions, Settings) ->
    io:format("Kanaloa settings is ~w\n", [Settings]),
    
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, Settings)
           end,
    
    Result = mochiweb_http:start([{loop, Loop} | MochiwebOptions]),
    io:format("mochiweb_http:start result: ~w\n", [Result]),
    Result.

%% @doc Stops mochiweb_http.
stop() ->
    mochiweb_http:stop(?MODULE).

%% @hidden
loop(Req, Settings) when is_record(Settings, kanaloa_settings) ->
    case catch parse_request(Req) of
	{ok, options} ->
	    handle_options_request(Req);
	{ok, CometMethod, ConnectionId, Body} ->
	    case catch parse_body(Body, Settings) of
		{ok, DataSegments} ->
		    handle_connection_request(Req, Settings, CometMethod, ConnectionId, DataSegments);
		Error ->
		    io:format("Body parse error: ~w\n", [Error]),
		    Req:respond({400, [], []}) % Bad request
	    end;
	ParseError ->
	    io:format("Request parse error: ~w\n", [ParseError]),
	    Req:respond({400, [], []}) % Bad request
    end.

%% Internal API

dispatch_chunks(_, [], _) ->
    ok;
dispatch_chunks(Owner, [Data| Rest], Kind) ->
    Owner ! {Kind, Data},
    dispatch_chunks(Owner, Rest, Kind).

dispatch_connection(Owner, NewConnection) ->
    Owner ! {connection, NewConnection}.

%% @spec get_comet_method(Request::mochiweb_request()) -> 'longpoll' | 'stream' | 'none'
get_comet_method(Req) ->
    QS = Req:parse_qs(),
    parse_query_string(QS).

%% @spec get_connection_id(Request::mochiweb_request()) -> binary() | none
get_connection_id(Req) ->
    case Req:get_header_value("ConnectionId") of
	undefined ->
	    none;
	Value when is_list(Value) ->
	    list_to_binary(Value);
	Value when is_binary(Value) ->
	    Value
    end.

get_connection_owner(ConnectionId) ->
    case ConnectionId of
	none ->
	    new;
	Binary when is_binary(Binary) ->
	    case kanaloa_state_server:get_owner(ConnectionId) of
		Owner when is_pid(Owner) ->
		    {ok, Owner};
		_ ->
		    expired
	    end
    end.

%% @spec get_path(Request::mochiweb_request()) -> string()
%% @doc Gets the path that the request was made to.
get_path(Req) ->
    FullPath = Req:get(path),
    FullPath.

handle_connection_request(Req, Settings, CometMethod, ConnectionId, Data) ->
    IsDownloadRequest = (CometMethod /= none),
    ChunkKind = case Settings#kanaloa_settings.parse_jsonrpc of
		    true -> json;
		    false -> chunk
		end,
    case get_connection_owner(ConnectionId) of
	expired -> % The owner has died. Tell the client to reconnect.
	    Req:respond({410, [], []}), % Gone
	    exit(bad_connection_owner);
	
	{ok, ExistingOwner} when not IsDownloadRequest -> % An upload request is posting data.
	    dispatch_chunks(ExistingOwner, Data, ChunkKind),
	    Req:ok({Settings#kanaloa_settings.http_content_type, [], []});
	
	{ok, ExistingOwner} when IsDownloadRequest -> % A download request is attempting to reconnect.
	    Connection = new_connection(Req, Settings, ConnectionId, CometMethod),
	    
	    % Try again to send any messages left over from the previous connection.
	    % This list will be non-empty only if the connection was interrupted from the client side.
	    PendingMessages = kanaloa_state_server:pop_pending(ConnectionId),
	    Connection:log("Re-sending list of ~w messages", [length(PendingMessages)]),
	    lists:map(fun (M) ->
			      Connection:send_json(M)
		      end, PendingMessages),
	    
	    dispatch_connection(ExistingOwner, Connection),
	    
	    % Note that the current Kanaloa client does not send upload data in download connection requests.
	    dispatch_chunks(ExistingOwner, Data, ChunkKind),
	    
	    open_connection(Connection, ExistingOwner);
	
	new when IsDownloadRequest -> % A new connection is being initiated.
	    NewConnectionId = kanaloa_state_server:new_guid(),

	    NewSettings = case CometMethod of
			   longpoll ->
			          % For new longpoll, we want to close the initial response quickly, after 1 batch.
			          % We need to send back the ConnectionId so the client can finish initializing.
				  Settings#kanaloa_settings{ batch_count = 1 };
			      _ ->
				  Settings
			  end,
	    Connection = new_connection(Req, NewSettings, NewConnectionId, CometMethod),
	    
	    Handler = Settings#kanaloa_settings.handler,
	    NewOwner =  spawn(fun () ->
				      Connection:log("Owner process spawned", []),
				      process_flag(trap_exit, true),
				      Handler(Connection)
			      end),
	    
	    ok = kanaloa_state_server:new_state(NewConnectionId, NewOwner),
	    
	    open_connection(Connection, NewOwner)
    end.

%% @doc Responds properly to the HTTP OPTIONS request sent by Chrome if the request is cross-origin.
%% Note that this doesn't work :(
%% http://code.google.com/p/chromium/issues/detail?id=20624
handle_options_request(Req) ->
    io:format("Got an OPTIONS request\n", []),
    
    ReqHeaders = Req:get(headers),
    ReqHeadersList = mochiweb_headers:to_list(ReqHeaders),
    lists:map(fun ({Key, Value}) ->
		      io:format("Header ~s : ~s\n", [Key, Value])
	      end, ReqHeadersList),
    
    Origin = Req:get_header_value("Origin"),
    Headers = [
	       {"Access-Control-Allow-Origin", Origin},
	       {"Access-Control-Allow-Max-Age", 0}, % Indicates that another "preflight check" is not necessary for the specified number of seconds
	       {"Access-Control-Allow-Headers", "Content-Type"},
	       {"Access-Control-Allow-Methods", "POST"}
	      ],
    Req:respond({200, Headers, []}).

new_connection(Req, Settings, ConnectionId, CometMethod) ->
    Headers = [{"ConnectionId", ConnectionId}],
    ContentType = Settings#kanaloa_settings.http_content_type,
    Resp = Req:ok({ContentType, Headers, chunked}),
    kanaloa_connection:new(Resp, self(), Settings, ConnectionId, CometMethod).

%% @doc Opens a connection and waits for it to die, reporting the cause of death.
open_connection(Connection, Owner) ->
    Result = (catch Connection:open(Owner)),
    Connection:log("Connection closed with reason ~w", [Result]),
    ok.

parse_body(BodyText, Settings) when is_record(Settings, kanaloa_settings) ->
    BodyJson = case BodyText of
	       <<"">> ->
		   [];
	       _ when is_binary(BodyText) ->
		   mochijson2:decode(BodyText)
	   end,
    true = is_list(BodyJson),
    
    Body = case Settings#kanaloa_settings.parse_jsonrpc of
	       true ->
		   lists:map(fun (Item) ->
				     kanaloa_rpc:parse(Item)
			     end, BodyJson);
	       _ ->
		   BodyJson
	   end,
    true = is_list(BodyJson),
    
    {ok, Body}.

%% @doc Parses the comet method out of the query string, throwing if anything else is encountered.
parse_query_string([]) ->
    none;
parse_query_string([{"t", "longpoll"}]) ->
    longpoll;
parse_query_string([{"t", "stream"}]) ->
    stream.

%% @doc Parses the request. Throws if something invalid is encountered.
parse_request(Req) ->
    case Req:get(method) of
	'POST' ->
	    Path = get_path(Req),
	    io:format("POST request to ~s\n", [Path]),
	    "/" = Path,
	    
	    CometMethod = get_comet_method(Req),
	    
	    ConnectionId = get_connection_id(Req),
	    
	    % TODO: Add an option for how much of the body to receive.
	    Body = Req:recv_body(), % Receives up to 1MB.
	    
	    {ok, CometMethod, ConnectionId, Body};
	'OPTIONS' ->
	    {ok, options}
    end.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
