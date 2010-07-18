%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc Web server for kanaloa.

-module(kanaloa_web).
-author('Stephen Schwink <kanaloa@schwink.net>').

-export([start_link/2, stop/0, loop/4]).

%% External API

start_link(MochiOptions, KanaOptions) ->
    MochiOptions2 = get_mochiweb_options(MochiOptions),
    {ok, HttpContentType, Parser, Handler} = parse_kanaloa_options(KanaOptions),
    
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, Parser, Handler, HttpContentType)
           end,
    io:format("kanaloa_web:start(~w, ~w)\n", [MochiOptions2, KanaOptions]),
    mochiweb_http:start([{loop, Loop} | MochiOptions2]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, Parser, Handler, ContentType) ->
    io:format("Got a request!\n", []),
    
    case catch parse_request(Req) of
	{ok, options} ->
	    handle_options_request(Req);
	{ok, CometMethod, ConnectionId, Body} ->
	    case catch Parser(Body) of
		{ok, Data} ->
		    handle_connection_request(Req, ContentType, Handler, CometMethod, ConnectionId, Data);
		Error ->
		    io:format("Body parse error: ~w\n", [Error]),
		    Req:respond({400, [], []}) % Bad request
	    end;
	ParseError ->
	    io:format("Request parse error: ~w\n", [ParseError]),
	    Req:respond({400, [], []}) % Bad request
    end.

%% Internal API

dispatch_chunk(Owner, Data) ->
    io:format("dispatch_chunk: sending data '~s' to owner ~w\n", [Data, Owner]),
    Owner ! {chunk, Data}.

%% @spec get_comet_method(Request::mochiweb_request()) -> 'longpoll' | 'stream'
get_comet_method(Req) ->
    QS = Req:parse_qs(),
    parse_query_string(QS, none).

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
	    case kanaloa_guid_server:find(ConnectionId) of
		{ok, Owner} when is_pid(Owner) ->
		    {ok, Owner};
		_ ->
		    expired
	    end
    end.

%% @doc We need to supervise the options passed to mochiweb.
get_mochiweb_options(Options) ->
    Options1 = proplists:delete(docroot, Options), % Must unset the docroot property.
    Options2 = proplists:delete(loop, Options1), % Ignore the loop property.
    
    Options3 = set_default_option(max, Options2, 1048576), % Set a high default for max number of connections.
    Options4 = set_default_option(ip, Options3, "0.0.0.0"),
    Options5 = set_default_option(port, Options4, 8000),
    
    Options6 = replace_option(name, Options5, ?MODULE),
    Options6.

%% @spec get_path(Request::mochiweb_request()) -> string()
%% @doc Gets the path that the request was made to.
get_path(Req) ->
    FullPath = Req:get(path),

    % Remove the "/thruster" prefix if it is present
    Path = case string:substr(FullPath, 2, 8) of
	       "thruster" ->
		   string:substr(FullPath, 10);
	       _ ->
		   FullPath
	   end,
    Path.

handle_connection_request(Req, ContentType, Handler, CometMethod, ConnectionId, Data) ->
    case get_connection_owner(ConnectionId) of
	expired ->
	    Req:respond({410, [], []}), % Gone
	    exit(bad_connection_owner);
	{ok, ExistingOwner} ->
	    io:format("Sending message to existing owner ~w\n", [ExistingOwner]),
	    dispatch_chunk(ExistingOwner, Data),
	    Req:ok({ContentType, [], []});
	new ->
	    NewConnectionId = kanaloa_guid_server:new_guid(),
	    
	    Headers = [{"ConnectionId", NewConnectionId}],
	    Resp = Req:ok({ContentType, Headers, chunked}),
	    Connection = kanaloa_connection:new(Resp, self(), CometMethod),
	    
	    NewOwner = spawn(fun () ->
				     io:format("Owner process spawned\n", []),
				     Handler(Connection)
			     end),
	    ok = kanaloa_guid_server:register_new(NewOwner, NewConnectionId),
	    
	    dispatch_chunk(NewOwner, Data),
	    
	    Result = (catch Connection:open(NewOwner)),
	    io:format("Connection closed with reason ~w\n", [Result])
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

%% @doc Parse out the required and optional options.
parse_kanaloa_options(Options) ->
    ContentType = case proplists:get_value(http_content_type, Options, <<"application/json">>) of
		      C when is_binary(C) ->
			  C
		  end,
    
    ParseFun = case proplists:get_value(parse, Options, undefined) of
		   undefined ->
		       fun (Value) ->
			       % BodyJson = mochijson2:decode(Value),
			       % true = is_list(BodyJson),
			       {ok, Value}
		       end;
		   P when is_function(P) ->
		       P
	       end,
    
    HandlerFun = case proplists:get_value(handler, Options, undefined) of
		     undefined -> % TODO: Remove this debugging default
			 fun (_Connection) ->
				 io:format("New connection!\n"),
				 receive
				     {chunk, _Data} ->
					 io:format("Received a first chunk!\n")
				 end
			 end;
		     H when is_function(H) ->
			 H
		 end,
    
    {ok, ContentType, ParseFun, HandlerFun}.

%% @doc Parses the comet method out of the query string, throwing if anything else is encountered.
parse_query_string([{"t", "longpoll"}], none) ->
    longpoll;
parse_query_string([{"t", "stream"}], none) ->
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

%% @doc Replaces the specified entry in a proplist.
replace_option(Key, Options, Value) ->
    [{Key, Value} | proplists:delete(Key, Options)].

%% @doc If the specified option is not present in the proplist, set it to the specified value.
set_default_option(Key, Options, Default) ->
    case proplists:get_value(Key, Options, undefined) of
	undefined ->
	    replace_option(Key, Options, Default);
	_ ->
	    Options
    end.

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
