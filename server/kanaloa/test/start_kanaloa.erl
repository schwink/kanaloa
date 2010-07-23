%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc TEMPLATE.

-module(start_kanaloa).
-author('Stephen Schwink <kanaloa@schwink.net>').
-export([start/0]).

handle_connection(orphaned) ->
    receive
	{connection, NewConnection} ->
	    NewConnection:log("Handler is un-orphaned.", []),
	    handle_connection(NewConnection)
    after 60000 ->
	    io:format("Didn't get a new connection in time, the handler dies.\n", []),
	    ok
    end;
handle_connection(Connection) ->
    receive
	{connection, NewConnection} ->
	    NewConnection:log("Handler replacing live connection.", []),
	    Connection:close(),
	    handle_connection(NewConnection);
	{'EXIT', _Owner, _Reason} ->
	    Connection:log("Handler orphaned.", []),
	    handle_connection(orphaned);
	{chunk, Data} ->
	    Connection:log("Handler got a chunk! '~w'", [Data]),
	    Reply = <<"Got your message, which is re-encoded as:">>,
	    BodyText = mochijson2:encode(Data),
	    BodyBinary = iolist_to_binary(BodyText),
	    Connection:send(<<Reply/binary, BodyBinary/binary>>),
	    handle_connection(Connection)
    end.

%% @spec start() -> ok
%% @doc Start the kanaloa mochiweb server.
start() ->
    MochiwebOptions = [],
    
    HandlerFun = fun (Connection) ->
			 handle_connection(Connection)
		 end,
    KanaloaOptions = [{handler, HandlerFun}],
    
    {ok, _Pid} = kanaloa:start_link(MochiwebOptions, KanaloaOptions),
    
    receive
	exit -> % Apparently this function, passed to erl with "-s", isn't supposed to return.
	    ok
    end.
