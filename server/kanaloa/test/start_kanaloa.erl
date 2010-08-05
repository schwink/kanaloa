%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc Test kanaloa application.

-module(start_kanaloa).
-author('Stephen Schwink <kanaloa@schwink.net>').
-export([start/0]).

-define(CONNECTION_ORPHAN_TIMEOUT, 60000).

%% @spec start() -> ok
%% @doc Start the kanaloa mochiweb server.
start() ->
    MochiwebOptions = [{port, 8001}],
    
    HandlerFun = fun (Connection) ->
			 handle_connection(Connection)
		 end,
    KanaloaOptions = [{handler, HandlerFun}],
    
    {ok, _Pid} = kanaloa:start_link(MochiwebOptions, KanaloaOptions),
    
    receive
	exit -> % Apparently this function, passed to erl with "-s", isn't supposed to return.
	    ok
    end.

%% @doc Control comes here when we want to receive the next message for a presumably healty connection.
handle_connection(Connection) ->
    receive
	% First, check for high-priority messages that indicate a change of connection state.
	% We need to handle these first to prevent sending data to a dead response.
	
	{connection, NewConnection} ->
	    NewConnection:log("Handler replacing live connection.", []),
	    new_connection(Connection, NewConnection);
	
	{'EXIT', _Owner, _Reason} ->
	    handle_orphaned(Connection)
    
    after 0 ->
	  % Next, process regular application messages.
	  receive_normal(Connection)
    end.

%% @doc Control comes here while we wait for the client to reconnect.
handle_orphaned(OldConnection) ->
    OldConnection:log("Handler orphaned.", []),
    
    receive
	{connection, NewConnection} ->
	    NewConnection:log("Handler is un-orphaned.", []),
	    new_connection(OldConnection, NewConnection)
    
    after ?CONNECTION_ORPHAN_TIMEOUT ->
	    OldConnection:log("Didn't get a new connection in time, the handler dies.", []),
	    orphaned
    end.

new_connection(OldConnection, NewConnection) ->
    catch OldConnection:close(),
    handle_connection(NewConnection).

receive_normal(Connection) ->
    receive
	% Continue to listen for the high-priority events, even if they weren't in the queue initially.
	{connection, NewConnection} ->
	    NewConnection:log("Handler replacing live connection.", []),
	    new_connection(Connection, NewConnection);
	{'EXIT', _Owner, _Reason} ->
	    handle_orphaned(Connection);

	% React to messages from Kanaloa and from your application.
	{chunk, Data} ->
	    Connection:log("Handler got a chunk! '~w'", [Data]),
	    
	    Reply = <<"Got your message, which is re-encoded as:">>,
	    BodyText = mochijson2:encode(Data),
	    BodyBinary = iolist_to_binary(BodyText),
	    Connection:send(<<Reply/binary, BodyBinary/binary>>),
	    
	    handle_connection(Connection)
    
    end.
