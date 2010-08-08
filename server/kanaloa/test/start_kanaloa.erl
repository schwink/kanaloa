%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc Test kanaloa application. This is used by the client tests.

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
	    NewConnection:log("Owner replacing live connection.", []),
	    new_connection(Connection, NewConnection);
	
	{'EXIT', _Owner, _Reason} ->
	    handle_orphaned(Connection)
    
    after 0 ->
	  % Next, process regular application messages.
	  receive_normal(Connection)
    end.

%% @doc Control comes here while we wait for the client to reconnect.
handle_orphaned(OldConnection) ->
    OldConnection:log("Owner orphaned.", []),
    
    receive
	{connection, NewConnection} ->
	    NewConnection:log("Owner is un-orphaned.", []),
	    new_connection(OldConnection, NewConnection)
    
    after ?CONNECTION_ORPHAN_TIMEOUT ->
	    OldConnection:log("Didn't get a new connection in time, the owner dies.", []),
	    orphaned
    end.

new_connection(OldConnection, NewConnection) ->
    catch OldConnection:close(),
    handle_connection(NewConnection).

receive_normal(Connection) ->
    receive
	% Continue to listen for the high-priority events, even if they weren't in the queue initially.
	{connection, NewConnection} ->
	    NewConnection:log("Owner replacing live connection.", []),
	    new_connection(Connection, NewConnection);
	{'EXIT', _Owner, _Reason} ->
	    handle_orphaned(Connection);

	% React to messages from Kanaloa and from your application:
	
	{chunk, <<"kill_owner">>} ->
	    % This will cause the owner process to exit and the connection to die.
	    % Used by the integration tests.
	    Connection:log("Client instructs the owner to exit.", []),
	    kill_owner;
	
	{chunk, Data} ->
	    Connection:log("Owner got a chunk from the client: '~w'", [Data]),
	    
	    Now = kanaloa_utils:now_utc_ms(),
	    JsonObj = {struct, [
				{time, Now},
				{message, Data}
			       ]},
	    Connection:send(JsonObj),
	    
	    handle_connection(Connection)
    
    end.
