%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc Callbacks for the kanaloa_test_app application.

-module(kanaloa_test_app_app).
-author('Stephen Schwink <kanaloa@schwink.net>').

-behaviour(application).
-export([start/2, stop/1]).

-define(CONNECTION_ORPHAN_TIMEOUT, 60000).

%% @spec start(_Type, _StartArgs) -> {ok, pid()}
%% @doc application start callback for kanaloa_test_app.
start(_StartType, _StartArgs) ->
    io:format("kanaloa_test_app_app:start/2 called\n", []),
    {ok, Port} = application:get_env(kanaloa_test_app, port),
    io:format("starting on port ~w\n", [Port]),
    MochiwebOptions = [{port, Port}],
    
    HandlerFun = fun (Connection) ->
			 handle_connection(Connection)
		 end,
    KanaloaOptions = [{handler, HandlerFun}],
    
    kanaloa:start_link(MochiwebOptions, KanaloaOptions).    

%% @spec stop(_State) -> ok
%% @doc application stop callback for kanaloa_test_app.
stop(_State) ->
    ok.

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
	
	{chunk, <<"flood32">>} ->
	    % This will cause the owner to send a bunch of messages.
	    % Used by the integration tests.
	    Connection:log("Client says to send a flood of 32 messages.", []),
	    flood(Connection, 32, 0);
	
	{chunk, Data} ->
	    Connection:log("Owner got a chunk from the client: '~s'", [iolist_to_binary([Data])]),
	    
	    Now = kanaloa_utils:now_utc_ms(),
	    JsonObj = {struct, [
				{time, Now},
				{message, Data}
			       ]},
	    Connection:send(JsonObj),
	    
	    handle_connection(Connection)
    
    end.

flood(_Connection, N, SN) when N == SN ->
    done;
flood(Connection, N, SN) when is_integer(N) andalso is_integer(SN) ->
    Now = kanaloa_utils:now_utc_ms(),
    Filler = <<"">>,
    JsonObj = {struct, [
			{time, Now},
			{count, SN},
			{filler, Filler}
		       ]},
    
    Connection:send(JsonObj),
    
    flood(Connection, N, SN + 1).
