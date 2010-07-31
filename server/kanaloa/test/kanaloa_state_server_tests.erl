%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc Unit tests for kanaloa_state_server.

-module(kanaloa_state_server_tests).
-author('Stephen Schwink <kanaloa@schwink.net>').

-include_lib("../include/kanaloa.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(kanaloa_state_server, [start_link/0, stop/0]).
-import(kanaloa_state_server, [new_guid/0]).

new_guid_test() ->
    A = new_guid(),
    ?assert(is_binary(A)),
    B = new_guid(),
    ?assert(A /= B),
    ok.

registered_local_test() ->
    {ok, Server} = start_link(),
    
    true = is_pid(Server),
    true = is_pid(whereis('kanaloa_state_server')),
    
    stop().

find_empty_test() ->
    {ok, _Server} = start_link(),
    
    Result = kanaloa_state_server:get_state(<<"asdf">>),
    ?assertEqual(no_id, Result),
    
    stop().

find_existing_test() ->
    {ok, _Server} = start_link(),
    
    Entry = #kanaloa_connection_state { id = kanaloa_state_server:new_guid(),
					owner = get_sample_pid() },
    ok = kanaloa_state_server:set_state(Entry),
    {ok, ReceivedEntry} = kanaloa_state_server:get_state(Entry#kanaloa_connection_state.id),
    
    ?assertEqual(Entry, ReceivedEntry),
    
    stop().

find_multiple_existing_test() ->
    {ok, _Server} = start_link(),
    
    Entry1 = #kanaloa_connection_state { id = kanaloa_state_server:new_guid(),
					owner = get_sample_pid() },
    ok = kanaloa_state_server:set_state(Entry1),

    Entry2 = #kanaloa_connection_state { id = kanaloa_state_server:new_guid(),
					owner = get_sample_pid() },
    ok = kanaloa_state_server:set_state(Entry2),

    Entry3 = #kanaloa_connection_state { id = kanaloa_state_server:new_guid(),
					owner = get_sample_pid() },
    ok = kanaloa_state_server:set_state(Entry3),

    {ok, ReceivedEntry1} = kanaloa_state_server:get_state(Entry1#kanaloa_connection_state.id),
    {ok, ReceivedEntry2} = kanaloa_state_server:get_state(Entry2#kanaloa_connection_state.id),
    {ok, ReceivedEntry3} = kanaloa_state_server:get_state(Entry3#kanaloa_connection_state.id),
    
    ?assertEqual(Entry1, ReceivedEntry1),
    ?assertEqual(Entry2, ReceivedEntry2),
    ?assertEqual(Entry3, ReceivedEntry3),
    
    stop().

remove_on_death_test() ->
    {ok, _Server} = start_link(),
    
    Id = kanaloa_state_server:new_guid(),
    Pid = get_sample_pid(),
    Entry = #kanaloa_connection_state{ id = Id, owner = Pid },
    ok = kanaloa_state_server:set_state(Entry),
    {ok, Entry} = kanaloa_state_server:get_state(Id),
    ?assertEqual(Pid, Entry#kanaloa_connection_state.owner),
    
    % Kill the process by sending it a message
    process_flag(trap_exit, true),
    link(Pid),
    exit(Pid, kill),
    receive
	{'EXIT', Pid, _} -> ok;
	M -> io:format("received ~w\n", [M])
    end,
    
    Result = kanaloa_state_server:get_state(Id),
    ?assertEqual(no_id, Result),
    
    stop().

remove_single_on_death_test() ->
    {ok, _Server} = start_link(),
    
    Id1 = kanaloa_state_server:new_guid(),
    Pid1 = get_sample_pid(),
    Entry1 = #kanaloa_connection_state{ id = Id1, owner = Pid1 },
    ok = kanaloa_state_server:set_state(Entry1),
    {ok, ReceivedEntry1} = kanaloa_state_server:get_state(Id1),
    ?assertEqual(Pid1, ReceivedEntry1#kanaloa_connection_state.owner),
    
    Id2 = kanaloa_state_server:new_guid(),
    Pid2 = get_sample_pid(),
    Entry2 = #kanaloa_connection_state{ id = Id2, owner = Pid2 },
    ok = kanaloa_state_server:set_state(Entry2),
    {ok, ReceivedEntry2} = kanaloa_state_server:get_state(Id2),
    ?assertEqual(Pid2, ReceivedEntry2#kanaloa_connection_state.owner),

    Id3 = kanaloa_state_server:new_guid(),
    Pid3 = get_sample_pid(),
    Entry3 = #kanaloa_connection_state{ id = Id3, owner = Pid3 },
    ok = kanaloa_state_server:set_state(Entry3),
    {ok, ReceivedEntry3} = kanaloa_state_server:get_state(Id3),
    ?assertEqual(Pid3, ReceivedEntry3#kanaloa_connection_state.owner),
    
    % Kill the process by sending it a message
    process_flag(trap_exit, true),
    link(Pid2),
    exit(Pid2, kill),
    receive
	{'EXIT', Pid2, _} -> ok;
	M -> io:format("received ~w\n", [M])
    end,
    
    % Verify that only process 2 was removed.
    
    {ok, ReceivedEntry1_2} = kanaloa_state_server:get_state(Id1),
    ?assertEqual(Pid1, ReceivedEntry1_2#kanaloa_connection_state.owner),

    Result = kanaloa_state_server:get_state(Id2),
    ?assertEqual(no_id, Result),

    {ok, ReceivedEntry3_2} = kanaloa_state_server:get_state(Id3),
    ?assertEqual(Pid3, ReceivedEntry3_2#kanaloa_connection_state.owner),
    
    stop().

%% @spec get_sample_pid() -> pid()
%% @doc Returns a pid that won't exit for a while. Useful for unimportant fake clients.
get_sample_pid() ->
    spawn(fun () ->
		  receive
		      _ -> ok
		  end
	  end).
