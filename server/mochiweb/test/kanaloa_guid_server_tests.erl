%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc Unit tests for kanaloa_guid_server.

-module(kanaloa_guid_server_tests).
-author('Stephen Schwink <kanaloa@schwink.net>').

-include_lib("eunit/include/eunit.hrl").

-import(kanaloa_guid_server, [start_link/0, stop/0]).
-import(kanaloa_guid_server, [new_guid/0]).

new_guid_test() ->
    A = new_guid(),
    ?assert(is_binary(A)),
    B = new_guid(),
    ?assert(A /= B),
    ok.

registered_local_test() ->
    {ok, Server} = start_link(),
    
    true = is_pid(Server),
    true = is_pid(whereis('kanaloa_guid_server')),
    
    stop().

find_empty_test() ->
    {ok, _Server} = start_link(),
    
    Result = kanaloa_guid_server:find(<<"asdf">>),
    ?assertEqual(no_id, Result),
    
    stop().

find_existing_test() ->
    {ok, _Server} = start_link(),
    
    Id = kanaloa_guid_server:new_guid(),
    Pid = get_sample_pid(),
    ok = kanaloa_guid_server:register_new(Pid, Id),
    {ok, ReceivedPid} = kanaloa_guid_server:find(Id),
    
    ?assertEqual(Pid, ReceivedPid),

    stop().

find_multiple_existing_test() ->
    {ok, _Server} = start_link(),
    
    Id1 = kanaloa_guid_server:new_guid(),
    Pid1 = get_sample_pid(),
    ok = kanaloa_guid_server:register_new(Pid1, Id1),

    Id2 = kanaloa_guid_server:new_guid(),
    Pid2 = get_sample_pid(),
    ok = kanaloa_guid_server:register_new(Pid2, Id2),

    Id3 = kanaloa_guid_server:new_guid(),
    Pid3 = get_sample_pid(),
    ok = kanaloa_guid_server:register_new(Pid3, Id3),

    {ok, ReceivedPid1} = kanaloa_guid_server:find(Id1),
    {ok, ReceivedPid2} = kanaloa_guid_server:find(Id2),
    {ok, ReceivedPid3} = kanaloa_guid_server:find(Id3),
    
    ?assertEqual(Pid1, ReceivedPid1),
    ?assertEqual(Pid2, ReceivedPid2),
    ?assertEqual(Pid3, ReceivedPid3),

    stop().

remove_on_death_test() ->
    {ok, _Server} = start_link(),
    
    Id = kanaloa_guid_server:new_guid(),
    Pid = get_sample_pid(),
    ok = kanaloa_guid_server:register_new(Pid, Id),
    {ok, ReceivedPid} = kanaloa_guid_server:find(Id),
    ?assertEqual(Pid, ReceivedPid),
    
    % Kill the process by sending it a message
    process_flag(trap_exit, true),
    link(Pid),
    exit(Pid, kill),
    receive
	{'EXIT', Pid, _} -> ok;
	M -> io:format("received ~w\n", [M])
    end,
    
    Result = kanaloa_guid_server:find(Id),
    ?assertEqual(no_id, Result),
    
    stop().

remove_single_on_death_test() ->
    {ok, _Server} = start_link(),
    
    Id1 = kanaloa_guid_server:new_guid(),
    Pid1 = get_sample_pid(),
    ok = kanaloa_guid_server:register_new(Pid1, Id1),
    {ok, ReceivedPid1} = kanaloa_guid_server:find(Id1),
    ?assertEqual(Pid1, ReceivedPid1),

    Id2 = kanaloa_guid_server:new_guid(),
    Pid2 = get_sample_pid(),
    ok = kanaloa_guid_server:register_new(Pid2, Id2),
    {ok, ReceivedPid2} = kanaloa_guid_server:find(Id2),
    ?assertEqual(Pid2, ReceivedPid2),

    Id3 = kanaloa_guid_server:new_guid(),
    Pid3 = get_sample_pid(),
    ok = kanaloa_guid_server:register_new(Pid3, Id3),
    {ok, ReceivedPid3} = kanaloa_guid_server:find(Id3),
    ?assertEqual(Pid3, ReceivedPid3),
    
    % Kill the process by sending it a message
    process_flag(trap_exit, true),
    link(Pid2),
    exit(Pid2, kill),
    receive
	{'EXIT', Pid2, _} -> ok;
	M -> io:format("received ~w\n", [M])
    end,
    
    % Verify that only process 2 was removed.
    
    {ok, ReceivedPid1_2} = kanaloa_guid_server:find(Id1),
    ?assertEqual(Pid1, ReceivedPid1_2),

    Result = kanaloa_guid_server:find(Id2),
    ?assertEqual(no_id, Result),

    {ok, ReceivedPid3_2} = kanaloa_guid_server:find(Id3),
    ?assertEqual(Pid3, ReceivedPid3_2),
    
    stop().

%% @spec get_sample_pid() -> pid()
%% @doc Returns a pid that won't exit for a while. Useful for unimportant fake clients.
get_sample_pid() ->
    spawn(fun () ->
		  receive
		      _ -> ok
		  end
	  end).
