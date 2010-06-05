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
    
    Pid = get_sample_pid(),
    {ok, Id} = kanaloa_guid_server:register(Pid),
    {ok, ReceivedPid} = kanaloa_guid_server:find(Id),
    
    ?assertEqual(Pid, ReceivedPid),

    stop().

find_multiple_existing_test() ->
    {ok, _Server} = start_link(),
    
    Pid1 = get_sample_pid(),
    {ok, Id1} = kanaloa_guid_server:register(Pid1),

    Pid2 = get_sample_pid(),
    {ok, Id2} = kanaloa_guid_server:register(Pid2),

    Pid3 = get_sample_pid(),
    {ok, Id3} = kanaloa_guid_server:register(Pid3),

    {ok, ReceivedPid1} = kanaloa_guid_server:find(Id1),
    {ok, ReceivedPid2} = kanaloa_guid_server:find(Id2),
    {ok, ReceivedPid3} = kanaloa_guid_server:find(Id3),
    
    ?assertEqual(Pid1, ReceivedPid1),
    ?assertEqual(Pid2, ReceivedPid2),
    ?assertEqual(Pid3, ReceivedPid3),

    stop().

remove_on_death_test() ->
    {ok, _Server} = start_link(),
    
    Pid = get_sample_pid(),
    {ok, Id} = kanaloa_guid_server:register(Pid),
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

%% @spec get_sample_pid() -> pid()
%% @doc Returns a pid that won't exit for a while. Useful for unimportant fake clients.
get_sample_pid() ->
    spawn(fun () ->
		  receive
		      _ -> ok
		  end
	  end).
