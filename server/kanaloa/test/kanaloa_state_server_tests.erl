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

get_owner_none_test() ->
    {ok, _Server} = start_link(),
    
    Result = kanaloa_state_server:get_owner(<<"asdf">>),
    ?assertEqual(no_id, Result),
    
    stop().

get_owner_existing_test() ->
    {ok, _Server} = start_link(),
    
    Id = kanaloa_state_server:new_guid(),
    Owner = get_sample_pid(),
    ok = kanaloa_state_server:new_state(Id, Owner),
    ReceivedOwner = kanaloa_state_server:get_owner(Id),
    
    ?assertEqual(Owner, ReceivedOwner),
    
    stop().

get_owner_multiple_existing_test() ->
    {ok, _Server} = start_link(),
    
    Id1 = kanaloa_state_server:new_guid(),
    Owner1 = get_sample_pid(),
    ok = kanaloa_state_server:new_state(Id1, Owner1),
    
    Id2 = kanaloa_state_server:new_guid(),
    Owner2 = get_sample_pid(),
    ok = kanaloa_state_server:new_state(Id2, Owner2),
    
    Id3 = kanaloa_state_server:new_guid(),
    Owner3 = get_sample_pid(),
    ok = kanaloa_state_server:new_state(Id3, Owner3),
    
    ReceivedOwner2 = kanaloa_state_server:get_owner(Id2),
    ReceivedOwner1 = kanaloa_state_server:get_owner(Id1),
    ReceivedOwner3 = kanaloa_state_server:get_owner(Id3),
    
    ?assertEqual(Owner1, ReceivedOwner1),
    ?assertEqual(Owner2, ReceivedOwner2),
    ?assertEqual(Owner3, ReceivedOwner3),
    
    stop().

remove_on_death_test() ->
    {ok, _Server} = start_link(),
    
    Id = kanaloa_state_server:new_guid(),
    Pid = get_sample_pid(),
    ok = kanaloa_state_server:new_state(Id, Pid),
    RPid = kanaloa_state_server:get_owner(Id),
    ?assertEqual(Pid, RPid),
    
    % Kill the process by sending it a message
    process_flag(trap_exit, true),
    link(Pid),
    exit(Pid, kill),
    receive
	{'EXIT', Pid, _} -> ok;
	M -> io:format("received ~w\n", [M])
    end,
    
    Result = kanaloa_state_server:get_owner(Id),
    ?assertEqual(no_id, Result),
    
    stop().

remove_single_on_death_test() ->
    {ok, _Server} = start_link(),
    
    Id1 = kanaloa_state_server:new_guid(),
    Owner1 = get_sample_pid(),
    ok = kanaloa_state_server:new_state(Id1, Owner1),
    ROwner1 = kanaloa_state_server:get_owner(Id1),
    ?assertEqual(Owner1, ROwner1),
    
    Id2 = kanaloa_state_server:new_guid(),
    Owner2 = get_sample_pid(),
    ok = kanaloa_state_server:new_state(Id2, Owner2),
    ROwner2 = kanaloa_state_server:get_owner(Id2),
    ?assertEqual(Owner2, ROwner2),
    
    Id3 = kanaloa_state_server:new_guid(),
    Owner3 = get_sample_pid(),
    ok = kanaloa_state_server:new_state(Id3, Owner3),
    ROwner3 = kanaloa_state_server:get_owner(Id3),
    ?assertEqual(Owner3, ROwner3),
    
    % Kill the process by sending it a message
    process_flag(trap_exit, true),
    link(Owner2),
    exit(Owner2, kill),
    receive
	{'EXIT', Owner2, _} -> ok;
	M -> io:format("received ~w\n", [M])
    end,
    
    % Verify that only process 2 was removed.
    
    ROwner1_2 = kanaloa_state_server:get_owner(Id1),
    ?assertEqual(Owner1, ROwner1_2),
    
    Result = kanaloa_state_server:get_owner(Id2),
    ?assertEqual(no_id, Result),
    
    ROwner3_2 = kanaloa_state_server:get_owner(Id1),
    ?assertEqual(Owner1, ROwner3_2),

    stop().

pop_pending_new_test() ->
    {ok, _Server} = start_link(),
    
    Id1 = kanaloa_state_server:new_guid(),
    Owner1 = get_sample_pid(),
    ok = kanaloa_state_server:new_state(Id1, Owner1),
    
    Pop1 = kanaloa_state_server:pop_pending(Id1),
    ?assertEqual([], Pop1),
    
    stop().

add_pending_test() ->
    {ok, _Server} = start_link(),
    
    Id1 = kanaloa_state_server:new_guid(),
    Owner1 = get_sample_pid(),
    ok = kanaloa_state_server:new_state(Id1, Owner1),
    
    Pending1 = [<<"foo">>, <<"bar">>],
    Add1 = kanaloa_state_server:add_pending(Id1, Pending1),
    ?assertEqual(ok, Add1),
    
    Pop1 = kanaloa_state_server:pop_pending(Id1),
    ?assertEqual(Pending1, Pop1),

    Pop2 = kanaloa_state_server:pop_pending(Id1),
    ?assertEqual([], Pop2),
    
    stop().

add_pending_additional_test() ->
    {ok, _Server} = start_link(),
    
    Id1 = kanaloa_state_server:new_guid(),
    Owner1 = get_sample_pid(),
    ok = kanaloa_state_server:new_state(Id1, Owner1),
    
    Pending1 = [<<"foo">>, <<"bar">>],
    Add1 = kanaloa_state_server:add_pending(Id1, Pending1),
    ?assertEqual(ok, Add1),
    
    Pending2 = [<<"baz">>],
    Add2 = kanaloa_state_server:add_pending(Id1, Pending2),
    ?assertEqual(ok, Add2),
    
    Pop1 = kanaloa_state_server:pop_pending(Id1),
    PendingR = [<<"foo">>, <<"bar">>, <<"baz">>],
    ?assertEqual(PendingR, Pop1),
    
    Pop2 = kanaloa_state_server:pop_pending(Id1),
    ?assertEqual([], Pop2),
    
    stop().

%% @spec get_sample_pid() -> pid()
%% @doc Returns a pid that won't exit for a while. Useful for unimportant fake clients.
get_sample_pid() ->
    spawn(fun () ->
		  receive
		      _ -> ok
		  end
	  end).
