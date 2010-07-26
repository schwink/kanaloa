%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc Supervisor for the kanaloa gen_servers.

-module(kanaloa_sup).
-author('Stephen Schwink <kanaloa@schwink.net>').

-behaviour(supervisor).

%% External exports
-export([start_link/2, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link(MochiConfig, Settings) -> ServerRet
%% @doc API for starting the supervisor.
start_link(MochiConfig, Settings) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {MochiConfig, Settings}).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init({MochiConfig, Settings}) ->
    Guid = {kanaloa_guid_server,
	    {kanaloa_guid_server, start_link, []},
	    permanent, 5000, worker, dynamic},
    Web = {kanaloa_web,
           {kanaloa_web, start_link, [MochiConfig, Settings]},
           permanent, 5000, worker, dynamic},
    
    Processes = [Guid, Web],
    {ok, {{one_for_all, 100, 100}, Processes}}.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
