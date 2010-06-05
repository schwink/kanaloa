%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc Supervisor for the kanaloa_mochiweb application.

-module(kanaloa_mochiweb_sup).
-author('Stephen Schwink <kanaloa@schwink.net>').

-behaviour(supervisor).

%% External exports
-export([start_link/2, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link(MochiConfig, KanaConfig) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {MochiConfig, KanaConfig}).

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
init({MochiConfig, KanaConfig}) ->
    io:format("kanaloa_mochiweb_sup:init/2\n"),
    Web = {kanaloa_mochiweb_web,
           {kanaloa_mochiweb_web, start_link, [MochiConfig, KanaConfig]},
           permanent, 5000, worker, dynamic},
    
    Processes = [Web],
    {ok, {{one_for_one, 100, 100}, Processes}}.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
