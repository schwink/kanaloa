%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc Public functions to start and stop kanaloa.

-module(kanaloa).
-author('Stephen Schwink <kanaloa@schwink.net>').
-export([start_link/2]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
        {error, {already_started, App}} ->
	    ok
    end.

%% @spec start_link(MochiOptions, KanaloaOptions) -> {ok, Pid::pid()}
%% @doc Start a local instance of the kanaloa mochiweb server.
start_link(MochiOptions, KanaloaOptions) when is_list(MochiOptions) andalso is_list(KanaloaOptions) ->
    ensure_started(crypto),
    
    Result = kanaloa_sup:start_link(MochiOptions, KanaloaOptions),
    io:format("kanaloa start result: ~w\n", [Result]),
    Result.
