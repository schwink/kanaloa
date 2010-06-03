%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc TEMPLATE.

-module(kanaloa_mochiweb).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the kanaloa_mochiweb server.
start() ->
    kanaloa_mochiweb_deps:ensure(),
    ensure_started(crypto),
    application:start(kanaloa_mochiweb).

%% @spec stop() -> ok
%% @doc Stop the kanaloa_mochiweb server.
stop() ->
    Res = application:stop(kanaloa_mochiweb),
    application:stop(crypto),
    Res.
