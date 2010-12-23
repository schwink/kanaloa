%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc Starts the test kanaloa application. This is used by the client tests.

-module(test_app).
-author('Stephen Schwink <kanaloa@schwink.net>').

-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the server.
start() ->
    io:format("test_app:start/0 called\n", []),
    ensure_started(crypto),
    application:start(test_app).

%% @spec stop() -> ok
%% @doc Stop the server.
stop() ->
    Res = application:stop(test_app),
    application:stop(crypto),
    Res.
