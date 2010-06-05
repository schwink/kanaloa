%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc TEMPLATE.

-module(kanaloa_mochiweb).
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
%% @doc Start the kanaloa_mochiweb server.
start() ->
    ensure_started(crypto),
    Result = kanaloa_mochiweb_sup:start_link([], []),
    io:format("start result: ~w\n", [Result]),

    receive
	exit -> % Apparently this function, passed to erl with "-s", isn't supposed to return.
	    ok
    end.

%% @spec stop() -> ok
%% @doc Stop the kanaloa_mochiweb server.
stop() ->
    ok.
