%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc TEMPLATE.

-module(kanaloa).
-author('Stephen Schwink <kanaloa@schwink.net>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
        {error, {already_started, App}} ->
	    ok
    end.

handle_connection(Connection) ->
    receive
	{chunk, _Data} ->
	    io:format("Received a chunk!\n"),
	    Connection:send(<<"Got your message!">>),
	    handle_connection(Connection)
    end.

%% @spec start() -> ok
%% @doc Start the kanaloa mochiweb server.
start() ->
    HandlerFun = fun (Connection) ->
			 handle_connection(Connection)
		 end,
    
    ensure_started(crypto),
    Result = kanaloa_sup:start_link([], [{handler, HandlerFun}]),
    io:format("start result: ~w\n", [Result]),

    receive
	exit -> % Apparently this function, passed to erl with "-s", isn't supposed to return.
	    ok
    end.

%% @spec stop() -> ok
%% @doc Stop the kanaloa mochiweb server.
stop() ->
    ok.
