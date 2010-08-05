%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc Public functions to start kanaloa.

-module(kanaloa).
-author('Stephen Schwink <kanaloa@schwink.net>').

%% @headerfile "../include/kanaloa.hrl"
-include("../include/kanaloa.hrl").

-export([start_link/2]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
        {error, {already_started, App}} ->
	    ok
    end.

%% @spec start_link(MochiOptions::proplist(), KanaloaOptions::proplist()) -> {ok, Pid::pid()}
%% @doc Starts a local instance of the kanaloa mochiweb server. The pid() of the kanaloa supervisor is returned, and may be included
%% in the supervisor tree of the consuming application.
%%
%% See the mochiweb documentation at [http://dawsdesign.com/man/mochiweb/] for details on MochiOptions. The 'handler' option will be overridden by kanaloa.
%%
%% KanaloaOptions is a proplist() with the following possible items:
%% ===handler===
%% A fun(Connection::kanaloa_connection()) that is called when a new connection is established. Required.
%% ===http_content_type===
%% A term to pass as the value of the HTTP Content-Type header. Defaults to application/json.
%% ===parse_jsonrpc===
%% A boolean() indicating whether kanaloa should parse incoming data as JSON-RPC requests. Defaults to false.
%% ===batch_interval===
%% An integer() representing the number of milliseconds between batches are sent to a connected client. Defaults to 1000 (1 second).
%% ===batch_count===
%% An integer() representing the number of batches to send before forcing the client to make a fresh connection.
%% This is necessary to enforce memory leak protection on the client (so they don't try to hold a big unused buffer in memory). Defaults to 32.
%%
%% batch_interval * batch_count = maximum duration of an HTTP connection between client and server.
%% (Note that HTTP keep-alive should make the TCP connection duration much longer.)
start_link(MochiOptions, KanaloaOptions) when is_list(MochiOptions) andalso is_list(KanaloaOptions) ->
    MochiOptions2 = get_mochiweb_options(MochiOptions),
    Settings = get_kanaloa_settings(KanaloaOptions),

    ensure_started(crypto),
    
    Result = kanaloa_sup:start_link(MochiOptions2, Settings),
    io:format("kanaloa:start_link result: ~w\n", [Result]),
    Result.

%% @doc We need to supervise the options passed to mochiweb.
get_mochiweb_options(Options) ->
    Options1 = proplists:delete(docroot, Options), % Must unset the docroot property.
    Options2 = proplists:delete(loop, Options1), % Ignore the loop property, since we're going to replace it anyway.
    
    Options3 = set_default_option(max, Options2, 1048576), % Set a high default for max number of connections.
    
    Options4 = replace_option(name, Options3, ?MODULE),
    Options4.

%% @doc Parse out the required and optional options.
get_kanaloa_settings(Options) ->
    HandlerFun = case proplists:get_value(handler, Options, undefined) of
		     H when is_function(H) ->
			 H
		 end,
    ContentType = case proplists:get_value(http_content_type, Options, <<"application/json">>) of
		      C when is_binary(C) orelse is_list(C) ->
			  C
		  end,
    ParseJsonRpc = case proplists:get_value(parse_jsonrpc, Options, false) of
		       P when is_boolean(P) ->
			   P
		   end,
    BatchInterval = case proplists:get_value(batch_interval, Options, 1000) of
			BI when is_integer(BI) andalso (BI > 50) ->
			    BI
		    end,
    BatchCheckInterval = trunc(BatchInterval / 2),
    BatchCount = case proplists:get_value(batch_count, Options, 32) of
		     BC when is_integer(BC) andalso (BC > 16) ->
			 BC
		 end,

    ConnectionTimeout = case proplists:get_value(connection_message_timeout, Options, 100) of
			    CT when is_integer(CT) ->
				CT
			end,
    
    #kanaloa_settings{
			  handler = HandlerFun,
			  http_content_type = ContentType,
			  parse_jsonrpc = ParseJsonRpc,
			  batch_interval = BatchInterval,
			  batch_check_interval = BatchCheckInterval,
			  batch_count = BatchCount,
			  connection_message_timeout = ConnectionTimeout
			 }.

%% @doc Replaces the specified entry in a proplist.
replace_option(Key, Options, Value) ->
    [{Key, Value} | proplists:delete(Key, Options)].

%% @doc If the specified option is not present in the proplist, set it to the specified value.
set_default_option(Key, Options, Default) ->
    case proplists:get_value(Key, Options, undefined) of
	undefined ->
	    replace_option(Key, Options, Default);
	_ ->
	    Options
    end.
