%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc Utilities for parsing JSON-RPC version 1.0 objects.
%% For more information, set [http://json-rpc.org/wiki/specification].
%% The main caveat is that we only support integer IDs.

-module(kanaloa_rpc).
-author('Stephen Schwink <kanaloa@schwink.net>').

-export([parse/1, format_call/3, format_result/2, format_error/2]).

%% @spec parse(json_object()) -> (rpc_request() | rpc_response())
%% where rpc_request() = {request, Id::(integer() | null), Method::binary(), Params::json_array()}
%%       rpc_response() = {response, Id::integer(), Result::json_term(), Error::json_term()}
%% @doc Parses a mochijson2 json_object() as a JSON-RPC request or result.
parse({struct, Fields}) when is_list(Fields) ->
    parse_fields(Fields, {noid, nomethod, noparams, noresult, noerror}).

% Note that in mochijson2, field names are usually binaries, but could be atoms.
-define(RPC_ID, <<"id">>).
-define(RPC_METHOD, <<"method">>).
-define(RPC_PARAMS, <<"params">>).
-define(RPC_RESULT, <<"result">>).
-define(RPC_ERROR, <<"error">>).

% Note that according to spec, an ID can be any type. In practice, we limit it to 'null' and integers.
parse_fields([{id, Value} | Rest], Spec) ->
    parse_fields([{?RPC_ID, Value} | Rest], Spec);
parse_fields([{?RPC_ID, Value} | Rest], {noid, M, P, R, E}) when Value == null orelse is_integer(Value) ->
    parse_fields(Rest, {Value, M, P, R, E});

parse_fields([{method, Value} | Rest], Spec) ->
    parse_fields([{?RPC_METHOD, Value} | Rest], Spec);
parse_fields([{?RPC_METHOD, Value} | Rest], {I, nomethod, P, R, E}) when is_binary(Value) ->
    parse_fields(Rest, {I, Value, P, R, E});

% In version 1.0, there are only positional parameters (no named parameters).
parse_fields([{params, Value} | Rest], Spec) ->
    parse_fields([{?RPC_PARAMS, Value} | Rest], Spec);
parse_fields([{?RPC_PARAMS, Value} | Rest], {I, M, noparams, R, E}) when is_list(Value) ->
    parse_fields(Rest, {I, M, Value, R, E});

parse_fields([{result, Value} | Rest], Spec) ->
    parse_fields([{?RPC_RESULT, Value} | Rest], Spec);
parse_fields([{?RPC_RESULT, Value} | Rest], {I, M, P, noresult, E}) ->
    parse_fields(Rest, {I, M, P, Value, E});

parse_fields([{error, Value} | Rest], Spec) ->
    parse_fields([{?RPC_ERROR, Value} | Rest], Spec);
parse_fields([{?RPC_ERROR, Value} | Rest], {I, M, P, R, noerror}) ->
    parse_fields(Rest, {I, M, P, R, Value});

parse_fields([], {noid, _, _, _, _}) ->
    % Must always have an id.
    erlang:error(rpc_missing_id);
parse_fields([], {Id, Method, Params, noresult, noerror}) when is_binary(Method) andalso is_list(Params) ->
    {request, Id, Method, Params};
parse_fields([], {Id, nomethod, noparams, Result, null}) ->
    {response, Id, Result, null};
parse_fields([], {Id, nomethod, noparams, null, Error}) ->
    {response, Id, null, Error};
parse_fields([], _) ->
    erlang:error(rpc_invalid_format).

%% @spec format_call(Id::integer(), Method::binary(), Params::[json_term()]) -> iolist()
%% @doc Creates a JSON-RPC request object suitable for making an RPC call.
format_call(Id, Method, Params) when (Id == null orelse is_integer(Id)) andalso is_binary(Method) andalso is_list(Params) ->
    JsonObject = {struct, [{?RPC_ID, Id},
			   {?RPC_METHOD, Method},
			   {?RPC_PARAMS, Params}]},
    mochijson2:encode(JsonObject).

%% @spec format_result(Id::integer(), Result::json_term()) -> iolist()
%% @doc Creates a JSON-RPC response object suitable for reporting the result of an RPC call.
format_result(Id, Result) when is_integer(Id) ->
    JsonObject = {struct, [{?RPC_ID, Id},
			   {?RPC_RESULT, Result},
			   {?RPC_ERROR, null}]},
    mochijson2:encode(JsonObject).

%% @spec format_error(Id::rpc_id(), Error::json_term()) -> iolist()
%% where rpc_id() = integer() | binary()
%% @doc Creates a JSON-RPC response object suitable for reporting an error for an RPC call.
format_error(Id, Error) when is_integer(Id) ->
    JsonObject = {struct, [{?RPC_ID, Id},
			   {?RPC_RESULT, null},
			   {?RPC_ERROR, Error}]},
    mochijson2:encode(JsonObject).
