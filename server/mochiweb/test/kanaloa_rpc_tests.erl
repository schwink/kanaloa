%% @author Stephen Schwink <kanaloa@schwink.net>
%% @copyright 2010 Stephen Schwink.

%% @doc Unit tests for kanaloa_rpc.

-module(kanaloa_rpc_tests).
-author('Stephen Schwink <kanaloa@schwink.net>').

-include_lib("eunit/include/eunit.hrl").

-import(kanaloa_rpc, [parse/1]).
-import(kanaloa_rpc, [format_call/3, format_result/2, format_error/2]).

v1_0_request_one_test() ->
    Json = "{\"method\": \"postMessage\", \"params\": [\"Hello all!\"], \"id\": 99}",
    Spec = {request, 99, <<"postMessage">>, [<<"Hello all!">>]},
    
    Struct = mochijson2:decode(Json),
    Parsed = parse(Struct),
    ?assertEqual(Spec, Parsed).

v1_0_request_two_test() ->
    Json = "{\"method\": \"handleMessage\", \"params\": [\"user1\", \"we were just talking\"], \"id\": null}",
    Spec = {request, null, <<"handleMessage">>, [<<"user1">>, <<"we were just talking">>]},
    
    Struct = mochijson2:decode(Json),
    Parsed = parse(Struct),
    ?assertEqual(Spec, Parsed).

v1_0_request_three_test() ->
    Json = "{\"method\": \"handleMessage\", \"params\": [\"user3\", \"sorry, gotta go now, ttyl\"], \"id\": null}",
    Spec = {request, null, <<"handleMessage">>, [<<"user3">>, <<"sorry, gotta go now, ttyl">>]},
    
    Struct = mochijson2:decode(Json),
    Parsed = parse(Struct),
    ?assertEqual(Spec, Parsed).

v1_0_request_four_test() ->
    Json = "{\"method\": \"postMessage\", \"params\": [\"I have a question:\"], \"id\": 101}",
    Spec = {request, 101, <<"postMessage">>, [<<"I have a question:">>]},
    
    Struct = mochijson2:decode(Json),
    Parsed = parse(Struct),
    ?assertEqual(Spec, Parsed).

v1_0_request_five_test() ->
    Json = "{\"method\": \"userLeft\", \"params\": [\"user3\"], \"id\": null}",
    Spec = {request, null, <<"userLeft">>, [<<"user3">>]},
    
    Struct = mochijson2:decode(Json),
    Parsed = parse(Struct),
    ?assertEqual(Spec, Parsed).

v1_0_request_six_test() ->
    Json = "{\"method\": \"userLeft\", \"params\": [], \"id\": null}",
    Spec = {request, null, <<"userLeft">>, []},
    
    Struct = mochijson2:decode(Json),
    Parsed = parse(Struct),
    ?assertEqual(Spec, Parsed).

v1_0_response_zero_test() ->
    Json = "{ \"result\": \"Hello JSON-RPC\", \"error\": null, \"id\": 1}",
    Spec = {response, 1, <<"Hello JSON-RPC">>, null},
    
    Struct = mochijson2:decode(Json),
    Parsed = parse(Struct),
    ?assertEqual(Spec, Parsed).

v1_0_response_one_test() ->
    Json = "{\"result\": 1, \"error\": null, \"id\": 99}",
    Spec = {response, 99, 1, null},
    
    Struct = mochijson2:decode(Json),
    Parsed = parse(Struct),
    ?assertEqual(Spec, Parsed).

v1_0_response_two_test() ->
    Json = "{\"result\": null, \"error\": 1, \"id\": 101}",
    Spec = {response, 101, null, 1},
    
    Struct = mochijson2:decode(Json),
    Parsed = parse(Struct),
    ?assertEqual(Spec, Parsed).

format_result_array_test() ->
    Json = iolist_to_binary(format_result(5, [])),
    Expected = <<"{\"id\":5,\"result\":[],\"error\":null}">>,
    
    ?assertEqual(Expected, Json).

format_result_atom_test() ->
    Json = iolist_to_binary(format_result(5, ok)),
    Expected = <<"{\"id\":5,\"result\":\"ok\",\"error\":null}">>,
    
    ?assertEqual(Expected, Json).

format_error_atom_test() ->
    Json = iolist_to_binary(format_error(5, cache_miss)),
    Expected = <<"{\"id\":5,\"result\":null,\"error\":\"cache_miss\"}">>,
    
    ?assertEqual(Expected, Json).

format_call_atom_test() ->
    Json = iolist_to_binary(format_call(12, <<"update">>, [])),
    Expected = <<"{\"id\":12,\"method\":\"update\",\"params\":[]}">>,
    
    ?assertEqual(Expected, Json).
