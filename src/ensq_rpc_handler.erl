-module(ensq_rpc_handler).

-behaviour(ensq_channel_behaviour).

-export([init/0, response/2, message/3, error/2]).

init() -> done.

response(Msg, _) ->
    io:format("[response]  ~p~n", [Msg]).

error(Msg, _) ->
    io:format("[error]  ~p~n", [Msg]).

message(Msg, _, _) ->
    ensq_rpc:reply(Msg).
