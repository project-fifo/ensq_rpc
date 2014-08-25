-module(ensq_rpc_handler).

-behaviour(ensq_channel_behaviour).

-export([init/0, response/2, message/3, error/2]).

init() ->
    {ok, undefined}.

response(Msg, State) ->
    io:format("[response]  ~p~n", [Msg]),
    {ok, State}.

error(Msg, State) ->
    io:format("[error]  ~p~n", [Msg]),
    {ok, State}.

message(Msg, _,  State) ->
    ensq_rpc:reply(Msg),
    {ok, State}.
