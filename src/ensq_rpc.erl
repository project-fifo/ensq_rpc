%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 11 Feb 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(ensq_rpc).

-behaviour(gen_server).

%% API
-export([start_link/0, body/1, reply/1, send/3, send/4, reply_to/2, start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {pending = [], encoding=json, topic}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    ensq:start(),
    application:start(ensq_rpc).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

reply(Reply) ->
    gen_server:cast(?SERVER, {reply, Reply}).

send({Host, Port}, Topic, Body, Timeout) ->
    gen_server:call(?SERVER, {rpc, Host, Port, Topic, Body}, Timeout).

send({Host, Port}, Topic, Body) ->
    gen_server:call(?SERVER, {rpc, Host, Port, Topic, Body}).

reply_to([{<<"id">>, ID},
          {<<"reply_encoding">>, <<"json">>},
          {<<"reply_host">>, Host},
          {<<"reply_port">>, Port},
          {<<"reply_topic">>, Topic} | _], Body) ->
    R1 = [{<<"id">>, ID}, {<<"reply">>, Body}],
    send_to(binary_to_list(Host), Port, Topic, encode(json, R1));

reply_to([{<<"id">>, ID},
          {<<"reply_encoding">>, <<"bert">>},
          {<<"reply_host">>, Host},
          {<<"reply_port">>, Port},
          {<<"reply_topic">>, Topic} | _], Body) ->
    R1 = [{<<"id">>, ID}, {<<"reply">>, Body}],
    send_to(binary_to_list(Host), Port, Topic, encode(bert, R1)).

body([{<<"id">>, _}, {<<"reply_encoding">>, _}, {<<"reply_host">>, _},
      {<<"reply_port">>, _}, {<<"reply_topic">>, _}, {<<"request">>, Body}]) ->
    {ok, Body};
body(_) ->
    {error, bad_rpc}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Topic =
        case application:get_env(rpc_discovereres) of
            {ok, Ds} ->
                T = case application:get_env(rpc_channel) of
                        {ok, ID} when is_list(ID) ->
                            list_to_binary(ID);
                        {ok, ID} when is_binary(ID) ->
                            ID;
                        _ ->
                            ID = erlang:phash2(os:cmd("hostname")),
                            B = list_to_binary(integer_to_list(ID)),
                            <<"rcp-", B/binary>>
                    end,
                ensq:init({Ds, [{T, [{<<"rpc#ephemeral">>, ensq_rpc_handler}], []}]}),
                T;
            _ ->
                undefined
        end,
    case application:get_env(rpc_encoding) of
        {ok, E} ->
            {ok, #state{encoding=E, topic=Topic}};
        _ ->
            {ok, #state{topic=Topic}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({rpc, Host, Port, Topic, Body}, From, State = #state{pending=P,
                                                                 encoding=E}) ->
    UUID = uuid:uuid4s(),
    Body1 = [{<<"reply_topic">>, State#state.topic},
             {<<"reply_host">>, list_to_binary(Host)},
             {<<"reply_port">>, Port},
             {<<"reply_encoding">>, list_to_binary(atom_to_list(E))},
             {<<"id">>, UUID},
             {<<"request">>, Body}],
    lager:info("Encoding: ~p, Body: ~p", [E, Body1]),
    Bin = encode(E, Body1),
    case send_to(Host, Port, Topic, Bin) of
        ok ->
            {noreply, State#state{pending=[{UUID, From} | P]}};
        E ->
            {reply, E, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({reply, Bin}, State = #state{pending = P, encoding = E}) ->
    Msg = decode(E, Bin),
    case Msg of
        [{<<"id">>, ID}, {<<"reply">>, Body}] ->
            case lists:keyfind(ID, 1, P) of
                {ID, From} ->
                    gen_server:reply(From, {ok, Body}),
                    P1 = lists:keydelete(ID, 1, P),
                    {noreply, State#state{pending = P1}};
                _ ->
                    lager:error("[rpc] Unknown id: ~s", [ID]),
                    {noreply, State}
            end;
        Msg ->
            lager:error("[rpc] Unknown reply: ~p", [Msg]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

decode(bert, B) ->
    lists:sort(binary_to_term(B));
decode(json, B) ->
    lists:sort(jsx:decode(B)).

encode(bert, T) ->
    term_to_binary(T);
encode(json, T) ->
    jsx:encode(T).

send_to(Host, Port, Topic, Bin) ->
    case gen_tcp:connect(Host, Port, [binary]) of
        {ok, Socket} ->
            case gen_tcp:send(Socket, ensq_proto:encode(version)) of
                ok ->
                    Msg = ensq_proto:encode({publish, Topic, Bin}),
                    Res = gen_tcp:send(Socket, Msg),
                    gen_tcp:close(Socket),
                    Res;
                _ ->
                    gen_tcp:close(Socket),
                    {error, version}
            end;
        _ ->
            {error, connect}
    end.
