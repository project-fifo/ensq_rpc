%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2014, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 11 Feb 2014 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(ensq_rpc_proto).

-include("ensq_rpc.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([decode_request/1, encode_request/2,
         decode_response/1, encode_response/2,
         reencode/1]).

decode_request(<<?VSN:8/big-unsigned-integer,  %% Protocol version
                 ID:16/big-binary,             %% Binary ID
                 Enc:8/big-unsigned-integer,   %% The encoding of the body.
                 RHL:8/big-unsigned-integer,  %% Length of the Host
                 RTL:8/big-unsigned-integer,   %% Length of the Topic
                 Port:32/big-unsigned-integer, %% Port to send the response to
                 Host:RHL/big-binary,          %% The response Host
                 Topic:RTL/big-binary,         %% The response Topic
                 Body/big-binary>>) ->      %% The body of the message
    H = #rpc_header{
           vsn = ?VSN,
           id = ID,
           host = binary_to_list(Host),
           port = Port,
           topic = Topic
          },
    case encoding(Enc) of
        unsupported ->
            {h, unsupported};
        E ->
            {H#rpc_header{encoding = E}, decode(E, Body)}
    end.

encode_request(#rpc_header{
                  id = ID,
                  encoding = Enc,
                  host = HostL,
                  port = Port,
                  topic = Topic
                 }, Body) ->
    Host = list_to_binary(HostL),
    E = encoding(Enc),
    Bin = encode(Enc, Body),
    RHL = byte_size(Host),
    RTL = byte_size(Topic),
    <<?VSN:8/big-unsigned-integer, ID:16/big-binary, E:8/big-unsigned-integer,
      RHL:8/big-unsigned-integer,
      RTL:8/big-unsigned-integer,
      Port:32/big-unsigned-integer,Host:RHL/big-binary, Topic:RTL/big-binary,
      Bin/big-binary>>.


decode_response(<<?VSN:8/big-unsigned-integer, ID:16/big-binary,
                  ?REENCODE:8/big-unsigned-integer>>) ->
    {ID, reencode};
decode_response(<<?VSN:8/big-unsigned-integer,
                  ID:16/big-binary,              %% ID of the request
                  BE:8/big-unsigned-integer,     %% The encoding of the body.
                  Body/big-binary>>) ->       %% The body of the message
    {ID, decode(encoding(BE), Body)}.

%% A response of 255 requirests a reencode

encode_response(#rpc_header{
                   id = ID,
                   encoding = Enc
                  }, Body) ->
    Bin = encode(Enc, Body),
    BE = encoding(Enc),
    <<?VSN:8/big-unsigned-integer,
      ID:16/big-binary,
      BE:8/big-unsigned-integer,          %% The encoding of the body.
      Bin/big-binary>>.

reencode(#rpc_header{id=ID}) ->
    <<?VSN:8/big-unsigned-integer, ID:16/big-binary,
      ?REENCODE:8/big-unsigned-integer>>.


%%%===================================================================
%%% Internal functions
%%%===================================================================

decode(json, B) ->
    jsx:decode(B);
decode(bert, B) ->
    binary_to_term(B);
decode(binary, B) ->
    B.

encode(json, B) ->
    jsx:encode(B);
encode(bert, B) ->
    term_to_binary(B);
encode(binary, B) when is_binary(B) ->
    B.

%encoding(<<"binary">>) -> binary;
encoding(?ENC_BIN) -> binary;
encoding(binary) -> ?ENC_BIN;
%encoding(<<"json">>) -> json;
encoding(?ENC_JSON) -> json;
encoding(json) -> ?ENC_JSON;
%encoding(<<"bert">>) -> bert;
encoding(?ENC_BERT) -> bert;
encoding(bert) -> ?ENC_BERT;
                                                % encoding(?ENC_BERT) -> msgpack;
                                                % encoding(msgpack) -> ?ENC_BERT;
encoding(_N) when is_integer(_N) -> unsupported.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
-define(ID, <<"1234567890abcdef">>).
mk_h(E) ->
    #rpc_header{
       id = ?ID,
       encoding = E,
       host = "localhost",
       port = 1234,
       topic = <<"topic">>
      }.

req_bin_test() ->
    Header = mk_h(binary),
    Body = <<"body">>,
    Msg = encode_request(Header, Body),
    {Header1, Body1} = decode_request(Msg),
    ?assertEqual(Header, Header1),
    ?assertEqual(Body, Body1).

req_json_test() ->
    Header = mk_h(json),
    Body = [{<<"body">>, 42}],
    Msg = encode_request(Header, Body),
    {Header1, Body1} = decode_request(Msg),
    ?assertEqual(Header, Header1),
    ?assertEqual(Body, Body1).

req_bert_test() ->
    Header = mk_h(bert),
    Body = [{<<"body">>, 42}],
    Msg = encode_request(Header, Body),
    {Header1, Body1} = decode_request(Msg),
    ?assertEqual(Header, Header1),
    ?assertEqual(Body, Body1).


resp_bin_test() ->
    Header = mk_h(binary),
    Body = <<"body">>,
    Msg = encode_response(Header, Body),
    {ID, Body1} = decode_response(Msg),
    ?assertEqual(?ID, ID),
    ?assertEqual(Body, Body1).

resp_json_test() ->
    Header = mk_h(json),
    Body = [{<<"body">>, 42}],
    Msg = encode_response(Header, Body),
    {ID, Body1} = decode_response(Msg),
    ?assertEqual(?ID, ID),
    ?assertEqual(Body, Body1).

resp_bert_test() ->
    Header = mk_h(bert),
    Body = [{<<"body">>, 42}],
    Msg = encode_response(Header, Body),
    {ID, Body1} = decode_response(Msg),
    ?assertEqual(?ID, ID),
    ?assertEqual(Body, Body1).

reencode_test() ->
    Header = mk_h(bert),
    Msg = reencode(Header),
    Res = decode_response(Msg),
    ?assertEqual({?ID, reencode}, Res).

-endif.
