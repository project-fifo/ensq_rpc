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
         decode_response/1, encode_response/2]).

decode_request(<<?ENC_BIN:8/integer,    %% Binary decoded header.
                 BE:8/integer,          %% The encoding of the body.
                 IDL:8/integer,         %% Length of the ID
                 RHL:16/integer,        %% Length of the Host to return to
                 RTL:8/integer,         %% Length of the Topic to return to
                 BL:32/integer,         %% Lenght of the body
                 Port:32/integer,       %% Port to send the response to
                 ID:IDL/binary,         %% The message ID to response with
                 Host:RHL/binary,       %% The Host to send the response to
                 Topic:RTL/binary,      %% The Topic for the response
                 Body:BL/binary>>       %% The body of the message
              ) ->
    BodyEnc = encoding(BE),
    H = #rpc_header{
           id = ID,
           encoding = binary,
           body_encoding = BodyEnc,
           host = binary_to_list(Host),
           port = Port,
           topic = Topic
          },
    {H, decode(BodyEnc, Body)};

decode_request(<<Enc:8/integer, Bin/binary>>) ->
    B1 = decode(encoding(Enc), Bin),
    [{<<"body">>, Body},
     {<<"host">>, HostB},
     {<<"id">>, ID},
     {<<"port">>, Port},
     {<<"topic">>, Topic}] = lists:sort(B1),
    Host = binary_to_list(HostB),
    H = #rpc_header{
           id = ID,
           encoding = encoding(Enc),
           body_encoding = encoding(Enc),
           host = Host,
           port = Port,
           topic = Topic
          },
    {H, Body}.

encode_request(#rpc_header{
                  id = ID,
                  encoding = binary,
                  body_encoding = BodyEnc,
                  host = HostL,
                  port = Port,
                  topic = Topic
                 }, Body) ->
    Host = list_to_binary(HostL),
    BE = encoding(BodyEnc),
    Bin = encode(BodyEnc, Body),
    IDL = byte_size(ID),
    RHL = byte_size(Host),
    RTL = byte_size(Topic),
    BL = byte_size(Bin),
    <<?ENC_BIN:8/integer, BE:8/integer, IDL:8/integer, RHL:16/integer,
      RTL:8/integer, BL:32/integer, Port:32/integer, ID:IDL/binary,
      Host:RHL/binary, Topic:RTL/binary, Bin:BL/binary>>;

encode_request(#rpc_header{
                  id = ID,
                  encoding = E,
                  body_encoding = E,
                  host = HostL,
                  port = Port,
                  topic = Topic
                 }, Body) ->
    Host = list_to_binary(HostL),
    Content = encode(E, [{<<"body">>, Body},
                         {<<"host">>, Host},
                         {<<"id">>, ID},
                         {<<"port">>, Port},
                         {<<"topic">>, Topic}]),
    Encoding=encoding(E),
    <<Encoding:8/integer, Content/binary>>.

decode_response(<<?ENC_BIN:8/integer,
                  BE:8/integer,          %% The encoding of the body.
                  IDL:8/integer,         %% Length of the ID
                  BL:32/integer,         %% Lenght of the body
                  ID:IDL/binary,         %% The message ID to response with
                  Body:BL/binary>>       %% The body of the message
               ) ->
    {ID, decode(encoding(BE), Body)};

decode_response(<<Enc:8/integer, Bin/binary>>) ->
    B1 = decode(encoding(Enc), Bin),
    [{<<"body">>, Body},
     {<<"id">>, ID}] = lists:sort(B1),
    {ID, Body}.

encode_response(#rpc_header{
                   id = ID,
                   encoding = binary,
                   body_encoding = BodyEnc
                  }, Body) ->
    Bin = encode(BodyEnc, Body),
    BE = encoding(BodyEnc),
    IDL = byte_size(ID),
    BL = byte_size(Bin),
    <<?ENC_BIN:8/integer,
      BE:8/integer,          %% The encoding of the body.
      IDL:8/integer,         %% Length of the ID
      BL:32/integer,         %% Lenght of the body
      ID:IDL/binary,         %% The message ID to response with
      Bin:BL/binary>>;

encode_response(#rpc_header{
                   id = ID,
                   encoding = Enc
                  }, Body) ->
    BE = encoding(Enc),
    B1 = [{<<"body">>, Body}, {<<"id">>, ID}],
    Bin = encode(Enc, B1),
    <<BE:8/integer, Bin/binary>>.


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
encode(binary, B) when is_binary(B)->
    B.

encoding(<<"binary">>) -> binary;
encoding(?ENC_BIN) -> binary;
encoding(binary) -> ?ENC_BIN;
encoding(<<"json">>) -> json;
encoding(?ENC_JSON) -> json;
encoding(json) -> ?ENC_JSON;
encoding(<<"bert">>) -> bert;
encoding(?ENC_BERT) -> bert;
encoding(bert) -> ?ENC_BERT.

%%%===================================================================
%%% Tests
%%%===================================================================
-ifdef(TEST).
mk_h(E, BE) ->
    #rpc_header{
       id = <<"id">>,
       encoding = E,
       body_encoding = BE,
       host = "localhost",
       port = 1234,
       topic = <<"topic">>
      }.

req_bin_bin_test() ->
    Header = mk_h(binary, binary),
    Body = <<"body">>,
    Msg = encode_request(Header, Body),
    {Header1, Body1} = decode_request(Msg),
    ?assertEqual(Header, Header1),
    ?assertEqual(Body, Body1).

req_bin_json_test() ->
    Header = mk_h(binary, json),
    Body = [{<<"body">>, 42}],
    Msg = encode_request(Header, Body),
    {Header1, Body1} = decode_request(Msg),
    ?assertEqual(Header, Header1),
    ?assertEqual(Body, Body1).

req_bin_bert_test() ->
    Header = mk_h(binary, bert),
    Body = [{<<"body">>, 42}],
    Msg = encode_request(Header, Body),
    {Header1, Body1} = decode_request(Msg),
    ?assertEqual(Header, Header1),
    ?assertEqual(Body, Body1).

req_json_test() ->
    Header = mk_h(json, json),
    Body = [{<<"body">>, 42}],
    Msg = encode_request(Header, Body),
    {Header1, Body1} = decode_request(Msg),
    ?assertEqual(Header, Header1),
    ?assertEqual(Body, Body1).

req_bert_test() ->
    Header = mk_h(bert, bert),
    Body = [{<<"body">>, 42}],
    Msg = encode_request(Header, Body),
    {Header1, Body1} = decode_request(Msg),
    ?assertEqual(Header, Header1),
    ?assertEqual(Body, Body1).


resp_bin_bin_test() ->
    Header = mk_h(binary, binary),
    Body = <<"body">>,
    Msg = encode_response(Header, Body),
    {ID, Body1} = decode_response(Msg),
    ?assertEqual(<<"id">>, ID),
    ?assertEqual(Body, Body1).

resp_bin_json_test() ->
    Header = mk_h(binary, json),
    Body = [{<<"body">>, 42}],
    Msg = encode_response(Header, Body),
    {ID, Body1} = decode_response(Msg),
    ?assertEqual(<<"id">>, ID),
    ?assertEqual(Body, Body1).

resp_bin_bert_test() ->
    Header = mk_h(binary, bert),
    Body = [{<<"body">>, 42}],
    Msg = encode_response(Header, Body),
    {ID, Body1} = decode_response(Msg),
    ?assertEqual(<<"id">>, ID),
    ?assertEqual(Body, Body1).

resp_json_test() ->
    Header = mk_h(json, json),
    Body = [{<<"body">>, 42}],
    Msg = encode_response(Header, Body),
    {ID, Body1} = decode_response(Msg),
    ?assertEqual(<<"id">>, ID),
    ?assertEqual(Body, Body1).

resp_bert_test() ->
    Header = mk_h(bert, bert),
    Body = [{<<"body">>, 42}],
    Msg = encode_response(Header, Body),
    {ID, Body1} = decode_response(Msg),
    ?assertEqual(<<"id">>, ID),
    ?assertEqual(Body, Body1).


-endif.
