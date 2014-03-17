
-type encoding() :: binary | json | bert.

-define(VSN,           0).
-define(ENC_BIN,       0).
-define(ENC_JSON,      1).
-define(ENC_BERT,      2).
-define(ENC_MSGPACK,   3).
-define(REENCODE,    255).

-record(rpc_header,
        {
          id :: binary(),
          vsn = ?VSN :: non_neg_integer(),
          encoding :: encoding(),
          host :: inet:ip_address() | inet:hostname(),
          port :: inet:port_number(),
          topic :: binary()
        }).
