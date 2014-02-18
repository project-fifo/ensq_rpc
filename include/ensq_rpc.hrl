
-type encoding() :: binary | json | bert.

-define(ENC_BIN, 0).
-define(ENC_JSON, 1).
-define(ENC_BERT, 2).

-record(rpc_header,
        {
          id :: binary(),
          encoding :: encoding(),
          body_encoding :: encoding(),
          host :: string(),
          port :: inet:ip_address(),
          topic :: binary()
        }).
