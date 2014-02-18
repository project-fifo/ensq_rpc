# RPC over NSQ

This is the implementation of a RPC protocol based in the [NSQ](http://bitly.github.io/nsq/) message queue. It is implemented the following way:

* Each RPC endpoint has a topic on which it receives `requests`.
* Each RPC client has a unique topic on which it receives `responses`.
* Each request has:
 	* The encoding used.
 	* Optionally the encoding of the body when the encoding is binary.
	* A unique ID (generally a uuid but that is an implementation detail).
 	* A body that carries the request payload.
 	* An host and port of a NSQD to reply to.
 	* A topic to reply to (which equals the unique topic of the RCP client).
 	
* Each response has:
	* The encoding used.
 	* Optionally the encoding of the body when the encoding is binary.
	* The ID of the request.
	* A Response body.

## Encoding

**All integers are encoded as little endian.**

Each message is prefixed with one byte that indicates the encoding the following encodings are used:

* 0 -> binary
* 1 -> JSON
* 2 -> BERT
* 3 -> msgpack

All clients must implement JSON as `encoding` for compatibility, all other encodings are optional.

```
|     1    | ...  |
| encoding | body |
```

### Request

#### Binary
```
|       1       |     1      |     2     |     1      |    4      |   4  | len(id) | len(host) | len(topic) | len(body) |
| body_encoding | length(id) | len(host) | len(topic) | len(body) | port |   id    |   host    |   topic    |   body    |
```
The body will need to be further decoded with the given `body_encoding`.

#### JSON/MSGPACK
```json
{
 "body": Body,     // any
 "host": Host,     // string
 "id":, ID,        // string
 "port": Port,     // number
 "topic": Topic    // string
}
```

#### BERT
```erlang
[{<<"body">>,  Body  :: term()},
 {<<"host">>,  Host  :: binary()},
 {<<"id">>,    ID    :: binary()},
 {<<"port">>,  Port  :: inet:ip_address()},
 {<<"topic">>, Topic :: binary()}]
```

### Response
```
|     1    | ...  |
| encoding | body |
```

#### Binary
The binary encoding is defined as:
```
|       1       |     1      |    4      | len(id) | len(body) |
| body_encoding | length(id) | len(body) |   id    |   body    |
```
The body will need to be further decoded with the given `body_encoding`.

#### JSON/MSGPACK
```json
{
 "body": Body,     // any
 "id":, ID         // string
}
```

#### BERT
```erlang
[{<<"body">>,  Body  :: term()},
 {<<"id">>,    ID    :: binary()}]
```

#### Incompatibility

When a endpoint receives a request in a not supported format it can send back a `JSON` encoded reply with the content `{"error": "retransmit"}` for which the client must either fail the request or retransmit it as `JSON` encoded.