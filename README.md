# RPC over NSQ

This is the implementation of a RPC protocol based in the [NSQ](http://bitly.github.io/nsq/) message queue. It is implemented the following way:

* Each RPC endpoint has a topic on which it receives `requests`.
* Each RPC client has a unique topic on which it receives `responses`.
* Each request has:
	* The version.
  	* The encoding used.
	* A 16 byte (128 bit) unique ID  (generally a uuid but that is an implementation detail).
 	* A body that carries the request payload.
 	* An host and port of a NSQD to reply to.
 	* A topic to reply to (which equals the unique topic of the RCP client).
 	
* Each response has:
	* The version.
	* The encoding used.
 	* Optionally the encoding of the body when the encoding is binary.
	* The ID of the request.
	* A Response body.


**All data is encoded as big endian.**

## Encoding


Each message is prefixed with one byte that indicates the encoding the following encodings are used:

* 0: binary   - the client hands the data on without further processing
* 1: JSON
* 2: BERT
* 3: msgpack
* 254: **reserved**
* 255: **reserved: special care for reencoding requests**

All clients and endpoints must implement JSON as `encoding` for compatibility, all other encodings are optional.

```
|  1  | 16 |    1     |     1     |     1      |  4   | len(host) | len(topic) | ...  |
| vsn | id | encoding | len(host) | len(topic) | port |   host    |   topic    | body |

```

### Response
```
|  1  | 16 |     1    | ...  |
| vsn | id | encoding | body |
```

#### Incompatibility

When a endpoint receives a request in a not supported encoding it can send back a reply with:
* the id of the request
* the reserved encoding value 255
* no body

```
|  1  | 16 |  1   |
| vsn | id | 0xff |
```

The client the must either drop the request or resend it encoded as JSON to guarantee compatibility.

