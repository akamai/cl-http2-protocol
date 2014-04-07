CL-HTTP2-PROTOCOL
========

What This Is
------------

This is [HTTP/2.0
draft-06](http://tools.ietf.org/html/draft-ietf-httpbis-http2-06)
[interopability test
code](https://github.com/http2/http2-spec/wiki/Implementations)
written in Common Lisp. It has only been tested against SBCL 1.1.8.0
on x86, but it should be possible to make it work on other Common Lisp
implementations, subject to some editing in util.lisp.

The code offers a pure Common Lisp transport agnostic implementation
of the HTTP 2.0 protocol at draft-06. An example client and server are
included for a "Hello, World" style test, which employ TLS using
`CL+SSL` and OpenSSL. For unencrypted communication (a.k.a. plain or
direct), code using `USOCKET` is included (on SBCL, another option is
available using `SB-BSD-SOCKETS`), but this is offered as a
convenience only, as HTTP/2.0 will enforce TLS.

Support for:

* [Binary
  framing](http://chimera.labs.oreilly.com/books/1230000000545/ch12.html#_binary_framing_layer)
  parsing and encoding
* [Stream
  multiplexing](http://chimera.labs.oreilly.com/books/1230000000545/ch12.html#HTTP2_STREAMS_MESSAGES_FRAMES)
  and
  [prioritization](http://chimera.labs.oreilly.com/books/1230000000545/ch12.html#HTTP2_PRIORITIZATION)
* Connection and stream [flow
  control](http://chimera.labs.oreilly.com/books/1230000000545/ch12.html#_flow_control)
* [Header
  compression](http://chimera.labs.oreilly.com/books/1230000000545/ch12.html#HTTP2_HEADER_COMPRESSION)
* [Server push](http://chimera.labs.oreilly.com/books/1230000000545/ch12.html#HTTP2_PUSH)
* Connection and stream management

Current implementation (see [HPBN chapter for HTTP 2.0 overview](http://chimera.labs.oreilly.com/books/1230000000545/ch12.html)), is based on:

* [draft-ietf-httpbis-http2-06](http://tools.ietf.org/html/draft-ietf-httpbis-http2-06)
* [draft-ietf-httpbis-header-compression-03](http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-03)

Copyright
---------

* Copyright (c) 2014 Akamai Technologies, Inc. Published under MIT
  License.

* Contains design and text from
  [http-2](https://github.com/igrigorik/http-2) which contains this
  notice: "(MIT License) - Copyright (c) 2013 Ilya Grigorik"

* Contains code from
  [CL+SSL](http://common-lisp.net/project/cl-plus-ssl/) which contains
  this notice: "This library is a fork of SSL-CMUCL. The original
  SSL-CMUCL source code was written by Eric Marsden and includes
  contributions by Jochen Schmidt. Development into CL+SSL was done by
  David Lichteblau. License: MIT-style."

Notes on Port
-------------

This code began life as a port from the [Ruby interopability
code](https://github.com/igrigorik/http-2) written by Ilya Grigorik
released under MIT license. This code may or may not track changes to
Ilya's code, depending on how quickly we move forward to later draft
iterations, etc. The port was done line-by-line so currently the
structure, comments, and techniques closely follow along. Some port
notes sprinkled in the code discuss choices or notes about the
port. For the most part, the major differences are:

* util.lisp defines several general purpose forms that give us some
  capabilities similar to calls in Ruby as well as convenience calls.

* buffer.lisp is much longer than buffer.rb in order to allow us to
  build up various primitives that Ruby offers in the `String` class
  for free.

* ssl.lisp redefines some items in the `CL+SSL` package (based on the
  `cl+ssl-20140316-git` version) in order to allow SSL communication
  for a variable-length frame binary protocol, as well as adding
  support for wrapping NPN support from OpenSSL. See comments in that
  file for more information.

* net.lisp provides a `NET` class that abstracts networking
  sufficiently to allow various approaches (`CL+SSL`, `USOCKET` albeit
  not in the most efficient manner due to reading variable-sized
  binary sequences, `SB-BSD-SOCKETS` on SBCL which is more efficient).

* The code in the example folder is contained in example.lisp in
  the form of functions.
  
* The Ruby code uses arrays and hashes which in the CL code are
  variously ported as alists, plists, and hashes depending on the
  specifics of access required.

* The error conditions are largely the same, but are prefixed with
  `HTTP2-` and an additional one is added named `HTTP2-NOT-STARTED` as
  it is very convenient in debugging (when NPN fails, etc).

* Classes are matched one-to-one but module/package organization is
  different. Use the `HTTP2` package for most functions and the
  `HTTP2-EXAMPLE` package for the examples.

This Common Lisp code was produced by Martin Flack, a Principal
Architect on the Foundry team in the Web Experience business at
Akamai. Our team's mission is innovative applied R&D, and accordingly
we explore new technologies close to the mission of excellent web
experience. Note that this code is intended to be used against the
other HTTP/2.0 interopability client/server code linked above, and not
necessarily any part of the Akamai network.

This system was named `CL-HTTP2-PROTOCOL` to avoid misunderstanding
that it was related to `CL-HTTP`, a Common Lisp web server.
Apologies for the redundant word, but it matches common parlance.

Server Setup and Example
------------

To run this on a fresh Ubuntu Linux 13.10 server, follow these
instructions. A non-root user of "ubuntu" with sudo access is assumed.

```shell
sudo apt-get update && sudo apt-get dist-upgrade -y && sudo reboot
# if prompted about grub choose "install package maintainer's version"
#
# copy files to server under ~ubuntu/cl-http2-protocol
# log in again
sudo apt-get install -y sbcl
wget http://beta.quicklisp.org/quicklisp.lisp
sbcl --script <<EOF
(load "quicklisp.lisp")
(quicklisp-quickstart:install)
(ql:quickload :swank)
(ql:quickload :alexandria)
(ql:quickload :babel)
(ql:quickload :puri)
(ql:quickload :usocket)
(ql:quickload :cl+ssl)
EOF
# and this one starts the server:
sbcl --script <<EOF
(load "quicklisp/setup.lisp")
(load "cl-http2-protocol/cl-http2-protocol.asd")
(require :cl-http2-protocol)
(in-package :http2-example)
(example-server :secure t)
EOF
# now you have an HTTP/2.0 server on port 8080
# note, any exception will cause it to exit
# EXAMPLE-SERVER accepts a :port keyword as well
#
# this will run a client (you can use screen to do both):
sbcl --script <<EOF
(load "quicklisp/setup.lisp")
(load "cl-http2-protocol/cl-http2-protocol.asd")
(require :cl-http2-protocol)
(in-package :http2-example)
(example-client "https://localhost:8080/")
EOF
```

Getting Started
---------------

```lisp
(load "cl-http2-protocol/cl-http2-protocol.asd")
(require :cl-http2-protocol)

(defvar socket #|  provide a transport...  |#)

(defvar conn (make-instance 'client))
(on conn :frame (lambda (bytes) #|  send the bytes...  |#))

(loop for bytes = #|  read some bytes...  |#
      if bytes (connection<< conn bytes) else (return))
```

Check out `EXAMPLE-CLIENT` and `EXAMPLE-SERVER` in `HTTP2-EXAMPLE` for
basic examples. These functions use `CL+SSL` for secure connections,
or `USOCKET` / `SB-BSD-SOCKETS` for plain connections.

Connection lifecycle management
-------------------------------

Depending on the role of the endpoint you must initialize either a
`CLIENT` or a `SERVER` object. Doing so picks the appropriate header
compression / decompression algorithms and stream management
logic. From there, you can subscribe to connection level events, or
invoke appropriate APIs to allocate new streams and manage the
lifecycle. For example:

```lisp
;;; server

(in-package :http2-example)

(defvar server (make-instance 'server))

(on server :stream (lambda (stream) ...))  ; process inbound stream
(on server :frame (lambda (bytes) ...))  ; encoded HTTP 2.0 frames

(ping server (lambda (payload) ...))  ; send ping, process pong

(goaway server)  ; send goaway frame to the client

;;; client

(in-package :http2-example)

(defvar client (make-instance 'client))

(on client :promise (lambda (stream) ...))  ; process push promise

(defparameter stream (new-stream client))  ; allocate new stream
(headers stream '((":method" . "post")) :end-stream: nil)
(data stream (buffer-simple "Hello") :end-stream t)
```

Events emitted by the `CONNECTION` object:

<table>
  <tr>
    <td><b>:promise</b></td>
    <td>client role only, fires once for each new push promise</td>
  </tr>
  <tr>
    <td><b>:stream</b></td>
    <td>server role only, fires once for each new client stream</td>
  </tr>
  <tr>
    <td><b>:frame</b></td>
    <td>fires once for every encoded HTTP 2.0 frame that needs to be sent to the peer</td>
  </tr>
</table>

Stream lifecycle management
---------------------------

A single HTTP 2.0 connection can
[multiplex multiple streams](http://chimera.labs.oreilly.com/books/1230000000545/ch12.html#REQUEST_RESPONSE_MULTIPLEXING)
in parallel: multiple requests and responses can be in flight
simultaneously and stream data can be interleaved and
prioritized. Further, the specification provides a well-defined
lifecycle for each stream (see below).

The good news is, all of the stream management, and state transitions,
and error checking is handled by the library. All you have to do is
subscribe to appropriate events (marked with ":" prefix in diagram
below) and provide your application logic to handle request and
response processing.

```
                         +--------+
                    PP   |        |   PP
                ,--------|  idle  |--------.
               /         |        |         \
              v          +--------+          v
       +----------+          |           +----------+
       |          |          | H         |          |
   ,---|:reserved |          |           |:reserved |---.
   |   | (local)  |          v           | (remote) |   |
   |   +----------+      +--------+      +----------+   |
   |      | :active      |        |      :active |      |
   |      |      ,-------|:active |-------.      |      |
   |      | H   /   ES   |        |   ES   \   H |      |
   |      v    v         +--------+         v    v      |
   |   +-----------+          |          +-_---------+  |
   |   |:half_close|          |          |:half_close|  |
   |   |  (remote) |          |          |  (local)  |  |
   |   +-----------+          |          +-----------+  |
   |        |                 v                |        |
   |        |    ES/R    +--------+    ES/R    |        |
   |        `----------->|        |<-----------'        |
   | R                   | :close |                   R |
   `-------------------->|        |<--------------------'
                         +--------+
```

For sake of example, let's take a look at a simple server implementation:

```Common Lisp
(defvar conn (make-instance 'server))

; emits new streams opened by the client
(on conn :stream
  (lambda (stream)
    (on stream :active
      (lambda () ))  ; fires when stream transitions to open
    (on stream :close
      (lambda (err) ))  ; stream is closed by client and server

    (on stream :headers
      (lambda (head) ))  ; header callback
    (on stream :data
      (lambda (chunk) ))  ; body payload callback
	  
    (on stream :half-close
	  (lambda ()
	    ; ... generate response ...
		; send response
		(headers stream '((":status" . "200")
		                  ("content-type" . "text/plain")))
						  
        ; split response between multiple DATA frames
		(let ((chunk1 (buffer-simple "stuff "))
		      (chunk2 (buffer-simple "more stuff")))
		  (data stream chunk1 :end-stream nil)
		  (data stream chunk2))))))
```

Events emitted by the `STREAM` object:

<table>
  <tr>
    <td><b>:reserved</b></td>
    <td>fires exactly once when a push stream is initialized</td>
  </tr>
  <tr>
    <td><b>:active</b></td>
    <td>fires exactly once when the stream become active and is counted towards the open stream limit</td>
  </tr>
  <tr>
    <td><b>:headers</b></td>
    <td>fires once for each received header block (multi-frame blocks are reassembled before emitting this event)</td>
  </tr>
  <tr>
    <td><b>:data</b></td>
    <td>fires once for every DATA frame (no buffering)</td>
  </tr>
  <tr>
    <td><b>:half_close</b></td>
    <td>fires exactly once when the opposing peer closes its end of connection (e.g. client indicating that request is finished, or server indicating that response is finished)</td>
  </tr>
  <tr>
    <td><b>:close</b></td>
    <td>fires exactly once when both peers close the stream, or if the stream is reset</td>
  </tr>
  <tr>
    <td><b>:priority</b></td>
    <td>fires once for each received priority update (server only)</td>
  </tr>
</table>

Prioritization
--------------

Each HTTP 2.0
[stream has a priority value](http://chimera.labs.oreilly.com/books/1230000000545/ch12.html#HTTP2_PRIORITIZATION)
that can be sent when the new stream is initialized, and optionally
reprioritized later:

```Common Lisp
(defvar client (make-instance 'client))

(defvar default-priority-stream (new-stream client))
(defvar custom-priority-stream (new-stream client :priority 42))

; sometime later, change priority value
(reprioritize custom-priority-stream 3200)  ; emits PRIORITY frame
```

On the opposite side, the server can optimize its stream processing
order or resource allocation by accessing the stream priority value
with `(STREAM-PRIORITY STREAM)`.

Flow control
-----------

Multiplexing multiple streams over the same TCP connection introduces
contention for shared bandwidth resources. Stream priorities can help
determine the relative order of delivery, but priorities alone are
insufficient to control how the resource allocation is performed
between multiple streams. To address this, HTTP 2.0 provides a simple
mechanism for
[stream and connection flow control](http://chimera.labs.oreilly.com/books/1230000000545/ch12.html#_flow_control).

Connection and stream flow control is handled by the library: all
streams are initialized with the default window size (64KB), and
send/receive window updates are automatically processed - i.e. window
is decremented on outgoing data transfers, and incremented on receipt
of window frames. Similarly, if the window is exceeded, then data
frames are automatically buffered until window is updated.

The only thing left is for your application to specify the logic as to
when to emit window updates:

```Common Lisp
(buffered-amount conn)      ; check amount of buffered data
(conn-window conn)          ; check current window size
(window-update conn 1024)   ; increment connection window by 1024 bytes

(buffered-amount stream)    ; check amount of buffered data
(stream-window stream)      ; check current window size
(window-update stream 2048) ; increment stream window by 2048 bytes
```

Alternatively, flow control can be disabled by emitting an appropriate
settings frame on the connection:

```Common Lisp
; limit the number of concurrent streams to 100 and disable flow control
(settings conn :streams 100 :window +infinity)
```

The symbol `+INFINITY` is defined in util.lisp and should be a symbol
macro for IEEE floating point positive infinity on your CL
implementation.

Server push
-----------

An HTTP 2.0 server can
[send multiple replies](http://chimera.labs.oreilly.com/books/1230000000545/ch12.html#HTTP2_PUSH)
to a single client request. To do so, first it emits a "push promise"
frame which contains the headers of the promised resource, followed by
the response to the original request, as well as promised resource
payloads (which may be interleaved). A simple example is in order:

```Common Lisp
(defvar conn (make-instance 'server))

(on conn :stream
  (lambda (stream)
    (on stream :headers
	  (lambda (head) ... ))
    (on stream :data
	  (lambda (chunk ... ))

    ; fires when client terminates its request (i.e. request finished)
    (on stream :half-close
	  (lambda ()
	    (let ((head '((":status" . "200")
		              (":path" . "/other_resource")
					  ("content-type" . "text/plain")))
              promise)
          (promise stream head
		    (lambda (push)
			  (headers push ...)
			  (setf promise push))))
			  
        (headers stream '((":status" . "200")
		                  ("content-type" . "text/plain")))
        (data stream response-chunk :end-stream nil)
		(data promise payload)
		(data stream last-chunk))))))
```

When a new push promise stream is sent by the server, the client is
notified via the `:promise` event:

```Common Lisp
(defvar conn (make-instance 'client))
(on conn :promise
  (lambda (push)
    ; process push stream
	))
```

The client can cancel any given push stream (via `STREAM-CLOSE`), or
disable server push entirely by sending the appropriate settings frame
(note that below setting only impacts server > client direction):

```Common Lisp
(settings client :streams: 0)  ; setting max limit to 0 disables server push
```
