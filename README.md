CL-HTTP2-PROTOCOL
========

What This Is
------------

This is
[HTTP/2 draft-14](http://tools.ietf.org/html/draft-ietf-httpbis-http2-14)
(a.k.a. "h2-14")
[interopability test code](https://github.com/http2/http2-spec/wiki/Implementations)
written in Common Lisp. It has only been tested against SBCL 1.1.8.0
to 1.1.14 on Ubuntu Linux on x86, but it should be possible to make it
work on other Common Lisp implementations, subject to some editing in
`util.lisp` and possibly `example.lisp`.

The code offers a pure Common Lisp transport agnostic implementation
of the HTTP/2 protocol at draft-14. An example client and server are
included for a "Hello, World" style test, which employ TLS using
`CL+SSL` and OpenSSL.

Networking examples have been based on the `CL-ASYNC` library. Some
alterations to functions in `CL+SSL` and `CL-ASYNC` were necessary and
are included in the source tree. (Prior versions of this library were
based on homemade event loops based on `USOCKET` and `SB-BSD-SOCKETS`
but this has now been removed.)

The current implementation is based on:

* [draft-ietf-httpbis-http2-14](http://tools.ietf.org/html/draft-ietf-httpbis-http2-14)
* [draft-ietf-httpbis-header-compression-09](http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-09)

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

* Contains code from
  [CL-ASYNC](https://github.com/orthecreedence/cl-async) which contains
  this notice: "As always, my code is MIT licenced. Do whatever the
  hell you want with it. Enjoy!"

Notes on Port
-------------

This code began life as a port from the draft-06
[Ruby interopability code](https://github.com/igrigorik/http-2)
written by Ilya Grigorik released under MIT license. This code has
since diverged to support subsequent working group drafts (up to
draft-14 currently), to allow queueing and multiplexing
support with a prioritization algorithm, and to move away from some
Ruby idioms towards Lisp idioms over time.

The following are notes only of interest to folks who followed along
since the beginning or have some interest in comparing the code
revisions. Others may skip this section.

* `util.lisp` defines several general purpose forms that give us some
  capabilities similar to calls in Ruby, as well as convenience calls
  that are stylistic Lisp choices.

* `buffer.lisp` is much longer than `buffer.rb` in order to allow us
  to build up various primitives that Ruby offers in the `String`
  class for free.

* `ssl.lisp` redefines some items in the `CL+SSL` package (based on
  the `cl+ssl-20140316-git` version) and `tcp-ssl.lisp` overrides some
  items in the `CL-ASYNC-SSL` package (based on the
  `cl-async-20140616-git` version), mostly to add support for
  [NPN](https://technotes.googlecode.com/git/nextprotoneg.html),
  [SNI](http://en.wikipedia.org/wiki/Server_Name_Indication), and
  [DH parameters](http://en.wikipedia.org/wiki/Diffie%E2%80%93Hellman_key_exchange),
  all of which are neecessary to establish the TLS connection as
  required by HTTP/2. See the comments in those files for more
  information.

* `tcp.lisp` redefines some items in the `CL-ASYNC` package (based on
  the `cl-async-20140616-git` version) mostly to fix a couple issues
  encountered in corner cases.

* The code in the `example` folder is contained in `example.lisp` in
  the form of functions. The examples in Lisp fire up `CL-ASYNC` event
  loops.
  
* The Ruby code uses arrays and hashes which in the CL code are
  variously ported as alists, plists, and hashes depending on the
  specifics of access required.

* The error conditions are largely the same, but are prefixed with
  `HTTP2-` and an additional one is added named `HTTP2-NOT-STARTED` as
  it is very convenient in debugging (when NPN fails, etc). In
  addition, the Lisp code defines various restarts in the normal
  manner, so that during debugging certain failures do not require the
  instant transaction to be aborted entirely.

* Classes are matched one-to-one but module/package organization is
  different. Use the `HTTP2` package for most functions and the
  `HTTP2-EXAMPLE` package for the examples.

This Common Lisp code was produced by Martin Flack, a Principal
Architect on the Foundry team in the Web Experience business at
[Akamai](http://www.akamai.com/).

Our team's mission is innovative applied R&D, and accordingly we
explore new technologies close to the mission of excellent web
experience. Note that this code is intended to be used against the
other HTTP/2 interopability client/server code linked above, and not
necessarily any part of the Akamai network; nor is any feature herein
indicative of any planned or actual feature of Akamai products or
services.

This system was named `CL-HTTP2-PROTOCOL` to avoid misunderstanding
that it was related to `CL-HTTP`, a Common Lisp web server.
Apologies for the redundant word.

Server Setup and Example
------------

To run this on a fresh Ubuntu Linux 13.10 or 14.04 server, follow these
instructions. A non-root user of "ubuntu" with sudo access is assumed.

```shell
sudo apt-get update && sudo apt-get dist-upgrade -y && sudo reboot
# ...if prompted about grub choose "install package maintainer's version"
#
# ...login again
sudo apt-get install -y git sbcl
git clone https://github.com/akamai/cl-http2-protocol.git
wget http://beta.quicklisp.org/quicklisp.lisp
sbcl --script <<EOF
(load "quicklisp.lisp")
(quicklisp-quickstart:install)
(ql:quickload :swank)
(ql:quickload :alexandria)
(ql:quickload :anaphora)
(ql:quickload :babel)
(ql:quickload :puri)
(ql:quickload :usocket)
(ql:quickload :cl+ssl)
(ql:quickload :cl-async)
(ql:quickload :cl-async-ssl)
EOF
#
# ...optionally run screen if you're familiar with it and you want to
# quickly test server/client on one system:
screen
#
# ...start the server:
sbcl --script <<EOF
(load "quicklisp/setup.lisp")
(load "cl-http2-protocol/cl-http2-protocol.asd")
(require :cl-http2-protocol)
(in-package :http2-example)
(example-server)
EOF
#
# ...now you have an HTTP/2 server, secured with TLS, on port 8080
# (note the server is ONLY HTTP/2; there is no HTTP/1.1 fallback)
# (note that port 8080 is chosen only to allow a non-root user to
# run the Lisp image and launch the server)
#
# ...to stop the server, CTRL+C followed by ABORT at the handler
#
# ...if you prefer to have less output and keep exceptions from
# stopping the program, break the server, set these globals and rerun
# the server:
(setf *verbose-mode* nil)
(setf *debug-mode* nil)
(setf *dump-bytes* nil)
(example-server)
#
# ...if you ran screen, press CTRL-A CTRL-C, else open a new terminal
# to the server in order to run the example client
#
# ...run a client:
sbcl --script <<EOF
(load "quicklisp/setup.lisp")
(load "cl-http2-protocol/cl-http2-protocol.asd")
(require :cl-http2-protocol)
(in-package :http2-example)
(example-client "https://localhost:8080/")
EOF
```

Please note that the files `mykey.pem`, `mycert.pem`, and
`dhparams.2048.pem` are used for the example server, so it is
recommended that you regenerate them. You will then remove security
warnings from real browsers. Please note that `EXAMPLE-CLIENT` does
absolutely no sensible checking of the TLS certificate, etc.

```shell
openssl genrsa -out mykey.pem 2048
openssl req -new -key mykey.pem -out mycert.csr
# CN should be set to the hostname you will use to reach the server
openssl req -noout -text -in mycert.csr
openssl x509 -req -days 365 -in mycert.csr -signkey mykey.pem -out mycert.pem
openssl dhparam -outform pem -out dhparams.2048.pem 2048
```

At the time of writing (Sep 10, 2014), Firefox Nightly and Chrome
Canary will be compatible with your HTTP/2 server started above as
well. The address bar in Firefox Nightly should be
`https://HOST:8080/` where HOST is the hostname used to reach your
server.

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
basic examples that perform complete HTTP request/responses. These
functions use `CL+SSL` for secure connections, or `USOCKET` /
`SB-BSD-SOCKETS` for plain connections.

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
(on server :frame (lambda (bytes) ...))  ; encoded HTTP/2 frames

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
    <td>fires once for every encoded HTTP/2 frame that needs to be sent to the peer</td>
  </tr>
</table>

Stream lifecycle management
---------------------------

A single HTTP/2 connection can
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

```lisp
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

Flow control
-----------

Multiplexing multiple streams over the same TCP connection introduces
contention for shared bandwidth resources. Stream priorities can help
determine the relative order of delivery, but priorities alone are
insufficient to control how the resource allocation is performed
between multiple streams. To address this, HTTP/2 provides a simple
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

```lisp
(buffered-amount conn)      ; check amount of buffered data
(conn-window conn)          ; check current window size
(window-update conn 1024)   ; increment connection window by 1024 bytes

(buffered-amount stream)    ; check amount of buffered data
(stream-window stream)      ; check current window size
(window-update stream 2048) ; increment stream window by 2048 bytes
```

Server push
-----------

An HTTP/2 server can
[send multiple replies](http://chimera.labs.oreilly.com/books/1230000000545/ch12.html#HTTP2_PUSH)
to a single client request. To do so, first it emits a "push promise"
frame which contains the headers of the promised resource, followed by
the response to the original request, as well as promised resource
payloads (which may be interleaved). A simple example is in order:

```lisp
(defvar conn (make-instance 'server))

(on conn :stream
  (lambda (stream)
    (on stream :headers
	  (lambda (head) ... ))
    (on stream :data
	  (lambda (chunk) ... ))

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

```lisp
(defvar conn (make-instance 'client))
(on conn :promise
  (lambda (push)
    ; process push stream
	))
```

The client can cancel any given push stream (via `STREAM-CLOSE`), or
disable server push entirely by sending the appropriate settings frame
(note that below setting only impacts server > client direction):

```lisp
(settings client :streams 0)  ; setting max limit to 0 disables server push
```
