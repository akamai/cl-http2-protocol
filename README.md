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
CL+SSL and OpenSSL. On SBCL, an option is available for unencrypted
(a.k.a. "plain" or "direct") communication using SB-BSD-SOCKETS, but
this is offered as a convenience only, as HTTP/2.0 will enforce TLS.

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
  build up various primitives that Ruby offers in the String class for
  free.

* ssl.lisp redefines some items in the CL+SSL package in order to
  allow SSL communication for a variable-length frame binary protocol,
  as well as adding support for wrapping NPN support from OpenSSL. See
  comments in that file for more information.

* net.lisp provides a NET class that abstracts networking sufficiently
  to allow various approaches (CL+SSL, USOCKET albeit not in the most
  efficient manner due to reading variable-sized binary sequences,
  SB-BSD-SOCKETS on SBCL which is more efficient).

* The code in the example folder is contained in example.lisp in
  the form of functions.
  
* The Ruby code uses arrays and hashes which in the CL code are
  variously ported as alists, plists, and hashes depending on the
  specifics of access required.

* The error conditions are largely the same, but are prefixed with
  HTTP2- and an additional one is added named HTTP2-NOT-STARTED as
  it is very convenient in debugging (when NPN fails, etc).

* Classes are matched one-to-one but module/package organization is
  different. Use the HTTP2 package for most functions and the
  HTTP2-EXAMPLE package for the examples.

This Common Lisp code was produced by Martin Flack, a Principal
Architect on the Foundry team in the Web Experience business at
Akamai. Our team's mission is innovative applied R&D, and accordingly
we explore new technologies close to the mission of excellent web
experience. Note that this code is intended to be used against the
other HTTP/2.0 interopability client/server code linked above, and not
necessarily any part of the Akamai network.

Server Setup
------------

To run this on a fresh Ubuntu Linux 13.10 server, follow these
instructions. A non-root user of "ubuntu" with sudo access is assumed.

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
	

Getting Started
---------------

    (load "cl-http2-protocol.asd")
    (require :cl-http2-protocol)

    ; provide a transport
    (defvar socket ...)

    (defvar conn (make-instance 'client))
    (on conn :frame (lambda (bytes) (socket-send socket bytes)) ; send bytes somehow
    (loop for bytes = (socket-read socket)
          if bytes (connection<< conn bytes) else (return))
