; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-user)

; (declaim (optimize (speed 0) (space 0) (debug 3) (compilation-speed 0)))

(defpackage :cl-http2-protocol-util
  (:nicknames :http2-util)
  (:documentation "Basic utilities.")
  (:use :cl :alexandria :anaphora :babel)
  (:export #:defalias #:shift #:unshift #:while #:while-let
	   #:ensuref #:deletef-if #:to-sym #:string-to-bytes
	   #:flatten-n #:reverse-plist #:dohash
	   #:+infinity #:-infinity
	   #:lambda-ignore #:lambda-apply
	   #:pack #:unpack #:split-if
	   #:*debug-mode* #:*debug-stream*
	   #:handler-case-unless
	   #:report-error))

(defpackage :cl-http2-protocol
  (:nicknames :http2)
  (:documentation "HTTP/2.0 draft-09/hpack-05 implementation.")
  (:use :cl :alexandria :anaphora :babel :http2-util)
  (:shadow #:stream #:stream-error)
  (:export #:http2-error #:http2-not-started #:http2-handshake-error
	   #:http2-protocol-error #:http2-compression-error
	   #:http2-header-exception #:http2-flow-control-error
	   #:http2-stream-error #:http2-stream-closed
	   #:http2-connection-closed #:http2-stream-limit-exceeded
	   #:vector-concat #:vector-prepend #:vector-splice
	   #:vector-delete #:vector-delete-at #:vector-slice
	   #:vector-inspect #:make-data-vector
	   #:buffer #:buffer-data #:buffer-empty-p #:buffer-adjust
	   #:buffer<< #:buffer-simple #:buffer-prepend
	   #:buffer-firstbyte #:buffer-firstchar #:buffer-getbyte
	   #:buffer-readbyte #:buffer-setbyte #:buffer-size
	   #:buffer-read #:buffer-delete-section #:buffer-slice
	   #:buffer-slice! #:buffer-read-uint32 #:buffer-mismatch
	   #:buffer-string #:buffer-ascii #:buffer-inspect
	   #:connection #:conn-state #:conn-error #:conn-window
	   #:conn-stream-limit #:conn-active-stream-count
	   #:connection<< #:new-stream
	   #:client #:server
	   #:on #:once #:emit
	   #:stream-id #:stream-priority #:stream-window
	   #:stream-parent #:stream-state #:stream-closed
	   #:stream-error-type #:stream<<
	   #:ping #:goaway #:settings
	   #:headers #:data #:promise #:reprioritize
	   #:stream-close #:cancel #:refuse))

(defpackage :cl-http2-protocol-example
  (:nicknames :http2-example)
  (:documentation "HTTP/2.0 draft-09/hpack-05 simple example client/server.")
  (:use :cl :alexandria :anaphora :babel :puri :usocket :cl+ssl :http2-util :http2)
  (:export #:example-client #:example-server
	   #:*dump-bytes* #:*dump-bytes-stream* #:*dump-bytes-hook*))
