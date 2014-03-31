(in-package :cl-user)

(defpackage :cl-http2-protocol
  (:nicknames :http2)
  (:documentation "HTTP/2.0 draft-06 implementation.")
  (:use :cl :alexandria :babel)
  (:shadow #:stream #:stream-error)
  (:export #:client #:server
	   #:on #:once #:emit
	   #:headers #:data #:promise
	   #:http2-error #:http2-not-started #:http2-handshake-error
	   #:http2-protocol-error #:http2-compression-error
	   #:http2-header-exception #:http2-flow-control-error
	   #:http2-stream-error #:http2-stream-closed
	   #:http2-connection-closed #:http2-stream-limit-exceeded
	   #:example-client #:example-server))

(defpackage :cl-http2-protocol-example
  (:nicknames :http2-example)
  (:documentation "HTTP/2.0 simple example client/server.")
  (:use :cl :alexandria :babel :puri :usocket :cl+ssl)
  (:export #:example-client #:example-server))
