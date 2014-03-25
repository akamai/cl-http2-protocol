(in-package :cl-user)

(defpackage :cl-http2
  (:nicknames :http2)
  (:documentation "HTTP/2.0 draft-06 implementation.")
  (:use :cl :alexandria :puri :usocket :cl+ssl :trivial-gray-streams)
  (:shadow #:stream #:stream-error)
  (:export #:do-client #:do-server))
