(in-package :cl-http2-protocol)

(defclass error-include () ()
  (:documentation "Stream, connection, and compressor exceptions."))

(define-condition http2-error (simple-error) ())

(define-condition http2-not-started (http2-error)
  ((other-protocol :initarg :other-protocol :accessor other-protocol))
  (:documentation "Raised if one side of the connection cannot do HTTP 2.0, usually
due to NPN not resolving an HTTP 2.0 outcome (or different drafts of HTTP 2.0)."))

(define-condition http2-handshake-error (http2-error) ()
  (:documentation "Raised if connection header is missing or invalid indicating that
this is an invalid HTTP 2.0 request - no frames are emitted and the
connection must be aborted."))

(define-condition http2-protocol-error (http2-error) ()
  (:documentation "Raised by stream or connection handlers, results in GOAWAY frame
which signals termination of the current connection. You *cannot*
recover from this exception, or any exceptions subclassed from it."))

(define-condition http2-compression-error (http2-protocol-error) ()
  (:documentation "Raised on any header encoding / decoding exception."))

(define-condition http2-header-exception (http2-protocol-error) ()
  (:documentation "Raised on invalid reference for current compression context: the
client and server contexts are out of sync."))

(define-condition http2-flow-control-error (http2-protocol-error) ()
  (:documentation "Raised on invalid flow control frame or command."))

(define-condition http2-stream-error (http2-protocol-error) ()
  (:documentation "Raised on invalid stream processing: invalid frame type received or
sent, or invalid command issued."))

; recoverable errors

(define-condition http2-stream-closed (http2-error) ()
  (:documentation "Raised if stream has been closed and new frames cannot be sent."))

(define-condition http2-connection-closed (http2-error) ()
  (:documentation "Raised if connection has been closed (or draining) and new stream
cannot be opened."))

(define-condition http2-stream-limit-exceeded (http2-error) ()
  (:documentation "Raised if stream limit has been reached and new stream cannot be opened."))
