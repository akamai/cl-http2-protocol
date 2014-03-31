; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol)

; HTTP 2.0 server connection class that implements appropriate header
; compression / decompression algorithms and stream management logic.
;
; Your code is responsible for feeding request data to the server object,
; which in turn performs all of the necessary HTTP 2.0 decoding / encoding,
; state management, and the rest. See README.md for an example.

(defclass server (connection)
  ((stream-id :initform 2)
   (state :initform :new)
   (compressor :accessor compressor :initarg :compressor :initform (make-instance 'compressor :type :response))
   (decompressor :accessor decompressor :initarg :decompressor :initform (make-instance 'decompressor :type :request)))
  (:documentation "HTTP 2.0 server object"))

(defmethod promise ((server server) &rest args &aux (callback (shift args)))
  "Handle locally initiated server-push event emitted by the stream."
  (destructuring-bind (parent headers flags) args
    (let ((promise (new-stream server :parent parent)))
      (send promise (list :type :push-promise
			  :flags flags
			  :stream (id parent)
			  :promise-stream (id promise)
			  :payload headers))

      (funcall callback promise))))
