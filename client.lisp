; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol)

; HTTP/2 client connection class that implements appropriate header
; compression / decompression algorithms and stream management logic.
;
; Your code is responsible for driving the client object, which in turn
; performs all of the necessary HTTP/2 encoding / decoding, state
; management, and the rest. See README.md for an example.

(defclass client (connection)
  ((stream-id :initform 1)
   (state :initform :connection-header)
   (compressor   :accessor client-compressor :initarg :compressor
	         :initform (make-instance 'compressor :type :request))
   (decompressor :accessor client-decompressor :initarg :decompressor
		 :initform (make-instance 'decompressor :type :response)))
  (:documentation "HTTP/2 client object"))

(defmethod send :before ((client client) frame)
  "Send an outgoing frame. Connection and stream flow control is managed by CONNECTION class."
  (with-slots (state stream-limit window-limit) client
    (when (eq state :connection-header)
      (emit client :frame *connection-preface*)
      (setf state :connected)
      (settings client stream-limit window-limit))))
