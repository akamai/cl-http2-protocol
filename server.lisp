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
   (decompressor :accessor decompressor :initarg :decompressor :initform (make-instance 'decompressor :type :request))
   (push-enabled :reader push-enabled :initform t :type (member t nil)))
  (:documentation "HTTP 2.0 server object"))

; Technically HTTP/2.0 as a protocol does not differentiate push
; between client/server, and the protocol can do it either way; but
; HTTP semantics applied to the protocol dictate that only a server
; pushes (it makes no sense for a client to push).

; a SERVER knows what to do with SETTINGS_ENABLE_PUSH so define a method
(defmethod connection-setting ((connection server) (key (eql :settings-enable-push)) value)
  (with-slots (push-enabled) connection
    (setf push-enabled (if (= value 1) t nil))))

(defmethod server-promise ((server server) &rest args &aux (callback (shift args)))
  "Handle locally initiated server-push event emitted by the stream."
  (destructuring-bind (parent headers flags) args
    (when (with-simple-restart (abort-promise "Abort the promise")
	    (unless (push-enabled server)
	      (raise :http2-push-disabled "Push disabled, cannot promise: ~A" headers))
	    t)
    
      (let ((promise (new-stream server *default-priority* parent)))
	(send promise (list :type :push-promise
			    :flags flags
			    :stream (stream-id parent)
			    :promise-stream (stream-id promise)
			    :payload headers))

	(funcall callback promise)))))

; Again, it only makes sense for a server to emit an ALTSVC frame in
; normal HTTP operations:

(defmethod altsvc ((server server) max-age port protocol-identifier host origin)
  "Issue ALTSVC frame to peer."
  (send server (list :type :altsvc :max-age max-age :port port
		     :protocol-identifier protocol-identifier :host host :origin origin)))
