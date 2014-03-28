(in-package :http2)

(defclass client (connection)
  ((stream-id :initform 1)
   (state :initform :connection-header)
   (compressor   :accessor client-compressor :initarg :compressor
	         :initform (make-instance 'compressor :type :request))
   (decompressor :accessor client-decompressor :initarg :decompressor
		 :initform (make-instance 'decompressor :type :response)))
  (:documentation "HTTP 2.0 client object"))

(defmethod send :before ((client client) frame)
  (with-slots (state stream-limit window-limit) client
    (when (eq state :connection-header)
      (emit client :frame *connection-header*)
      (setf state :connected)
      (settings client stream-limit window-limit))))
