(in-package :http2)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0) (compilation-speed 0)))

(defclass server (connection)
  ((stream-id :initform 2)
   (state :initform :new)
   (compressor :accessor compressor :initarg :compressor :initform (make-instance 'compressor :type :response))
   (decompressor :accessor decompressor :initarg :decompressor :initform (make-instance 'decompressor :type :request))))

(defmethod promise ((server server) &rest args &aux (callback (shift args)))
  (destructuring-bind (parent headers flags) args
    (let ((promise (new-stream server :parent parent)))
      (send promise (list :type :push-promise
			  :flags flags
			  :stream (id parent)
			  :promise-stream (id promise)
			  :payload headers))

      (funcall callback promise))))
