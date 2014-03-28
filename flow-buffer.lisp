(in-package :http2)

(defparameter *max-frame-size* (1- (expt 2 14)))

(defclass flowbuffer-include ()
  ((send-buffer :accessor send-buffer :initarg :send-buffer)))

(defmethod buffered-amount ((obj flowbuffer-include))
  (reduce #'+ (mapcar (lambda (f) (getf f :length)) (send-buffer obj))))

(defgeneric encode (obj frame))
(defgeneric emit (obj obj &rest args))

(defmethod send-data ((obj flowbuffer-include) &optional frame encode)
  (with-slots (send-buffer window) obj
    (when frame
      (push frame send-buffer))
    (while (and (plusp window) send-buffer)
      (let ((frame (shift send-buffer))
	    (sent 0)
	    (frame-size (buffer-size (getf frame :payload))))
	(if (> frame-size window)
	    (let* ((payload (remf frame :payload))
		   (chunk (copy-list frame))) ; dup***
	      (setf (getf frame :payload) (buffer-slice! payload 0 window))
	      (setf (getf chunk :length) (buffer-size payload))
	      (setf (getf chunk :payload) payload)
	      (deletef (getf frame :flags) :end-stream)
	      (unshift send-buffer chunk)
	      (setf sent window))
	    (setf sent frame-size))
	(when encode
	  (setf frame (encode obj frame)))
	(emit obj :frame frame)
	(decf window sent)))))
