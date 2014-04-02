; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol)

(defparameter *max-frame-size* (1- (expt 2 14))
  "Maximum size of a DATA payload (16383 bytes, ~16K).")

(defclass flowbuffer-include ()
  ((send-buffer :accessor send-buffer :initarg :send-buffer))
  (:documentation "Implementation of stream and connection DATA flow control: frames may
be split and / or may be buffered based on current flow control window."))

(defmethod buffered-amount ((obj flowbuffer-include))
  "Amount of buffered data. Only DATA payloads are subject to flow stream
and connection flow control."
  (reduce #'+ (mapcar (lambda (f) (getf f :length)) (send-buffer obj))))

(defmethod window-update ((obj flowbuffer-include) increment)
  "Increment window"
  (with-slots (window) obj
    (incf window increment)
    (emit obj :window-update increment)))

(defgeneric encode (obj frame))
(defgeneric emit (obj obj &rest args))

(defmethod send-data ((obj flowbuffer-include) &optional frame encode)
  "Buffers outgoing DATA frames and applies flow control logic to split
and emit DATA frames based on current flow control window. If the
window is large enough, the data is sent immediately. Otherwise, the
data is buffered until the flow control window is updated.
Buffered DATA frames are emitted in FIFO order."
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
