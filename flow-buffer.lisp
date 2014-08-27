; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol)

(defclass flowbuffer-include ()
  ((send-buffer :accessor send-buffer :initarg :send-buffer :initform nil))
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

(defmethod send-data ((obj flowbuffer-include) &optional frame)
  "Buffers outgoing DATA frames and applies flow control logic to split
and emit DATA frames based on current flow control window. If the
window is large enough, the data is sent immediately. Otherwise, the
data is buffered until the flow control window is updated.
Buffered DATA frames are emitted in FIFO order."
  (with-slots (send-buffer window) obj
    (when frame
      (check-type frame (satisfies framep))
      (push frame send-buffer))
    (drain-send-buffer obj)))

(defmethod drain-send-buffer ((obj flowbuffer-include) &optional encode)
  "Buffers outgoing DATA frames and applies flow control logic to split
and emit DATA frames based on current flow control window. If the
window is large enough, the data is sent immediately. Otherwise, the
data is buffered until the flow control window is updated.
Buffered DATA frames are emitted in FIFO order."
  (with-slots (send-buffer window) obj
    (while (and (plusp window) send-buffer)
      (let ((frame (shift send-buffer)))
	(check-type frame (satisfies framep))
	(let ((frame-size (buffer-size (getf frame :payload)))
	      (sent 0))
	  (if (> frame-size window)
	      (let* ((payload (prog1 (getf frame :payload)
				(remf frame :payload)))
		     (chunk (copy-tree frame)))
		(setf (getf frame :payload) (buffer-slice! payload 0 window))
		(setf (getf chunk :length) (buffer-size payload))
		(setf (getf chunk :payload) payload)
		(deletef (getf frame :flags) :end-stream)
		(unshift chunk send-buffer)
		(setf sent window))
	      (setf sent frame-size))
	  (when encode
	    (let ((frames (encode obj frame)))
	      (setf frame (pop frames))
	      (dolist (frame* (nreverse frames))
		(unshift frame* send-buffer))))
	  (emit obj :frame frame)
	  (decf window sent))))))
