(in-package :http2)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0) (compilation-speed 0)))

(defparameter *max-payload-size* (1- (expt 2 16))
  "Maximum frame size (65535 bytes)")

(defparameter *max-stream-id* #x7FFFFFFF
  "Maximum stream ID (2^31)")

(defparameter *max-windowinc* #x7FFFFFFF
  "Maximum window increment value (2^31)")

(defparameter *frame-types* '(:data          #x0
			      :headers       #x1
			      :priority      #x2
			      :rst-stream    #x3
			      :settings      #x4
			      :push-promise  #x5
			      :ping          #x6
			      :goaway        #x7
			      :window-update #x9
			      :continuation  #xA))

(defparameter *frame-flags* '(:data          (:end-stream 0 :reserved 1)
			      :headers       (:end-stream 0 :reserved 1
					      :end-headers 2 :priority 3)
			      :priority      ()
			      :rst-stream    ()
			      :settings      ()
			      :push-promise  (:end-push-promise 0)
			      :ping          (:pong 0)
			      :goaway        ()
			      :window-update ()
			      :continuation  (:end-stream 0 :end-headers 1)))

(defparameter *defined-settings* '(:settings-max-concurrent-streams 4
				   :settings-initial-window-size    7
				   :settings-flow-control-options   10))

(defparameter *defined-errors* '(:no-error           0
				 :protocol-error     1
				 :internal-error     2
				 :flow-control-error 3
				 :http-stream-closed 5
				 :frame-too-large    6
				 :refused-stream     7
				 :cancel             8
				 :compression-error  9))

(defparameter *rbit*  #x7FFFFFFF)
(defparameter *rbyte* #x0FFFFFFF)
(defparameter *headerpack* "nCCN")
(defparameter *uint32* "N")

(defclass framer () ())

(defmethod common-header ((framer framer) frame)
  "Generates common 8-byte frame header.
- http://tools.ietf.org/html/draft-ietf-httpbis-http2-04#section-4.1"
  (let (header)

    (when (not (getf *frame-types* (getf frame :type)))
      (raise 'http2-compression-error "Invalid frame type (~A)" (getf frame :type)))

    (when (> (getf frame :length) *max-payload-size*)
      (raise 'http2-compression-error "Frame size is too large: ~D" (getf frame :length)))

    (when (> (getf frame :stream) *max-stream-id*)
      (raise 'http2-compression-error "Stream ID (~A) is too large" (getf frame :stream)))

    (when (and (eq (getf frame :type) :window-update)
	       (> (getf frame :increment) *max-windowinc*))
      (raise 'http2-compression-error "Window increment (~D) is too large" (getf frame :increment)))

    (push (getf frame :length) header)
    (push (getf *frame-types* (getf frame :type)) header)
    (push (reduce (lambda (acc f)
		    (let ((position (getf (getf *frame-flags* (getf frame :type)) f)))
		      (when (null position)
			(raise 'http2-compression-error "Invalid frame flag (~A) for ~A" f (getf frame :type)))
		      (setf (ldb (byte 1 position) acc) 1)
		      acc))
		  (getf frame :flags) :initial-value 0) header)

    (push (getf frame :stream) header)
    (pack *headerpack* (nreverse header))))

(defmethod read-common-header ((framer framer) (buf buffer))
  (let (frame)
    (destructuring-bind (flength type flags stream)
	(unpack *headerpack* (buffer-data (buffer-slice buf 0 8)))
      (setf (getf frame :length) flength)

      (setf (getf frame :type)
	    (loop for (ft pos) on *frame-types* by #'cddr if (= type pos) return ft))
      (setf (getf frame :flags)
	    (loop for (name pos) on (getf *frame-flags* (getf frame :type)) by #'cddr
	          if (logbitp pos flags) collect name))

      (setf (getf frame :stream) (logand stream *rbit*)))
    frame))

(defmethod generate ((framer framer) frame)
  "Generates encoded HTTP 2.0 frame.
- http://tools.ietf.org/html/draft-ietf-httpbis-http2"
  (let ((bytes (make-instance 'buffer))
	(length 0))

    (ensuref (getf frame :flags) nil)
    (ensuref (getf frame :stream) 0)
    
    (case (getf frame :type)
      (:data
       (buffer<< bytes (getf frame :payload))
       (incf length (buffer-size (getf frame :payload))))

      (:headers
       (when-let (priority (getf frame :priority))
	 (when (not (member :priority (getf frame :flags)))
	   (appendf (getf frame :flags) (list :priority))))

       (when (member :priority (getf frame :flags))
	 (buffer<< bytes (pack *uint32* (list (logand (getf frame :priority) *rbit*))))
	 (incf length 4))

       (buffer<< bytes (getf frame :payload))
       (incf length (buffer-size (getf frame :payload))))

      (:priority
       (buffer<< bytes (pack *uint32* (list (logand (getf frame :priority) *rbit*))))
       (incf length 4))

      (:rst-stream
       (buffer<< bytes (pack-error (getf frame :error)))
       (incf length 4))
      
      (:settings
       (when (/= (getf frame :stream) 0)
	 (raise 'http2-compression-error "Invalid stream ID (~A)" (getf frame :stream)))

       (doplist (k v (getf frame :payload))
	 (when (not (integerp k))
	   (setf k (getf *defined-settings* k))
	   
	   (when (null k)
	     (raise 'http2-compression-error "Unknown settings ID for ~A" k)))
	 
	 (buffer<< bytes (pack *uint32* (list (logand k *rbyte*))))
	 (buffer<< bytes (pack *uint32* (list v)))
	 (incf length 8)))

      (:push-promise
       (buffer<< bytes (pack *uint32* (list (logand (getf frame :promise-stream) *rbit*))))
       (buffer<< bytes (buffer-data (getf frame :payload)))
       (incf length (+ 4 (buffer-size (getf frame :payload)))))

      (:ping
       (when (/= (buffer-size (getf frame :payload)) 8)
	 (raise 'http2-compression-error "Invalid payload size (~D != 8 bytes)"
		(buffer-size (getf frame :payload))))

       (buffer<< bytes (getf frame :payload))
       (incf length 8))

      (:goaway
       (buffer<< bytes (pack *uint32* (list (logand (getf frame :last-stream)) *rbit*)))
       (buffer<< bytes (pack-error (getf frame :error)))
       (incf length 8))

      (:window-update
       (buffer<< bytes (pack *uint32* (list (getf frame :increment))))
       (incf length 4))

      (:continuation
       (buffer<< bytes (getf frame :payload))
       (incf length (buffer-size (getf frame :payload)))))

    (setf (getf frame :length) length)
    (buffer-prepend bytes (common-header framer frame))))

(defmethod parse ((framer framer) (buf buffer))
  "Decodes complete HTTP 2.0 frame from provided buffer. If the buffer
does not contain enough data, no further work is performed."
  (when (< (buffer-size buf) 8)
    (return-from parse nil))
  (let ((frame (read-common-header framer buf)))
    (when (< (buffer-size buf) (+ 8 (getf frame :length)))
      (return-from parse nil))

    (buffer-read buf 8)
    (let ((payload (buffer-read buf (getf frame :length))))

      (case (getf frame :type)
	(:data
	 (setf (getf frame :payload) (buffer-read payload (getf frame :length))))
	(:headers
	 (let ((size (getf frame :length)))
	   (when (member :priority (getf frame :flags))
	     (setf (getf frame :priority) (logand (buffer-read-uint32 payload) *rbit*))
	     (decf size 4))
	   (setf (getf frame :payload) (buffer-read payload size))))
	(:priority
	 (setf (getf frame :priority) (logand (buffer-read-uint32 payload) *rbit*)))
	(:rst-stream
	 (getf frame :error (unpack-error (buffer-read-uint32 payload))))

	(:settings
	 (setf (getf frame :payload) nil)
	 (loop
	    :repeat (/ (getf frame :length) 8)
	    :for id = (logand (buffer-read-uint32 payload) *rbyte*)
	    :for val = (buffer-read-uint32 payload)
	    ; Unsupported or unrecognized settings MUST be ignored.
	    :do (when-let (name (loop for (name v) on *defined-settings* by #'cddr
				      if (= v id) return name))
		  (setf (getf (getf frame :payload) name) val))))
	(:push-promise
	 (setf (getf frame :promise-stream) (logand (buffer-read-uint32 payload) *rbit*))
	 (setf (getf frame :payload) (buffer-read payload (- (getf frame :length) 4))))
	(:ping
	 (setf (getf frame :payload) (buffer-read payload (getf frame :length))))
	(:goaway
	 (setf (getf frame :last-stream) (logand (buffer-read-uint32 payload) *rbit*))
	 (setf (getf frame :error) (unpack-error (buffer-read-uint32 payload)))

	 (let ((size (- (getf frame :length) 8)))
	   (when (plusp size)
	     (setf (getf frame :payload) (buffer-read payload size)))))
	(:window-update
	 (setf (getf frame :increment) (logand (buffer-read-uint32 payload) *rbit*)))
	(:continuation
	 (setf (getf frame :payload) (buffer-read payload (getf frame :length))))))
    frame))

(defun pack-error (e)
  (when (not (integerp e))
    (if-let (d (getf *defined-errors* e))
      (setf e d)
      (raise 'http2-compression-error "Unknown error ID for ~A" e))

  (pack *uint32* (list e))))

(defun unpack-error (e)
  (or (loop for (name v) on *defined-errors* by #'cddr if (= v e) return name) e))
