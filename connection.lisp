; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol)

(defparameter *default-flow-window* 65535
  "Default connection and stream flow control window (64KB)")

(defparameter *default-priority* (expt 2 30)
  "Default stream priority (lower values are higher priority)")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *connection-preface*
    (buffer-simple (concatenate 'string "PRI * HTTP/2.0" #1='(#\Return #\Linefeed) #1# "SM" #1# #1#))
    "Default connection \"fast-fail\" preamble string as defined by the spec"))

; Connection encapsulates all of the connection, stream, flow-control,
; error management, and other processing logic required for a well-behaved
; HTTP 2.0 endpoint.
;
; Note that this class should not be used directly. Instead, you want to
; use either Client or Server class to drive the HTTP 2.0 exchange.
;
(defclass connection (flowbuffer-include emitter-include error-include)
  ((state :reader conn-state :type (member :new :connection-header :connected :closed))
   (error :reader conn-error :initform nil)
   (window :reader conn-window :initarg :window :initform *default-flow-window* :type (or integer float))
   (stream-limit :reader conn-stream-limit :initarg :streams :initform 100 :type integer)
   (active-stream-count :reader conn-active-stream-count :initform 0 :type integer)
   (streams :initform (make-hash-table) :type hash-table)
   (framer :initform (make-instance 'framer) :type framer)
   (window-limit :initarg :window-limit :type (or integer float))
   (payload-limit :reader conn-payload-limit :initarg :payload-limit :initform *max-payload-size* :type integer)
   (headers-limit :reader conn-headers-limit :initarg :headers-limit :initform nil :type (or null integer))
   (recv-buffer :initform (make-instance 'buffer) :type buffer)
   (send-buffer :initform nil)
   (continuation :initform nil :type list)
   (stream-id :initform nil))
  (:documentation "Encapsulate connection, stream, flow-control, error management for an endpoint"))

(defmethod initialize-instance :after ((connection connection) &key)
  (setf (slot-value connection 'window-limit) (slot-value connection 'window)))

(defmethod shutdown-connection ((connection connection))
  (clear-stream-queues connection))

(defgeneric send (obj frame))

(defmethod new-stream ((connection connection) &optional (priority *default-priority*) (parent nil))
  "Allocates new stream for current connection."
  (with-slots (state active-stream-count stream-limit stream-id) connection
    (cond ((eq state :closed)                   (raise :http2-connection-closed))
	  ((= active-stream-count stream-limit) (raise :http2-stream-limit-exceeded))
	  (t (prog1
		 (activate-stream connection stream-id priority parent)
	       (incf stream-id 2))))))

(defmethod ping ((connection connection) payload blk)
  "Sends PING frame to the peer."
  (send connection (list :type :ping :stream 0 :payload payload))
  (if blk (once connection :pong blk)))

; Endpoints MAY append opaque data to the payload of any GOAWAY frame.
; Additional debug data is intended for diagnostic purposes only and
; carries no semantic value. Debug data MUST NOT be persistently stored,
; since it could contain sensitive information.
;
(defmethod goaway ((connection connection) &optional (error :no-error) (payload nil))
  "Sends a GOAWAY frame indicating that the peer should stop creating
new streams for current connection."
  (with-slots (streams) connection
    (let ((last-stream (or (loop for k being the hash-keys of streams maximize k) 0)))
      (send connection (list :type :goaway :last-stream last-stream
			     :error error :payload payload)))))

(defmethod restrict ((connection connection))
  "Issue ENHANCE_YOUR_CALM to peer."
  (goaway :error :enhance-your-calm))

(defmethod settings ((connection connection)
		     &optional
		       (stream-limit (slot-value connection 'stream-limit))
		       (window-limit (slot-value connection 'window-limit)))
  "Sends a connection SETTINGS frame to the peer."
  (send connection (list :type :settings :stream 0
			 :payload (list :settings-max-concurrent-streams stream-limit
					:settings-initial-window-size window-limit))))

; these have to appear here to compile (receive connection ...) properly
(defgeneric receive (obj data))
(defalias stream<< receive)

(defmethod receive ((connection connection) data)
  "Decodes incoming bytes into HTTP 2.0 frames and routes them to
appropriate receivers: connection frames are handled directly, and
stream frames are passed to appropriate stream objects."

  (handler-case-unless *debug-mode*
      (with-slots (state recv-buffer stream-limit window-limit continuation streams framer) connection
	(buffer<< recv-buffer data)

	;; Upon establishment of a TCP connection and determination that
	;; HTTP/2.0 will be used by both peers, each endpoint MUST send a
	;; connection header as a final confirmation and to establish the
	;; initial settings for the HTTP/2.0 connection.
	;; Client connection header is 24 byte connection header followed by
	;; SETTINGS frame. Server connection header is SETTINGS frame only.
	(when (eq state :new)
	  (if (< (buffer-size recv-buffer) #.(buffer-size *connection-preface*))
	      (if (buffer-mismatch recv-buffer
				   *connection-preface*
				   :end2 (buffer-size recv-buffer))
		  (raise :http2-handshake-error)
		  (return-from receive))
	      (if (buffer-mismatch (buffer-read recv-buffer #.(buffer-size *connection-preface*))
				   *connection-preface*
				   :end1 #.(buffer-size *connection-preface*))
		  (raise :http2-handshake-error)
		  (progn
		    (setf state :connection-header)
		    (settings connection stream-limit window-limit)))))

	(while-let (frame (parse framer recv-buffer))
	  (block this-frame

	    ;; Header blocks MUST be transmitted as a contiguous sequence of frames
	    ;; with no interleaved frames of any other type, or from any other stream.
	    (when continuation
	      (when (or (not (eq (getf frame :type) :continuation))
			(not (eq (getf frame :stream) (getf (first continuation) :stream))))
		(connection-error connection :msg "Expected CONTINUATION frame to follow frames ~S not ~S" continuation frame))

	      (push frame continuation)
	      (when (not (member :end-headers (getf frame :flags)))
		(return-from this-frame))

	      (let ((list (nreverse continuation)))
		(setf continuation nil)
		(let ((combined-buffer (reduce #'buffer<< list :key (lambda (f) (getf f :payload)))))
		  (setf frame (first list)
			(getf frame :payload) combined-buffer
			(getf frame :length) (buffer-size combined-buffer))
		  (push :end-headers (getf frame :flags)))))

	    ;; SETTINGS frames always apply to a connection, never a single stream.
	    ;; The stream identifier for a settings frame MUST be zero.  If an
	    ;; endpoint receives a SETTINGS frame whose stream identifier field is
	    ;; anything other than 0x0, the endpoint MUST respond with a connection
	    ;; error (Section 5.4.1) of type PROTOCOL_ERROR.
	    (if (connection-frame-p connection frame)
		(connection-management connection frame)
		(case (getf frame :type)
		  (:headers
		   ;; The last frame in a sequence of HEADERS/CONTINUATION
		   ;; frames MUST have the END_HEADERS flag set.
		   (when (not (member :end-headers (getf frame :flags)))
		     (push frame continuation)
		     (return-from this-frame))

		   ;; After sending a GOAWAY frame, the sender can discard frames
		   ;; for new streams.  However, any frames that alter connection
		   ;; state cannot be completely ignored.  For instance, HEADERS,
		   ;; PUSH_PROMISE and CONTINUATION frames MUST be minimally
		   ;; processed to ensure a consistent compression state
		   (decode-headers connection frame)
		   (when (eq state :closed)
		     (return-from this-frame))

		   (let ((stream (gethash (getf frame :stream) streams)))
		     (when (null stream)
		       (setf stream (activate-stream connection
						     (getf frame :stream)
						     (or (getf frame :weight) *default-priority*)))
		       (emit connection :stream stream))

		     (stream<< stream frame)))
		  (:push-promise
		   ;; The last frame in a sequence of PUSH_PROMISE/CONTINUATION
		   ;; frames MUST have the END_HEADERS flag set
		   (when (not (member :end-headers (getf frame :flags)))
		     (push frame continuation)
		     (return-from this-frame))
	     
		   (decode-headers connection frame)
		   (when (eq state :closed)
		     (return-from this-frame))
	     
		   ;; PUSH_PROMISE frames MUST be associated with an existing, peer-
		   ;; initiated stream... A receiver MUST treat the receipt of a
		   ;; PUSH_PROMISE on a stream that is neither "open" nor
		   ;; "half-closed (local)" as a connection error (Section 5.4.1) of
		   ;; type PROTOCOL_ERROR. Similarly, a receiver MUST treat the
		   ;; receipt of a PUSH_PROMISE that promises an illegal stream
		   ;; identifier (Section 5.1.1) (that is, an identifier for a stream
		   ;; that is not currently in the "idle" state) as a connection error
		   ;; (Section 5.4.1) of type PROTOCOL_ERROR, unless the receiver
		   ;; recently sent a RST_STREAM frame to cancel the associated stream.
		   (let ((parent (gethash (getf frame :stream) streams))
			 (pid (getf frame :promise-stream)))

		     (when (null parent)
		       (connection-error connection :msg "missing parent ID"))

		     (if (not (member (stream-state parent) '(:open :half-closed-local)))
			 ;; An endpoint might receive a PUSH_PROMISE frame after it sends
			 ;; RST_STREAM.  PUSH_PROMISE causes a stream to become "reserved".
			 ;; The RST_STREAM does not cancel any promised stream.  Therefore, if
			 ;; promised streams are not desired, a RST_STREAM can be used to
			 ;; close any of those streams.
			 (if (eq (stream-closed parent) :local-rst)
			     ;; We can either (a) 'resurrect' the parent, or (b) RST_STREAM
			     ;; ... sticking with (b), might need to revisit later.
			     (send connection (list :type :rst-stream :stream pid :error :refused-stream))
			     (connection-error connection)))

		     (let ((stream (activate-stream pid *default-priority* parent)))
		       (emit connection :promise stream)
		       (stream<< stream frame))))
		  (otherwise
		   (if-let (stream (gethash (getf frame :stream) streams))
		     (stream<< stream frame)
		     ;; An endpoint that receives an unexpected stream identifier
		     ;; MUST respond with a connection error of type PROTOCOL_ERROR.
		     (connection-error connection))))))))
    (t (e) (declare (ignore e)) (connection-error connection))))

(defalias connection<< receive)

(defmethod send ((connection connection) frame)
  "Send an outgoing frame. DATA frames are subject to connection flow
control and may be split and / or buffered based on current window size.
All other frames are sent immediately."
  (if (eq (getf frame :type) :data)
      (send-data connection frame)
      ;; An endpoint can end a connection at any time. In particular, an
      ;; endpoint MAY choose to treat a stream error as a connection error.
      (if (eq (getf frame :type) :rst-stream)
	  (when (eq (getf frame :error) :protocol-error)
	    (goaway connection (getf frame :error)))
	  (dolist (encoded-frame (encode connection frame))
	    (emit connection :frame encoded-frame)))))

(defmethod encode ((connection connection) frame)
  "Applies HTTP 2.0 binary encoding to the frame.
Returns a list of encoded frames since a frame may be chopped up for size."
  (with-slots (framer) connection
    (when (member (getf frame :type) '(:headers :push-promise))
      (list (encode-headers connection frame)))
    (mapcar (lambda (f) (generate framer f)) (maybe-downsize-frame connection frame))))

(defmethod connection-frame-p ((connection connection) frame)
  "Check if frame is a connection frame: SETTINGS, PING, GOAWAY, and any
frame addressed to stream ID = 0."
  (or (= (getf frame :stream) 0)
      (member (getf frame :type) '(:settings :ping :goaway))))

(defmethod connection-management ((connection connection) frame)
  "Process received connection frame (stream ID = 0).
- Handle SETTINGS updates
- Connection flow control (WINDOW_UPDATE)
- Emit PONG auto-reply to PING frames
- Mark connection as closed on GOAWAY"
  (with-slots (state window) connection
    (case state
      (:connection-header
       ; SETTINGS frames MUST be sent at the start of a connection.
       (connection-settings connection frame)
       (setf state :connected))

      (:connected
       (case (getf frame :type)
	 (:settings
	  (connection-settings connection frame))
	 (:window-update
	  (incf window (getf frame :increment))
	  (drain-send-buffer connection))
	 (:ping
	  (if (member :ack (getf frame :flags))
	      (emit connection :pong (getf frame :payload))
	      (send connection
		    (list :type :ping :stream 0 :flags (list :ack)
			  :payload (getf frame :payload)))))
	 (:goaway
	  ; Receivers of a GOAWAY frame MUST NOT open additional streams on
          ; the connection, although a new connection can be established
          ; for new streams.
	  (setf state :closed)
	  (emit connection :goaway (getf frame :last-stream) (getf frame :error) (getf frame :payload)))
	 (otherwise
	  (connection-error connection))))
      (otherwise
       (connection-error connection)))))

(defmethod connection-settings ((connection connection) frame)
  "Update local connection settings based on parameters set by the peer."
  (with-slots (stream-limit window window-limit streams) connection
    (when (or (not (eq (getf frame :type) :settings))
	      (/= (getf frame :stream) 0))
      (connection-error connection))
    (when (and (member :ack (getf frame :flags))
	       (getf frame :payload))
      (connection-error connection :type :frame-size-error))

    (when (getf frame :payload)
      (doplist (key value (getf frame :payload))
	(connection-setting connection key value))

      ; Bit 1 being set indicates that this frame acknowledges
      ; receipt and application of the peer's SETTINGS frame. When this
      ; bit is set, the payload of the SETTINGS frame MUST be empty.
      (send connection (list :type :settings :flags '(:ack) :payload nil)))))

(defmethod connection-setting ((connection connection) (key (eql :settings-header-table-size)) value)
  (with-slots (compressor) connection
    (setf (settings-limit compressor) value)))

;; this will be overridden for server class
(defmethod connection-setting ((connection connection) (key (eql :settings-enable-push)) value)
  (declare (ignore value))
  (connection-error :msg "SETTINGS_ENABLE_PUSH received, but connection is not a server."))

;; A change to SETTINGS_INITIAL_WINDOW_SIZE could cause the available
;; space in a flow control window to become negative. A sender MUST
;; track the negative flow control window, and MUST NOT send new flow
;; controlled frames until it receives WINDOW_UPDATE frames that cause
;; the flow control window to become positive.
(defmethod connection-setting ((connection connection) (key (eql :settings-initial-window-size)) value)
  (with-slots (window window-limit streams) connection
    (setf window (+ (- window window-limit) value))
    (dohash (id stream streams)
      (emit stream :window (+ (- (stream-window stream) window-limit) value)))
    (setf window-limit value)))

(defmethod connection-setting ((connection connection) (key (eql :settings-max-concurrent-streams)) value)
  (with-slots (stream-limit) connection
    (setf stream-limit value)))

(defmethod connection-setting ((connection connection) (key (eql :settings-max-frame-size)) value)
  (with-slots (payload-limit) connection
    (setf payload-limit value)))

(defmethod connection-setting ((connection connection) (key (eql :settings-max-header-list-size)) value)
  (with-slots (headers-limit) connection
    (setf headers-limit value)))

(defmethod maybe-downsize-frame ((connection connection) frame)
  (with-slots (payload-limit) connection
    (let (frames
	  (payload (getf frame :payload)))
      (when (bufferp payload)
	(let ((max payload-limit))
	  (while (> (buffer-size payload) max)
	    (let ((new-frame (copy-list frame))
		  (chunk (buffer-slice! payload 0 max)))
	      (when (member (getf frame :type) '(:headers :push-promise))
		(setf (getf new-frame :type) :continuation))
	      (setf (getf new-frame :payload) chunk)
	      (setf (getf new-frame :length) (buffer-size chunk))
	      (setf (getf new-frame :flags) (remove-if (lambda (f) (member f '(:end-stream :end-headers))) (getf frame :flags)))
	      (push new-frame frames)))))
      (if frames
	  (progn
	    (setf (getf frame :length) (buffer-size payload))
	    (push frame frames)
	    (nreverse frames))
	  (list frame)))))

(defmethod check-headers-size ((connection connection) headers)
  (with-slots (headers-limit compressor) connection
    (when headers-limit
      (when (> (headers-size compressor headers) headers-limit)
	(raise :http2-headers-too-big "Header set is larger than peer's SETTINGS_MAX_HEADER_LIST_SIZE.")))))

(defmethod decode-headers ((connection connection) frame)
  "Decode headers payload and update connection decompressor state.
The receiver endpoint reassembles the header block by concatenating
the individual fragments, then decompresses the block to reconstruct
the header set - aka, header payloads are buffered until END_HEADERS,
or an END_PROMISE flag is seen."
  (handler-case-unless *debug-mode*
      (progn
	(with-slots (decompressor) connection
	  (when (bufferp (getf frame :payload))
	    (setf (getf frame :payload) (postprocess decompressor (decode decompressor (getf frame :payload)))))))
    (t (e) (connection-error connection :type :compression-error :msg e))))

(defmethod encode-headers ((connection connection) frame)
  "Encode headers payload and update connection compressor state."
  (handler-case-unless *debug-mode*
      (with-slots (compressor) connection
	(when (not (bufferp (getf frame :payload)))
	  (setf (getf frame :payload) (encode compressor (preprocess compressor (getf frame :payload))))))
    (t (e) (connection-error connection :type :compression-error :msg e))))

(defmethod activate-stream ((connection connection) id priority &optional parent)
  "Activates new incoming or outgoing stream and registers appropriate
connection management callbacks."
  (with-slots (streams window-limit active-stream-count) connection
    (when (gethash id streams)
      (connection-error connection :msg "Stream ID already exists"))

    (let ((stream (make-instance 'stream :id id :priority priority
				 :window window-limit :parent parent
				 :connection connection)))

      ; Streams that are in the "open" state, or either of the "half closed"
      ; states count toward the maximum number of streams that an endpoint is
      ; permitted to open.
      (once stream :active (lambda-ignore (incf active-stream-count)))
      (once stream :close  (lambda-ignore (decf active-stream-count)))
      (when (typep connection 'server)
	(on stream :promise (lambda-apply (server-promise connection))))
      (on stream :frame (lambda-apply (send connection)))

      (setf (gethash id streams) stream))))

(defmethod pump-stream-queues ((connection connection) n)
  (with-slots (streams) connection
    (loop
       with pending = (the boolean nil)  ; will set to t if any streams have queues or dependencies left at end
       with pump-these = (make-array 16 :element-type 'stream :fill-pointer 0)  ; entertain 16 streams at max in one pass
       for stream being the hash-values of streams
       when (queue-populated-p stream)
       if (stream-dependency stream)
       do (setf pending t)
       else
       do (or (vector-push stream pump-these) (loop-finish))
       and minimize (stream-priority stream) into lowest-weight
       finally (progn
		 (loop
		    with lowest-weight/2 = (max (ash lowest-weight -1) 1)
		    for stream across pump-these
		    do (progn
			 ;; send 2 frames for lowest weight and multiples thereupon
			 (pump-queue stream (round (/ (stream-priority stream) lowest-weight/2)))
			 ;; remove this stream as dependent stream on all other streams if done
			 (if (queue-populated-p stream)
			     (setf pending t)
			     (loop
				for other-stream being the hash-values of streams
				when (eq (stream-dependency other-stream) stream)
				do (setf (stream-dependency other-stream) nil)))))
		 (return pending)))))

(defmethod clear-stream-queues ((connection connection))
  (with-slots (streams) connection
    (dohash (key stream streams)
      (when (queue-populated-p stream)
	(clear-queue stream)))))

;; DRAIN-SEND-BUFFER is a FLOW-BUFFER method, but enforce ENCODE when used
;; on CONNECTION subclass, as we want encoding when calling from here
(defmethod drain-send-buffer :around ((obj connection) &optional encode)
  (declare (ignore encode))
  (call-next-method obj t))

(defmethod connection-error ((connection connection)
			     &key (type :protocol-error) (msg "Connection error"))
  "Emit GOAWAY error indicating to peer that the connection is being
aborted, and once sent, raise a local exception."
  (with-slots (state error) connection
    (when (and (not (eq state :closed)) (not (eq state :new)))
      (goaway connection type))
    
    (setf state :closed
	  error type)

    (when (not (stringp msg))
      (setf msg (apply #'format nil
		       (simple-condition-format-control msg)
		       (simple-condition-format-arguments msg))))

    (raise (find-symbol (concatenate 'string "HTTP2-" (symbol-name type))) msg)))
