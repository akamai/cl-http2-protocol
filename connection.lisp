(in-package :cl-http2-protocol)

(defparameter *default-flow-window* 65535
  "Default connection and stream flow control window (64KB)")

(defparameter *default-priority* (expt 2 30)
  "Default stream priority (lower values are higher priority)")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *connection-header*
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
   (recv-buffer :initform (make-instance 'buffer) :type buffer)
   (send-buffer :initform nil)
   (continuation :initform nil :type list)
   (stream-id :initform nil))
  (:documentation "Encapsulate connection, stream, flow-control, error management for an endpoint"))

(defmethod initialize-instance :after ((connection connection) &key)
  (setf (slot-value connection 'window-limit) (slot-value connection 'window)))

(defgeneric send (obj frame))

(defmethod new-stream ((connection connection) &optional (priority *default-priority*) (parent nil))
  "Allocates new stream for current connection."
  (with-slots (state active-stream-count stream-limit stream-id) connection
    (cond ((eq state :closed)                   (raise 'http2-connection-closed))
	  ((= active-stream-count stream-limit) (raise 'http2-stream-limit-exceeded))
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

(defmethod settings ((connection connection) &optional
		     (stream-limit (slot-value connection 'stream-limit))
		     (window-limit (slot-value connection 'window-limit)))
  "Sends a connection SETTINGS frame to the peer. Setting window size
to +INFINITY disables flow control."
  (with-slots (window) connection
    (let ((payload (list :settings-max-concurrent-streams stream-limit)))
      (if (eql window +infinity)
	  (setf (getf payload :settings-flow-control-options) 1)
	  (setf (getf payload :settings-initial-window-size) window-limit))
      (send connection (list :type :settings :stream 0 :payload (reverse-plist payload))))))

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

	; Upon establishment of a TCP connection and determination that
	; HTTP/2.0 will be used by both peers, each endpoint MUST send a
	; connection header as a final confirmation and to establish the
	; initial settings for the HTTP/2.0 connection.
	; Client connection header is 24 byte connection header followed by
	; SETTINGS frame. Server connection header is SETTINGS frame only.
	(when (eq state :new)
	  (if (< (buffer-size recv-buffer) #.(buffer-size *connection-header*))
	      (if (buffer-mismatch recv-buffer
				   *connection-header*
				   :end2 (buffer-size recv-buffer))
		  (raise 'http2-handshake-error)
		  (return-from receive))
	      (if (buffer-mismatch (buffer-read recv-buffer #.(buffer-size *connection-header*))
				   *connection-header*
				   :end1 #.(buffer-size *connection-header*))
		  (raise 'http2-handshake-error)
		  (progn
		    (setf state :connection-header)
		    (settings connection stream-limit window-limit)))))

	(while-let (frame (parse framer recv-buffer))
	  ; Header blocks MUST be transmitted as a contiguous sequence of frames
	  ; with no interleaved frames of any other type, or from any other stream.
	  (when continuation
	    (when (or (not (eq (getf frame :type) :continuation))
		      (not (equal (getf frame :stream) (getf (first continuation) :stream)))) ; *** equal ?
	      (connection-error connection))

	    (push frame continuation)
	    (when (not (member :end-headers (getf frame :flags)))
	      (return-from receive))

	    (let ((headers (flatten-n (mapcar (lambda (chunk)
						(decode-headers connection chunk)
						(getf chunk :payload))
					      continuation) 1)))

	      (setf frame (shift continuation))
	      (setf continuation nil)

	      (remf frame :length)
	      (setf (getf frame :payload) headers)
	      (push (if (eq (getf frame :type) :push-promise)
			:end-push-promise
			:end-headers)
		    (getf frame :flags))))

	  ; SETTINGS frames always apply to a connection, never a single stream.
	  ; The stream identifier for a settings frame MUST be zero.  If an
	  ; endpoint receives a SETTINGS frame whose stream identifier field is
	  ; anything other than 0x0, the endpoint MUST respond with a connection
	  ; error (Section 5.4.1) of type PROTOCOL_ERROR.
	  (if (connection-frame-p connection frame)
	      (connection-management connection frame)
	      (case (getf frame :type)
		(:headers
	         ; The last frame in a sequence of HEADERS/CONTINUATION
	         ; frames MUST have the END_HEADERS flag set.
		 (when (not (member :end-headers (getf frame :flags)))
		   (push frame continuation)
		   (return-from receive))

	  	 ; After sending a GOAWAY frame, the sender can discard frames
	  	 ; for new streams.  However, any frames that alter connection
	  	 ; state cannot be completely ignored.  For instance, HEADERS,
	  	 ; PUSH_PROMISE and CONTINUATION frames MUST be minimally
	  	 ; processed to ensure a consistent compression state
		 (decode-headers connection frame)
		 (when (eq state :closed)
		   (return-from receive))

		 (let ((stream (gethash (getf frame :stream) streams)))
		   (when (null stream)
		     (setf stream (activate-stream connection
				   (getf frame :stream)
				   (or (getf frame :priority) *default-priority*)))
		     (emit connection :stream stream))

		   (stream<< stream frame)))
		(:push-promise
		 ; The last frame in a sequence of PUSH_PROMISE/CONTINUATION
		 ; frames MUST have the END_PUSH_PROMISE/END_HEADERS flag set
		 (when (not (member :end-push-promise (getf frame :flags)))
		   (push frame continuation)
		   (return-from receive))
	     
		 (decode-headers connection frame)
		 (when (eq state :closed)
		   (return-from receive))
	     
		 ; PUSH_PROMISE frames MUST be associated with an existing, peer-
		 ; initiated stream... A receiver MUST treat the receipt of a
		 ; PUSH_PROMISE on a stream that is neither "open" nor
		 ; "half-closed (local)" as a connection error (Section 5.4.1) of
		 ; type PROTOCOL_ERROR. Similarly, a receiver MUST treat the
		 ; receipt of a PUSH_PROMISE that promises an illegal stream
		 ; identifier (Section 5.1.1) (that is, an identifier for a stream
		 ; that is not currently in the "idle" state) as a connection error
		 ; (Section 5.4.1) of type PROTOCOL_ERROR, unless the receiver
		 ; recently sent a RST_STREAM frame to cancel the associated stream.
		 (let ((parent (gethash (getf frame :stream) streams))
		       (pid (getf frame :promise-stream)))

		   (when (null parent)
		     (connection-error connection :msg "missing parent ID"))

		   (if (not (or (eq (state parent) :open)
				(eq (state parent) :half-closed-local)))
		       ; An endpoint might receive a PUSH_PROMISE frame after it sends
		       ; RST_STREAM.  PUSH_PROMISE causes a stream to become "reserved".
		       ; The RST_STREAM does not cancel any promised stream.  Therefore, if
		       ; promised streams are not desired, a RST_STREAM can be used to
		       ; close any of those streams.
		       (if (eq (closed parent) :local-rst)
			   ; We can either (a) 'resurrect' the parent, or (b) RST_STREAM
			   ; ... sticking with (b), might need to revisit later.
			   (send connection (list :type :rst-stream :stream pid :error :refused-stream))
			   (connection-error connection)))

		   (let ((stream (activate-stream pid *default-priority* parent)))
		     (emit connection :promise stream)
		     (stream<< stream frame))))
		(otherwise
		 (if-let (stream (gethash (getf frame :stream) streams))
		   (stream<< stream frame)
		   ; An endpoint that receives an unexpected stream identifier
		   ; MUST respond with a connection error of type PROTOCOL_ERROR.
		   (connection-error connection)))))))
    (t (e) (declare (ignore e)) (connection-error connection))))

(defalias connection<< receive)

(defmethod send ((connection connection) frame)
  "Send an outgoing frame. DATA frames are subject to connection flow
control and may be split and / or buffered based on current window size.
All other frames are sent immediately."
  (if (eq (getf frame :type) :data)
      (send-data connection frame t)
      ; An endpoint can end a connection at any time. In particular, an
      ; endpoint MAY choose to treat a stream error as a connection error.
      (if (eq (getf frame :type) :rst-stream)
	  (when (eq (getf frame :error) :protocol-error)
	    (goaway connection (getf frame :error)))
	  (emit connection :frame (encode connection frame)))))

(defmethod encode ((connection connection) frame)
  "Applies HTTP 2.0 binary encoding to the frame."
  (with-slots (framer) connection
    (when (member (getf frame :type) '(:headers :push-promise))
      (encode-headers connection frame))
    (generate framer frame)))

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
	  (flow-control-allowed-p connection)
	  (incf window (getf frame :increment))
	  (send-data connection nil t))
	 (:ping
	  (if (member :pong (getf frame :flags))
	      (emit connection :pong (getf frame :payload))
	      (send connection
		    (list :type :ping :stream 0 :flags (list :pong)
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

    (when (getf frame :payload)
      (doplist (key v (getf frame :payload))
	(case key
	  (:settings-max-concurrent-streams
	   (setf stream-limit v))

	  ; A change to SETTINGS_INITIAL_WINDOW_SIZE could cause the available
	  ; space in a flow control window to become negative. A sender MUST
	  ; track the negative flow control window, and MUST NOT send new flow
	  ; controlled frames until it receives WINDOW_UPDATE frames that cause
	  ; the flow control window to become positive.
	  (:settings-initial-window-size
	   (flow-control-allowed-p connection)
	   (setf window (+ (- window window-limit) v))
	   (dohash (id stream streams)
	     (emit stream :window (+ (- (window stream) window-limit) v)))
	   (setf window-limit v))

	  (:settings-flow-control-options
	   (flow-control-allowed-p connection)
	   (when (= v 1)
	     (setf window +infinity
		   window-limit +infinity))))))))

(defmethod decode-headers ((connection connection) frame)
  "Decode headers payload and update connection decompressor state.
The receiver endpoint reassembles the header block by concatenating
the individual fragments, then decompresses the block to reconstruct
the header set - aka, header payloads are buffered until END_HEADERS,
or an END_PROMISE flag is seen."
  (handler-case-unless *debug-mode*
      (progn
	(with-slots (decompressor) connection
	  (when (not (vectorp (getf frame :payload)))
	    (setf (getf frame :payload) (decode decompressor (getf frame :payload))))))
    (t (e) (connection-error connection :type :compression-error :msg e)))) ; ***

(defmethod encode-headers ((connection connection) frame)
  "Encode headers payload and update connection compressor state."
  (handler-case-unless *debug-mode*
      (with-slots (compressor) connection
	(when (not (vectorp (getf frame :payload)))
	  (setf (getf frame :payload) (encode compressor (getf frame :payload)))))
    (t (e) (connection-error connection :type :compression-error :msg e))))

(defmethod flow-control-allowed-p ((connection connection))
  "Once disabled, no further flow control operations are permitted."
  (with-slots (window-limit) connection
    (when (= window-limit +infinity)
      (connection-error connection :type :flow-control-error))))

(defmethod activate-stream ((connection connection) id priority &optional parent)
  "Activates new incoming or outgoing stream and registers appropriate
connection management callbacks."
  (with-slots (streams window-limit active-stream-count) connection
    (when (gethash id streams)
      (connection-error connection :msg "Stream ID already exists"))

    (let ((stream (make-instance 'stream :id id :priority priority
				 :window window-limit :parent parent)))

      ; Streams that are in the "open" state, or either of the "half closed"
      ; states count toward the maximum number of streams that an endpoint is
      ; permitted to open.
      (once stream :active (lambda-ignore (incf active-stream-count)))
      (once stream :close  (lambda-ignore (decf active-stream-count)))
      (when (typep connection 'server)
	(on stream :promise (lambda-apply (promise connection))))
      (on stream :frame (lambda-apply (send connection)))

      (setf (gethash id streams) stream))))

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

    (raise (find-symbol (symbol-name type)) msg)))
