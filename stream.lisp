; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol)

; A single HTTP 2.0 connection can multiplex multiple streams in parallel:
; multiple requests and responses can be in flight simultaneously and stream
; data can be interleaved and prioritized.
;
; This class encapsulates all of the state, transition, flow-control, and
; error management as defined by the HTTP 2.0 specification. All you have
; to do is subscribe to appropriate events (marked with ":" prefix in
; diagram below) and provide your application logic to handle request
; and response processing.
;
;                         +--------+
;                    PP   |        |   PP
;                ,--------|  idle  |--------.
;               /         |        |         \
;              v          +--------+          v
;       +----------+          |           +----------+
;       |          |          | H         |          |
;   ,---|:reserved |          |           |:reserved |---.
;   |   | (local)  |          v           | (remote) |   |
;   |   +----------+      +--------+      +----------+   |
;   |      | :active      |        |      :active |      |
;   |      |      ,-------|:active |-------.      |      |
;   |      | H   /   ES   |        |   ES   \   H |      |
;   |      v    v         +--------+         v    v      |
;   |   +-----------+          |          +-_---------+  |
;   |   |:half_close|          |          |:half_close|  |
;   |   |  (remote) |          |          |  (local)  |  |
;   |   +-----------+          |          +-----------+  |
;   |        |                 v                |        |
;   |        |    ES/R    +--------+    ES/R    |        |
;   |        `----------->|        |<-----------'        |
;   | R                   | :close |                   R |
;   `-------------------->|        |<--------------------'
;                         +--------+

(defclass stream (flowbuffer-include emitter-include error-include)
  ((id :reader stream-id :initarg :id :type integer
       :documentation "Stream ID (odd for client initiated streams, even otherwise).")
   (priority :reader stream-priority :initarg :priority :type integer
	     :documentation "Stream priority as set by initiator.")
   (window :reader stream-window :initarg :window :type (or integer float)
	   :documentation "Size of current stream flow control window.")
   (parent :reader stream-parent :initarg :parent :initform nil :type (or null stream)
	   :documentation "Request parent stream of push stream.")
   (state :reader stream-state :initform :idle
	  :type (member :idle :open :reserved-local :reserved-remote
			:half-closed-local :half-closed-remote
			:local-closed :remote-closed
			:local-rst :remote-rst
			:half-closing :closing :closed)
	  :documentation "Stream state as defined by HTTP 2.0.")
   (error :reader stream-error-type :initform nil)
   (closed :reader stream-closed :initform nil
	   :documentation "Reason why connection was closed.")
   (send-buffer :initform nil)))

; Note that you should never have to call MAKE-INSTANCE directly. To
; create a new client initiated stream, use (DEFMETHOD NEW-STREAM
; (CONNECTION ...)). Similarly, CONNECTION will emit new stream
; objects, when new stream frames are received.

(defmethod initialize-instance :after ((stream stream) &key)
  (with-slots (window) stream
    (on stream :window (lambda (v) (setf window v)))))

(defmethod receive ((stream stream) frame)
  "Processes incoming HTTP 2.0 frames. The frames must be decoded upstream."
  (with-slots (priority window) stream
    (transition stream frame nil)
  
    (case (getf frame :type)
      (:data
       (when (not (getf frame :ignore))
	 (emit stream :data frame)))
      ((:headers :push-promise)
       (if (listp (getf frame :payload))
	   (when (not (getf frame :ignore))
	     (emit stream :headers (plist-alist (flatten (getf frame :payload)))))
	   (when (not (getf frame :ignore))
	     (emit stream :headers (getf frame :payload)))))
      (:priority
       (setf priority (getf frame :priority))
       (emit stream :priority priority))
      (:window-update
       (incf window (getf frame :increment))
       (send-data stream)))

    (complete-transition stream frame)))

(defmethod send ((stream stream) frame)
  "Processes outgoing HTTP 2.0 frames. Data frames may be automatically
split and buffered based on maximum frame size and current stream flow
control window size."
  (with-slots (id priority) stream
    (transition stream frame t)
    (ensuref (getf frame :stream) id)
    
    (when (eq (getf frame :type) :priority)
      (setf priority (getf frame :priority)))

    (if (eq (getf frame :type) :data)
	(send-data stream frame)
	(emit stream :frame frame))

    (complete-transition stream frame)))

(defmethod headers ((stream stream) headers &key (end-headers t) (end-stream nil))
  "Sends a HEADERS frame containing HTTP response headers."
  (let (flags)
    (when end-headers
      (push :end-headers flags))
    (when end-stream
      (push :end-stream flags))
    (send stream (list :type :headers
		       :flags (nreverse flags)
		       :payload headers))))

(defmethod stream-promise ((stream stream) headers &optional (end-push-promise t) block) ; ***
  (when (null block)
    (error "must provide callback"))

  (let ((flags (if end-push-promise (list :end-push-promise) nil)))
    (emit stream :promise stream headers flags block)))

(defmethod reprioritize ((stream stream) p)
  "Sends a PRIORITY frame with new stream priority value (can only be
performed by the client)."
  (with-slots (id) stream
    (when (evenp id)
      (stream-error stream))
    (send stream (list :type :priority :priority p))))

(defmethod data ((stream stream) payload &key (end-stream t))
  "Sends DATA frame containing response payload."
  (let (flags)
    (when end-stream
      (push :end-stream flags))
    
    (while (> (buffer-size payload) *max-frame-size*)
      (let ((chunk (buffer-slice! payload 0 *max-frame-size*)))
	(send stream (list :type :data :payload chunk))))
    
    (send stream (list :type :data :flags flags :payload payload))))

(defmethod stream-close ((stream stream) &optional (error :stream-closed)) ; @ ***
  "Sends a RST_STREAM frame which closes current stream - this does not
close the underlying connection."
  (send stream (list :type :rst-stream :error error)))

(defmethod cancel ((stream stream))
  "Sends a RST_STREAM indicating that the stream is no longer needed."
  (send stream (list :type :rst-stream :error :cancel)))

(defmethod refuse ((stream stream))
  "Sends a RST_STREAM indicating that the stream has been refused prior
to performing any application processing."
  (send stream (list :type :rst-stream :error :refused-stream)))

; HTTP 2.0 Stream States
; - http://tools.ietf.org/html/draft-ietf-httpbis-http2-05#section-5
;
;                       +--------+
;                 PP    |        |    PP
;              ,--------|  idle  |--------.
;             /         |        |         \
;            v          +--------+          v
;     +----------+          |           +----------+
;     |          |          | H         |          |
; ,---| reserved |          |           | reserved |---.
; |   | (local)  |          v           | (remote) |   |
; |   +----------+      +--------+      +----------+   |
; |      |          ES  |        |  ES          |      |
; |      | H    ,-------|  open  |-------.      | H    |
; |      |     /        |        |        \     |      |
; |      v    v         +--------+         v    v      |
; |   +----------+          |           +----------+   |
; |   |   half   |          |           |   half   |   |
; |   |  closed  |          | R         |  closed  |   |
; |   | (remote) |          |           | (local)  |   |
; |   +----------+          |           +----------+   |
; |        |                v                 |        |
; |        |  ES / R    +--------+  ES / R    |        |
; |        `----------->|        |<-----------'        |
; |  R                  | closed |                  R  |
; `-------------------->|        |<--------------------'
;                       +--------+
;
(defmethod transition ((stream stream) frame sending)
  (with-slots (state closed) stream
    (case state
      ; All streams start in the "idle" state.  In this state, no frames
      ; have been exchanged.
      ; *  Sending or receiving a HEADERS frame causes the stream to
      ;    become "open".  The stream identifier is selected as described
      ;    in Section 5.1.1.
      ; *  Sending a PUSH_PROMISE frame marks the associated stream for
      ;    later use.  The stream state for the reserved stream
      ;    transitions to "reserved (local)".
      ; *  Receiving a PUSH_PROMISE frame marks the associated stream as
      ;    reserved by the remote peer.  The state of the stream becomes
      ;    "reserved (remote)".
      (:idle
       (if sending
	   (case (getf frame :type)
	     (:push-promise
	      (event stream :reserved-local))
	     (:headers
	      (if (end-stream-p stream frame)
		  (event stream :half-closed-local)
		  (event stream :open)))
	     (:rst-stream
	      (event stream :local-rst))
	     (otherwise
	      (stream-error stream)))
	   (case (getf frame :type)
	     (:push-promise
	      (event stream :reserved-remote))
	     (:headers
	      (if (end-stream-p stream frame)
		  (event stream :half-closed-remote)
		  (event stream :open)))
	     (otherwise
	      (stream-error stream :type :protocol-error)))))

      ; A stream in the "reserved (local)" state is one that has been
      ; promised by sending a PUSH_PROMISE frame.  A PUSH_PROMISE frame
      ; reserves an idle stream by associating the stream with an open
      ; stream that was initiated by the remote peer (see Section 8.2).
      ; *  The endpoint can send a HEADERS frame.  This causes the stream
      ;    to open in a "half closed (remote)" state.
      ; *  Either endpoint can send a RST_STREAM frame to cause the stream
      ;    to become "closed".  This also releases the stream reservation.
      ; An endpoint MUST NOT send any other type of frame in this state.
      ; Receiving any frame other than RST_STREAM or PRIORITY MUST be
      ; treated as a connection error (Section 5.4.1) of type
      ; PROTOCOL_ERROR.
      (:reserved-local
       (if sending
	   (setf state (case (getf frame :type)
			 (:headers (event stream :half-closed-remote))
			 (:rst-stream (event stream :local-rst))
			 (otherwise (stream-error stream))))
	   (setf state (case (getf frame :type)
			 (:rst-stream (event stream :remote-rst))
			 (:priority state)
			 (otherwise (stream-error stream))))))
      
      ; A stream in the "reserved (remote)" state has been reserved by a
      ; remote peer.
      ; *  Receiving a HEADERS frame causes the stream to transition to
      ;    "half closed (local)".
      ; *  Either endpoint can send a RST_STREAM frame to cause the stream
      ;    to become "closed".  This also releases the stream reservation.
      ; Receiving any other type of frame MUST be treated as a stream
      ; error (Section 5.4.2) of type PROTOCOL_ERROR.  An endpoint MAY
      ; send RST_STREAM or PRIORITY frames in this state to cancel or
      ; reprioritize the reserved stream.
      (:reserved-remote
       (if sending
	   (setf state (case (getf frame :type)
			 (:rst-stream (event stream :local-rst))
			 (:priority state)
			 (otherwise (stream-error stream))))
	   (setf state (case (getf frame :type)
			 (:headers (event stream :half-closed-local))
			 (:rst-stream (event stream :remote-rst))
			 (otherwise (stream-error stream))))))
      
      ; The "open" state is where both peers can send frames of any type.
      ; In this state, sending peers observe advertised stream level flow
      ; control limits (Section 5.2).
      ; * From this state either endpoint can send a frame with a END_STREAM
      ;   flag set, which causes the stream to transition into one of the
      ;   "half closed" states: an endpoint sending a END_STREAM flag causes
      ;   the stream state to become "half closed (local)"; an endpoint
      ;   receiving a END_STREAM flag causes the stream state to become
      ;   "half closed (remote)".
      ; * Either endpoint can send a RST_STREAM frame from this state,
      ;   causing it to transition immediately to "closed".
      (:open
       (if sending
	   (case (getf frame :type)
	     ((:data :headers :continuation)
	      (when (end-stream-p stream frame)
		(event stream :half-closed-local)))
	     (:rst-stream
	      (event stream :local-rst)))
	   (case (getf frame :type)
	     ((:data :headers :continuation)
	      (when (end-stream-p stream frame)
		(event stream :half-closed-remote)))
	     (:rst-stream
	      (event stream :remote-rst)))))
      
      ; A stream that is "half closed (local)" cannot be used for sending
      ; frames.
      ; A stream transitions from this state to "closed" when a frame that
      ; contains a END_STREAM flag is received, or when either peer sends
      ; a RST_STREAM frame.
      ; A receiver can ignore WINDOW_UPDATE or PRIORITY frames in this
      ; state.  These frame types might arrive for a short period after a
      ; frame bearing the END_STREAM flag is sent.
      (:half-closed-local
       (if sending
	   (if (eq (getf frame :type) :rst-stream)
	       (event stream :local-rst)
	       (stream-error stream))
	   (case (getf frame :type)
	     ((:data :headers :continuation)
	      (when (end-stream-p stream frame)
		(event stream :remote-closed)))
	     (:rst-stream
	      (event stream :remote-rst))
	     ((:window-update :priority)
	      ; *** In the Ruby :ignore on the next line is misspelled :igore
	      (setf (getf frame :ignore) t)))))

      ; A stream that is "half closed (remote)" is no longer being used by
      ; the peer to send frames.  In this state, an endpoint is no longer
      ; obligated to maintain a receiver flow control window if it
      ; performs flow control.
      ; If an endpoint receives additional frames for a stream that is in
      ; this state it MUST respond with a stream error (Section 5.4.2) of
      ; type STREAM_CLOSED.
      ; A stream can transition from this state to "closed" by sending a
      ; frame that contains a END_STREAM flag, or when either peer sends a
      ; RST_STREAM frame.
      (:half-closed-remote
       (if sending
	   (case (getf frame :type)
	     ((:data :headers :continuation)
	      (when (end-stream-p stream frame)
		(event stream :local-closed)))
	     (:rst-stream
	      (event stream :local-rst)))
	   (case (getf frame :type)
	     (:rst-stream
	      (event stream :remote-rst))
	     (:window-update
	      (setf (getf frame :ignore) t))
	     (otherwise
	      (stream-error stream :type :stream-closed)))))

      ; An endpoint MUST NOT send frames on a closed stream. An endpoint
      ; that receives a frame after receiving a RST_STREAM or a frame
      ; containing a END_STREAM flag on that stream MUST treat that as a
      ; stream error (Section 5.4.2) of type STREAM_CLOSED.
      ;
      ; WINDOW_UPDATE or PRIORITY frames can be received in this state for
      ; a short period after a a frame containing an END_STREAM flag is
      ; sent.  Until the remote peer receives and processes the frame
      ; bearing the END_STREAM flag, it might send either frame type.
      ;
      ; If this state is reached as a result of sending a RST_STREAM
      ; frame, the peer that receives the RST_STREAM might have already
      ; sent - or enqueued for sending - frames on the stream that cannot
      ; be withdrawn. An endpoint MUST ignore frames that it receives on
      ; closed streams after it has sent a RST_STREAM frame.
      ;
      ; An endpoint might receive a PUSH_PROMISE or a CONTINUATION frame
      ; after it sends RST_STREAM. PUSH_PROMISE causes a stream to become
      ; "reserved". If promised streams are not desired, a RST_STREAM can
      ; be used to close any of those streams.
      (:closed
       (if sending
	   (case (getf frame :type)
	     (:rst-stream nil)
	     (otherwise
	      (when (not (eq (getf frame :type) :rst-stream))
		(stream-error stream :type :stream-closed))))
	   (case closed
	     ((:remote-rst :remote-closed)
	      (when (not (eq (getf frame :type) :rst-stream))
		(stream-error stream :type :stream-closed)))
	     ((:local-rst :local-closed)
	      (setf (getf frame :ignore) t))))))))

(defmethod event ((stream stream) newstate)
  (with-slots (state closed) stream
    (case newstate
      (:open
       (setf state newstate)
       (emit stream :active))
      ((:reserved-local :reserved-remote)
       (setf state newstate)
       (emit stream :reserved))
      ((:half-closed-local :half-closed-remote)
       (setf closed newstate)
       (unless (eq state :open)
	 (emit stream :active))
       (setf state :half-closing))
      ((:local-closed :remote-closed :local-rst :remote-rst)
       (setf closed newstate)
       (setf state :closing)))
    state))

(defmethod complete-transition ((stream stream) frame)
  (with-slots (state closed) stream
    (case state
      (:closing
       (setf state :closed)
       (emit stream :close (getf frame :error)))
      (:half-closing
       (setf state closed)
       (emit stream :half-close)))))

(defmethod end-stream-p ((stream stream) frame)
  (case (getf frame :type)
    ((:data :headers :continuation)
     (if (member :end-stream (getf frame :flags)) t nil))
    (otherwise nil)))

(defmethod stream-error ((stream stream) &key (type :http-stream-error) (msg "Stream error"))
  (with-slots (error state) stream
    (setf error type)
    (when (not (eq state :closed))
      (stream-close stream type))

    (raise (find-symbol (symbol-name type)) msg)))
