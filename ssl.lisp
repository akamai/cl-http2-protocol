; We have to make some redefinitions in CL+SSL to get support for:
;
; (a) Reading a partial sequence, which we need for HTTP/2.0 binary frames.
;     To follow the ported model, a function is used to read network bytes
;     that is blocking for one byte and non-blocking for any remaining
;     bytes that may be available at that time. This likely results in the
;     fewest reads to the network system as multiple frames at once can be
;     slurped in and added to the receive buffer.
;
;     Background reading:
;     https://groups.google.com/forum/#!topic/comp.lang.lisp/qYA6daIGCzk
;     http://www.nhplace.com/kent/CL/Issues/stream-definition-by-user.html
;
;     Basically, while reading a variable number of octets from the
;     network is something you can do in almost all modern CL
;     implementations, the Gray Streams specification to which CL+SSL
;     adheres was closer to the traditional CL STREAM specification
;     which didn't conceive a way for that to happen. So wrapping a
;     socket in CL+SSL takes you a step backwards in that regard. But
;     OpenSSL supports it, so we just add wrapped support.
;
;     HTTP/2.0 does precede each frame with a payload size, so it would be
;     possible to read that, and then read the remainder of the frame. A
;     change could be made to read in 8 bytes, call CONNECTION<< and in
;     (DEFMETHOD PARSE (FRAMER ...)) to return the number of
;     further bytes needed for a frame to be complete instead of nil, which
;     could bubble back to the implementation and could be used to call
;     another read with the target length.
;
;     However those changes wouldn't help with the next issue:
;
; (b) Next Protocol Negotiation support, which we need for HTTP/2.0.
;     HTTP/2.0 uses TLS, and annouces itself via NPN.
;     (we'll also need ALPN in the future)
;

(in-package :cl+ssl)

; a method based on cl+ssl:read-sequence that permits non-blocking
(defmethod stream-read-partial-sequence ((stream ssl-stream) seq start end &key)
  (when (and (< start end) (ssl-stream-peeked-byte stream))
    (setf (elt seq start) (ssl-stream-peeked-byte stream))
    (setf (ssl-stream-peeked-byte stream) nil)
    (incf start))
  (let ((buf (ssl-stream-input-buffer stream)))
    (let ((length (min (- end start) (buffer-length buf))))
      (when (plusp length)
	(handler-case
	    (with-pointer-to-vector-data (ptr buf)
	      (let ((read-bytes
		     (ensure-ssl-funcall stream
					 (ssl-stream-handle stream)
					 #'ssl-read
					 (ssl-stream-handle stream)
					 ptr
					 length)))
		(s/b-replace seq buf :start1 start :end1 (+ start read-bytes))
		(incf start read-bytes)))
	  (ssl-error-zero-return () ;SSL_read returns 0 on end-of-file
	    (return-from stream-read-partial-sequence nil))
	  (t (e)
	    (error e)))))
    ;; fixme: kein out-of-file wenn (zerop start)?
    start))

(cffi:defcfun ("SSL_CTX_set_next_protos_advertised_cb" ssl-ctx-set-next-protos-advertised-cb)
    :void
  (ctx ssl-ctx)
  (cb :pointer)
  (arg :pointer))

(cffi:defcfun ("SSL_CTX_set_next_proto_select_cb" ssl-ctx-set-next-proto-select-cb)
    :void
  (ctx ssl-ctx)
  (cb :pointer)
  (arg :pointer))

(cffi:defcfun ("SSL_get0_next_proto_negotiated" ssl-get0-next-proto-negotiated)
    :void
  (s ssl-pointer)
  (data (:pointer :string))
  (len (:pointer :unsigned-int)))

(cffi:defcfun ("SSL_select_next_proto" ssl-select-next-proto)
    :int
  (out (:pointer (:pointer :unsigned-char)))
  (outlen (:pointer :unsigned-char))
  (server (:pointer :unsigned-char))
  (server-len (:pointer :unsigned-int))
  (client (:pointer :unsigned-char))
  (client-len :unsigned-int))

(cffi:defcfun ("next_protos_parse" next-protos-parse)
    :string
  (outlen (:pointer :unsigned-short))
  (in :string))

(defconstant +OPENSSL_NPN_UNSUPPORTED+ 0)
(defconstant +OPENSSL_NPN_NEGOTIATED+ 1)
(defconstant +OPENSSL_NPN_NO_OVERLAP+ 2)
(defconstant +SSL_TLSEXT_ERR_OK+ 0)

(cffi:defcstruct tlsextnextprotoctx
  (data :string)
  (len :unsigned-int))

(cffi:defcallback lisp-next-proto-cb :int
    ((s ssl-pointer) (data (:pointer :pointer)) (len (:pointer :unsigned-int)) (arg (:pointer tlsextnextprotoctx)))
  (declare (ignore s))
  (let (tmp-data tmp-len)
    (cffi:with-foreign-slots ((data len) arg tlsextnextprotoctx)
      (setf tmp-data data
	    tmp-len len))
    (setf (cffi:mem-ref data :string) tmp-data
	  (cffi:mem-ref len :unsigned-int) tmp-len))
  +SSL_TLSEXT_ERR_OK+)

; add NPN support
(defun make-ssl-client-stream
    (socket &key certificate key password (method 'ssl-v23-method) external-format
     close-callback (unwrap-stream-p t) next-protos-spec)
  "Returns an SSL stream for the client socket descriptor SOCKET.
CERTIFICATE is the path to a file containing the PEM-encoded certificate for
 your client. KEY is the path to the PEM-encoded key for the client, which
may be associated with the passphrase PASSWORD."
  (ensure-initialized :method method)
  (let ((stream (make-instance 'ssl-stream
			       :socket socket
			       :close-callback close-callback))
        (handle (ssl-new *ssl-global-context*)))
    (setf socket (install-handle-and-bio stream handle socket unwrap-stream-p))
    (ssl-set-connect-state handle)
    (with-pem-password (password)
      (install-key-and-cert handle key certificate))

    (let ((nps (pack-next-protos-spec next-protos-spec)))
      (cffi:with-foreign-object (arg 'tlsextnextprotoctx)
	(setf (cffi:foreign-slot-value arg 'tlsextnextprotoctx 'data) nps)
	(setf (cffi:foreign-slot-value arg 'tlsextnextprotoctx 'len) (length nps))
	(ssl-ctx-set-next-protos-advertised-cb *ssl-global-context*
					       (cffi:callback lisp-next-proto-cb)
					       arg)
	(ensure-ssl-funcall stream handle #'ssl-connect handle)))

    (when (ssl-check-verify-p)
      (ssl-stream-check-verify stream))
    (handle-external-format stream external-format)))

(defun pack-next-protos-spec (next-protos-spec)
  "Convert a list of NPN protocol names into a single concatenated string of length-prefixed names."
  (with-output-to-string (s)
    (dolist (p next-protos-spec)
      (assert (and (stringp p) (plusp (length p)) p "Entry in NPN list must be a string with 1+ characters."))
      (princ (code-char (length p)) s)
      (princ p s))))

; add NPN support
(defun make-ssl-server-stream
    (socket &key certificate key password (method 'ssl-v23-method) external-format
     close-callback (unwrap-stream-p t)
     (cipher-list *default-cipher-list*)
     next-protos-spec)
  "Returns an SSL stream for the server socket descriptor SOCKET.
CERTIFICATE is the path to a file containing the PEM-encoded certificate for
 your server. KEY is the path to the PEM-encoded key for the server, which
may be associated with the passphrase PASSWORD."
  (ensure-initialized :method method)
  (let ((stream (make-instance 'ssl-server-stream
		 :socket socket
		 :close-callback close-callback
		 :certificate certificate
		 :key key))
        (handle (ssl-new *ssl-global-context*)))

    (setf socket (install-handle-and-bio stream handle socket unwrap-stream-p))
    (ssl-set-accept-state handle)
    (when (zerop (ssl-set-cipher-list handle cipher-list))
      (error 'ssl-error-initialize :reason "Can't set SSL cipher list"))
    (with-pem-password (password)
      (install-key-and-cert handle key certificate))

    (let ((nps (pack-next-protos-spec next-protos-spec)))
      (cffi:with-foreign-object (arg 'tlsextnextprotoctx)
	(setf (cffi:foreign-slot-value arg 'tlsextnextprotoctx 'data) nps)
	(setf (cffi:foreign-slot-value arg 'tlsextnextprotoctx 'len) (length nps))
	(ssl-ctx-set-next-protos-advertised-cb *ssl-global-context*
					       (cffi:callback lisp-next-proto-cb)
					       arg)
	(ensure-ssl-funcall stream handle #'ssl-accept handle)))
    
    (handle-external-format stream external-format)))

(defun get-next-proto-negotiated (stream)
  (let ((handle (ssl-stream-handle stream))
	next-proto-negotiated)
    (cffi:with-foreign-object (len :unsigned-int)
      (cffi:with-foreign-object (data '(:pointer :unsigned-char))
	(ssl-get0-next-proto-negotiated handle data len)
	(setf next-proto-negotiated
	      (loop
		 with tmp-len = (cffi:mem-ref len :unsigned-int)
		 with ptr = (cffi:mem-ref data :pointer)
		 for i from 0 below tmp-len
		 collect (code-char (cffi:mem-aref ptr :unsigned-char i)) into chars
		 finally (return (coerce chars 'string))))))
    next-proto-negotiated))
