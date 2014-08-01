; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

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

(cffi:defcfun ("TLSv1_2_client_method" ssl-TLSv1.2-client-method)
    ssl-method)
(cffi:defcfun ("TLSv1_2_server_method" ssl-TLSv1.2-server-method)
    ssl-method)
(cffi:defcfun ("TLSv1_2_method" ssl-TLSv1.2-method)
    ssl-method)

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
  (server-len :unsigned-int)
  (client (:pointer :unsigned-char))
  (client-len :unsigned-int))

(cffi:defcfun ("SSL_CTX_set_tmp_dh_callback" ssl-ctx-set-tmp-dh-callback)
    :void
  (ctx (ssl-ctx))
  (cb :pointer))

(cffi:defcfun ("SSL_CTX_ctrl" ssl-ctx-ctrl)
    :long
  (ctx (ssl-ctx))
  (cmd :int)
  (larg :long)
  (parg :pointer))

; we just overrode SSL_CTX_ctrl to have the last parameter be a pointer
; override the places it is used in CL+SSL to make zeros into pointers
(defun ssl-ctx-set-session-cache-mode (ctx mode)
  (ssl-ctx-ctrl ctx +SSL_CTRL_SET_SESS_CACHE_MODE+ mode (cffi:null-pointer)))
(defun install-handle-and-bio (stream handle socket unwrap-stream-p)
  (setf (ssl-stream-handle stream) handle)
  (when unwrap-stream-p
    (let ((fd (stream-fd socket)))
      (when fd
	(setf socket fd))))
  (etypecase socket
    (integer
     (install-nonblock-flag socket)
     (ssl-set-fd handle socket))
    (stream
     (ssl-set-bio handle (bio-new-lisp) (bio-new-lisp))))
  (ssl-ctx-ctrl handle
		+SSL_CTRL_MODE+
		+SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER+
		(cffi:null-pointer))
  socket)


(defconstant +SSL_CTRL_SET_TMP_ECDH+ 4)

(defmacro ssl-ctx-set-tmp-ecdh (ctx ecdh)
  `(ssl-ctx-ctrl ,ctx +SSL_CTRL_SET_TMP_ECDH+ 0 ,ecdh))

(cffi:defctype bio-pointer :pointer)
(cffi:defctype dh-pointer :pointer)
(cffi:defctype ec-key-pointer :pointer)

(cffi:defcfun ("BIO_new_file" bio-new-file)
    bio-pointer
  (filename :string)
  (mode :string))

(cffi:defcfun ("PEM_read_bio_DHparams" pem-read-bio-dhparams)
    dh-pointer
  (bp bio-pointer)
  (x (:pointer :pointer))
  (cb :pointer)
  (u :pointer))

(cffi:defcfun ("BIO_free" bio-free)
    :int
  (a bio-pointer))

(cffi:defcfun ("DH_free" dh-free)
    :void
  (dh dh-pointer))

(cffi:defcfun ("EC_KEY_new_by_curve_name" ec-key-new-by-curve-name)
    ec-key-pointer
  (nid :int))

(defconstant +OPENSSL_NPN_UNSUPPORTED+ 0)
(defconstant +OPENSSL_NPN_NEGOTIATED+ 1)
(defconstant +OPENSSL_NPN_NO_OVERLAP+ 2)
(defconstant +SSL_TLSEXT_ERR_OK+ 0)

(defconstant +OPENSSL_NPN_UNSUPPORTED+ 0)
(defconstant +OPENSSL_NPN_NEGOTIATED+ 1)
(defconstant +OPENSSL_NPN_NO_OVERLAP+ 2)

(defconstant +NID_X9_62_prime256v1+ 415)

(cffi:defcstruct server-tlsextnextprotoctx
  (data :string)
  (len :unsigned-int))

(cffi:defcstruct client-tlsextnextprotoctx
  (data :string)
  (len :unsigned-short)
  (status :int))

(cffi:defcallback lisp-server-next-proto-cb :int
    ((s ssl-pointer) (data (:pointer :pointer)) (len (:pointer :unsigned-int))
     (arg (:pointer server-tlsextnextprotoctx)))
  (declare (ignore s))
  (let (tmp-data tmp-len)
    (cffi:with-foreign-slots ((data len) arg server-tlsextnextprotoctx)
      (setf tmp-data data
	    tmp-len len))
    ; (format t "lisp-server-next-proto-cb: ~S ~S~%" tmp-data tmp-len)
    (setf (cffi:mem-ref data :string) tmp-data
	  (cffi:mem-ref len :unsigned-int) tmp-len))
  +SSL_TLSEXT_ERR_OK+)

(cffi:defcallback lisp-client-next-proto-cb :int
    ((s ssl-pointer)
     (out (:pointer (:pointer :unsigned-char))) (outlen (:pointer :unsigned-char))
     (in (:pointer :unsigned-char)) (inlen :unsigned-int) (arg :pointer))
  (declare (ignore s))
  (format t "Inside lisp-client-next-proto-cb~%")
  (cffi:with-foreign-slots ((data len status) arg (:struct client-tlsextnextprotoctx))
    (cffi:with-foreign-string (data* data)
      (setf status (ssl-select-next-proto out outlen in inlen data* len))))
  +SSL_TLSEXT_ERR_OK+)

(cffi:defcfun ("SSL_CTX_callback_ctrl" ssl-ctx-callback-ctrl)
    :long
  (ctx ssl-ctx)
  (cmd :int)
  (fp :pointer))

(defconstant +SSL_CTRL_SET_TLSEXT_SERVERNAME_CB+ 53)
(defconstant +SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG+ 54)
(defconstant +SSL_CTRL_SET_TLSEXT_HOSTNAME+ 55)

(defmacro ssl-ctx-set-tlsext-servername-callback (ctx cb)
  `(ssl-ctx-callback-ctrl ,ctx +SSL_CTRL_SET_TLSEXT_SERVERNAME_CB+ ,cb))

(defmacro ssl-ctx-set-tlsext-servername-arg (ctx arg)
  `(ssl-ctx-ctrl ,ctx +SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG+ 0 ,arg))

(defmacro ssl-set-tlsext-host-name (s name)
  `(ssl-ctrl ,s +SSL_CTRL_SET_TLSEXT_HOSTNAME+ +TLSEXT_NAMETYPE_host_name+ ,name))

(cffi:defcstruct tlsextctx
  (biodebug bio-pointer)
  (ack :int))

(cffi:defcfun ("SSL_get_servername" ssl-get-servername)
    (:pointer :char)
  (s ssl-pointer)
  (type :int))

(cffi:defcfun ("SSL_get_servername_type" ssl-get-servername-type)
    :int
  (s ssl-pointer))

(cffi:defcfun ("SSL_ctrl" ssl-ctrl)
    :long
  (s ssl-pointer)
  (cmd :int)
  (larg :long)
  (parg :pointer))

(defconstant +SSL_CTRL_GET_SESSION_REUSED+ 8)

(defmacro ssl-session-reused (s)
  `(ssl-ctrl ,s +SSL_CTRL_GET_SESSION_REUSED+ 0 (cffi:null-pointer)))

(defconstant +TLSEXT_NAMETYPE_host_name+ 0)

(cffi:defcallback lisp-ssl-servername-cb :int
    ((s ssl-pointer)
     (ad (:pointer :int))
     (arg :pointer))
  (declare (ignore ad))
  (format t "lisp-ssl-servername-cb~%")
  (cffi:with-foreign-slots ((ack) arg (:struct tlsextctx))
    (let ((hn (ssl-get-servername s +TLSEXT_NAMETYPE_host_name+)))
      (if (/= (ssl-get-servername-type s) -1)
	  (setf ack (if (and (zerop (ssl-session-reused s))
			     (not (cffi:null-pointer-p hn))) 1 0))
	  (format t "Can't use SSL_get_servername~%"))
      +SSL_TLSEXT_ERR_OK+)))

(defparameter *dh2048* (cffi:null-pointer))

(defun init-dhparams (filename)
  (let ((bp (bio-new-file filename "r")))
    (when (cffi:null-pointer-p bp)
      (error "Error opening DH 2048 file"))
    (when *dh2048*
      (dh-free *dh2048*)
      (setf *dh2048* (cffi:null-pointer)))
    (unwind-protect
	 (progn
	   (setf *dh2048* (pem-read-bio-dhparams bp (cffi:null-pointer) (cffi:null-pointer) (cffi:null-pointer)))
	   (when (cffi:null-pointer-p *dh2048*)
	     (error "Error reading DH 2048 parameters")))
      (bio-free bp)))
  *dh2048*)

(cffi:defcallback lisp-tmp-dh-callback :pointer
    ((ssl ssl-pointer)
     (is-export :int)
     (keylength :int))
  (declare (ignore ssl is-export))
  (if (= keylength 2048)
      *dh2048*  ; set by init-dhparams called by make-ssl-server-stream
      (cffi:null-pointer)))

(defun pack-next-protos-spec (next-protos-spec)
  "Convert a list of NPN protocol names into a single concatenated string of length-prefixed names."
  (with-output-to-string (s)
    (dolist (p next-protos-spec)
      (assert (and (stringp p) (plusp (length p)) p "Entry in NPN list must be a string with 1+ characters."))
      (princ (code-char (length p)) s)
      (princ p s))))

; add NPN support
(defun make-ssl-client-stream
    (socket &key certificate key password (method 'ssl-tlsv1.2-method) external-format
     close-callback (unwrap-stream-p t) servername next-protos-spec)
  "Returns an SSL stream for the client socket descriptor SOCKET.
CERTIFICATE is the path to a file containing the PEM-encoded certificate for
 your client. KEY is the path to the PEM-encoded key for the client, which
may be associated with the passphrase PASSWORD."
  (format t "make-ssl-client-stream overridden~%")
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
      (cffi:with-foreign-object (arg '(:struct client-tlsextnextprotoctx))
	(cffi:with-foreign-slots ((data len) arg (:struct client-tlsextnextprotoctx))
	  (cffi:with-foreign-string (nps* nps)
	    (setf data nps*
		  len (length nps))
	    (ssl-ctx-set-next-proto-select-cb *ssl-global-context*
					      (cffi:callback lisp-client-next-proto-cb)
					      arg)

	    (if servername
		(cffi:with-foreign-object (sni '(:struct tlsextctx))
		  (cffi:with-foreign-slots ((biodebug) sni (:struct tlsextctx))
		    (setf biodebug (cffi:null-pointer))
		    (ssl-ctx-set-tlsext-servername-callback *ssl-global-context*
							    (cffi:callback lisp-ssl-servername-cb))
		    (ssl-ctx-set-tlsext-servername-arg *ssl-global-context*
						       sni)
		    (cffi:with-foreign-string (servername* servername)
		      (ssl-set-tlsext-host-name handle servername*)
		      (ensure-ssl-funcall stream handle #'ssl-connect handle))))
		(ensure-ssl-funcall stream handle #'ssl-connect handle))))))

    (when (ssl-check-verify-p)
      (ssl-stream-check-verify stream))
    (handle-external-format stream external-format)))

; add NPN support
(defun make-ssl-server-stream
    (socket &key certificate key dhparams password (method 'ssl-v23-method) external-format
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
    (when dhparams
      (init-dhparams dhparams))

    (let ((nps (pack-next-protos-spec next-protos-spec)))
      (cffi:with-foreign-object (arg '(:struct server-tlsextnextprotoctx))
	(cffi:with-foreign-slots ((data len) arg (:struct server-tlsextnextprotoctx))
	  (cffi:with-foreign-string (nps* nps)
	    (setf data nps*
		  len (length nps))
	    (ssl-ctx-set-next-protos-advertised-cb *ssl-global-context*
						   (cffi:callback lisp-server-next-proto-cb)
						   arg)
	    (ensure-ssl-funcall stream handle #'ssl-accept handle)))))
    
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

(defun initialize (&key (method 'ssl-v23-method) rand-seed)
  (setf *locks* (loop
		   repeat (crypto-num-locks)
		   collect (bt:make-lock)))
  (crypto-set-locking-callback (cffi:callback locking-callback))
  (crypto-set-id-callback (cffi:callback threadid-callback))
  (setf *bio-lisp-method* (make-bio-lisp-method))
  (ssl-load-error-strings)
  (ssl-library-init)
  (when rand-seed
    (init-prng rand-seed))
  (setf *ssl-check-verify-p* :unspecified)
  (setf *ssl-global-method* (funcall method))
  (setf *ssl-global-context* (ssl-ctx-new *ssl-global-method*))
  (ssl-ctx-set-session-cache-mode *ssl-global-context* 3)
  (ssl-ctx-set-default-passwd-cb *ssl-global-context* 
                                 (cffi:callback pem-password-callback))
  (ssl-ctx-set-tmp-rsa-callback *ssl-global-context*
                                (cffi:callback need-tmp-rsa-callback))
  (ssl-ctx-set-tmp-dh-callback *ssl-global-context*
			       (cffi:callback lisp-tmp-dh-callback))
  (ssl-ctx-set-tmp-ecdh *ssl-global-context*
			(ec-key-new-by-curve-name +NID_X9_62_prime256v1+)))
