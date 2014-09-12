;; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)
;; Portions copied from CL+SSL, also under MIT License. Please see README.md for details.

;; Add & override some functions in CL+SSL
(in-package :cl+ssl)

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

;; we just overrode SSL_CTX_ctrl to have the last parameter be a pointer
;; override the places it is used in CL+SSL to make zeros into pointers
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
  (data :pointer)
  (len :unsigned-int))

(cffi:defcstruct client-tlsextnextprotoctx
  (data :string)
  (len :unsigned-short)
  (status :int))

(cffi:defcallback lisp-server-next-proto-cb :int
    ((s ssl-pointer) (data* (:pointer :pointer)) (len* (:pointer :unsigned-int))
     (arg (:pointer (:struct server-tlsextnextprotoctx))))
  (declare (ignore s))
  ;; (format t "inside lisp-server-next-proto-cb~%")
  (let (tmp-data tmp-len)
    (cffi:with-foreign-slots ((data len) arg (:struct server-tlsextnextprotoctx))
      (setf tmp-data data
	    tmp-len len))
    (setf (cffi:mem-ref data* :string) tmp-data
	  (cffi:mem-ref len* :unsigned-int) tmp-len))
  +SSL_TLSEXT_ERR_OK+)

(cffi:defcallback lisp-client-next-proto-cb :int
    ((s ssl-pointer)
     (out (:pointer (:pointer :unsigned-char))) (outlen (:pointer :unsigned-char))
     (in (:pointer :unsigned-char)) (inlen :unsigned-int) (arg :pointer))
  (declare (ignore s))
  ;; (format t "inside lisp-client-next-proto-cb~%")
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
  ;; (format t "lisp-ssl-servername-cb~%")
  (cffi:with-foreign-slots ((ack) arg (:struct tlsextctx))
    (let ((hn (ssl-get-servername s +TLSEXT_NAMETYPE_host_name+)))
      (if (/= (ssl-get-servername-type s) -1)
	  (setf ack (if (and (zerop (ssl-session-reused s))
			     (not (cffi:null-pointer-p hn))) 1 0))
	  (format t "Can't use SSL_get_servername~%"))
      +SSL_TLSEXT_ERR_OK+)))

(defparameter *dh2048* (cffi:null-pointer))

(defun init-dhparams (filename)
  (let ((bp (bio-new-file (namestring filename) "r")))
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
      *dh2048*
      (cffi:null-pointer)))

(defun get-next-proto-negotiated-from-handle (handle)
  (cffi:with-foreign-object (len :unsigned-int)
    (cffi:with-foreign-object (data '(:pointer :unsigned-char))
      (ssl-get0-next-proto-negotiated handle data len)
      (loop
	 with tmp-len = (cffi:mem-ref len :unsigned-int)
	 with ptr = (cffi:mem-ref data :pointer)
	 for i from 0 below tmp-len
	 collect (code-char (cffi:mem-aref ptr :unsigned-char i)) into chars
	 finally (return (coerce chars 'string))))))

(defun get-next-proto-negotiated (stream)
  (get-next-proto-negotiated-from-handle (ssl-stream-handle stream)))

;; note that we added DH support here but CL-ASYNC actually does this
;; itself so it's duplicated elsewhere
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
