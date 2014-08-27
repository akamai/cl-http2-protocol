; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol-example)

(defparameter *key-pathname*         (merge-pathnames "cl-http2-protocol/" (user-homedir-pathname)))
(defparameter *server-key-file*      (merge-pathnames "mykey.pem" *key-pathname*))
(defparameter *server-cert-file*     (merge-pathnames "mycert.pem" *key-pathname*))
(defparameter *server-dhparams-file* (merge-pathnames "dhparams.2048.pem" *key-pathname*))

(defparameter *next-protos-spec* '("h2-13"))

(defparameter *dump-bytes* t)         ; t or nil
(defparameter *dump-bytes-stream* t)  ; t for stdout, or a stream
(defparameter *dump-bytes-base* 10)   ; 10 for decimal, 16 for hexadecimal
(defparameter *dump-bytes-hook* nil)  ; nil or 'vector-inspect make sense

(defmacro maybe-dump-bytes (type bytes)
  `(when *dump-bytes*
     (let ((*print-base* *dump-bytes-base*))
       (format *dump-bytes-stream* ,(concatenate 'string "http2 " (string-downcase type) ": ~A~%")
	       (if *dump-bytes-hook* (funcall *dump-bytes-hook* ,bytes) ,bytes)))))

(defun example-client (uri &key (net nil net-arg-p) (secure nil secure-arg-p)
			     (debug-mode *debug-mode*)
			     (dump-bytes *dump-bytes*))
  (assert (or (not net-arg-p) (not secure-arg-p)) (net secure) "Provide either :NET or :SECURE")
  (when (not (uri-p uri))
    (setf uri (parse-uri uri)))
  (ensuref net (make-instance (if (or secure (eq (uri-scheme uri) :https))
				  'net-ssl
				  #+sbcl 'net-plain-sb-bsd-sockets
				  #-sbcl 'net-plain-usocket)))
  (assert (typep net 'net) (net) ":NET object must be of type NET")
  (let ((*debug-mode* debug-mode)
	(*dump-bytes* dump-bytes))
    (handler-case
	(example-client-inner net uri)
      (connection-refused-error ()
	(format t "Connection refused: ~A~%" uri)))))

(defun example-client-inner (net uri)
  (format t "About to connect socket to ~A port ~A...~%"
	  (uri-host uri) (or (uri-port uri) 443))
  (net-socket-connect net (uri-host uri) (or (uri-port uri) (if (eq (uri-scheme uri) :https) 443 80)))
  (multiple-value-bind (address port) (net-socket-peer net)
    ;; the usocket forms here are generic enough to work on addresses from the other libraries too
    (format t "Connected to ~A:~A!~%" (usocket:hbo-to-dotted-quad (usocket:ip-from-octet-buffer address)) port))
  (unwind-protect
       (progn
	 (net-socket-prepare-client net)
	 (example-client-connected-socket net uri))
    (net-socket-close net)))

(defun example-client-connected-socket (net uri)
  (let ((conn (make-instance 'client)))
    (on conn :frame
	(lambda (bytes)
	  (send-bytes net (buffer-data bytes))))
    
    (let ((stream (new-stream conn)))

      (on conn :promise
	  (lambda (promise)
	    (on promise :headers
		(lambda (h)
		  (format t "promise headers: ~S~%" h)))
	    (on promise :data
		(lambda (d)
		  (format t "promise data chunk: ~D~%" (length d))))))
      
      (on stream :close
	  (lambda (e)
	    (if e
		(format t "stream closed, error: ~A~%" e)
		(format t "stream closed~%"))
	    (error 'end-of-file :stream (net-socket net))))  ; normal behavior to throw EOF

      (on stream :half-close
	  (lambda ()
	    (format t "closing client-end of the stream~%")))

      (on stream :headers
	  (lambda (h)
	    (format t "response headers: ~S~%" h)))

      (on stream :data
	  (lambda (d)
	    (format t "response data chunk: <<~A>>~%" (buffer-string (getf d :payload)))))

      (let ((head `((":scheme" . ,(string-downcase (string (uri-scheme uri))))
		    (":method" . "GET")
		    (":authority" . ,(if (= (or (uri-port uri) 80) 80)
					 (uri-host uri)
					 (format nil "~A:~A" (uri-host uri) (or (uri-port uri) 80))))
		    (":path"   . ,(or (uri-path uri) "/"))
		    ("accept"  . "*/*")
		    ("user-agent" . "HTTP/2 Common Lisp Test Agent"))))
	(format t "Sending HTTP 2.0 request~%~S~%" head)
	(headers stream head :end-headers t :end-stream t))

      (receive-loop net conn))))

(in-package :cl-async-ssl)

(defparameter *npn-arg-fo* nil)
(defparameter *npn-str-fo* nil)

(defun tcp-ssl-server (bind-address port read-cb event-cb
                       &key connect-cb (backlog -1) stream
                            (ssl-method 'cl+ssl::ssl-v23-server-method)
			    certificate key password dhparams npn)
  "Start a TCP listener, and wrap incoming connections in an SSL handler.
   Returns a tcp-server object, which can be closed with close-tcp-server.

   If you need a self-signed cert/key to test with:
     openssl genrsa -out pkey 2048
     openssl req -new -key pkey -out cert.req
     openssl x509 -req -days 3650 -in cert.req -signkey pkey -out cert"
  ;; make sure SSL is initialized
  (cl+ssl:ensure-initialized :method ssl-method)

  ;; create the server and grab its data-pointer
  (let* ((server (tcp-server bind-address port
                             read-cb event-cb
                             :connect-cb connect-cb
                             :backlog backlog
                             :stream stream))
         (data-pointer (tcp-server-data-pointer server)))
    ;; overwrite the accept callback from tcp-accept-cb -> tcp-ssl-accept-cb
    (le:evconnlistener-set-cb (tcp-server-c server)
                              (cffi:callback tcp-ssl-accept-cb)
                              data-pointer)
    ;; create a server context
    (let* ((ssl-ctx (cl+ssl::ssl-ctx-new (funcall ssl-method)))
           (ssl-server (change-class server 'tcp-ssl-server :ssl-ctx ssl-ctx)))
      ;; make sure if there is a cert password, it's used
      (cl+ssl::with-pem-password (password)
        (cl+ssl::ssl-ctx-set-default-passwd-cb ssl-ctx (cffi:callback cl+ssl::pem-password-callback))

        ;; load the cert
        (when certificate
          (let ((res (cffi:foreign-funcall "SSL_CTX_use_certificate_chain_file"
                                           :pointer ssl-ctx
                                           :string (namestring certificate)
                                           :int)))
            (unless (= res 1)
              (error (format nil "Error initializing certificate: ~a."
                             (last-ssl-error))))))

        ;; load the private key
        (when key
          (let ((res (cffi:foreign-funcall "SSL_CTX_use_PrivateKey_file"
                                           :pointer ssl-ctx
                                           :string (namestring key)
                                           :int cl+ssl::+ssl-filetype-pem+
                                           :int)))
            (unless (= res 1)
              (error (format nil "Error initializing private key file: ~a."
                             (last-ssl-error)))))))

      ;; setup dhparams
      (when dhparams
	(cl+ssl::init-dhparams dhparams))

      ;; setup next protocol negotiation
      (let ((nps (format nil "~{~C~A~}" (mapcan (lambda (n) (list (code-char (length n)) n)) npn))))
	(when *npn-arg-fo* (cffi:foreign-free *npn-arg-fo*))
	(setf *npn-arg-fo* (cffi:foreign-alloc '(:struct cl+ssl::server-tlsextnextprotoctx)))
	(when *npn-str-fo* (cffi:foreign-string-free *npn-str-fo*))
	(setf *npn-str-fo* (cffi:foreign-string-alloc nps))
	(cffi:with-foreign-slots ((cl+ssl::data cl+ssl::len) *npn-arg-fo* (:struct cl+ssl::server-tlsextnextprotoctx))
	  (setf cl+ssl::data *npn-str-fo*
		cl+ssl::len (length nps)))
	(cffi:foreign-funcall "SSL_CTX_set_next_protos_advertised_cb"
			      :pointer ssl-ctx
			      :pointer (cffi:callback cl+ssl::lisp-server-next-proto-cb)
			      :pointer *npn-arg-fo*
			      :void))

      ;; adjust the data-pointer's data a bit
      (attach-data-to-pointer data-pointer
                              (list :server server
                                    :ctx ssl-ctx))
      ssl-server)))

(in-package :cl-http2-protocol-example)

(defun pump-connection (conn)
  (when (pump-stream-queues conn 2)
    (delay (lambda () (pump-connection conn)))))

(defmacro options-with-defaults (list &body body)
  (let ((new (gensym "OPTIONS")))
    `(let ((,new (copy-list ,list)))
       ,@(loop for (key value) in body collect `(ensuref (getf options ,key) ,value))
       ,new)))

(defun example-server (&key (interface "0.0.0.0") (port 8080)
			 (cl-async-server 'tcp-ssl-server)
			 cl-async-options
			 request-handler
			 entry-handler
			 exit-handler
			 (debug-mode *debug-mode*)
			 (dump-bytes *dump-bytes*))
  (let ((*debug-mode* debug-mode)
	(*dump-bytes* dump-bytes)
	sockets)
    (format t "Starting server on port ~D~%" port)
    (with-event-loop ()
      (when entry-handler (as:delay entry-handler))
      (apply cl-async-server
	     interface port
	     #'example-server-read
	     #'example-server-event
	     :connect-cb
	     (lambda (socket)
	       (format t "New TCP connection received!~%")
	       (setf sockets (cons socket (delete-if #'socket-closed-p sockets)))
	       (setf (socket-data socket) (example-server-accepted-socket socket request-handler)))
	     (options-with-defaults cl-async-options
	       (:ssl-method  'cl+ssl::ssl-tlsv1.2-method)
	       (:npn         *next-protos-spec*)
	       (:key         *server-key-file*)
	       (:certificate *server-cert-file*)
	       (:dhparams    *server-dhparams-file*)))
      (add-event-loop-exit-callback
       (lambda () (mapc #'close-socket (delete-if #'socket-closed-p sockets))))
      (when exit-handler (add-event-loop-exit-callback exit-handler)))))

(defun example-server-read (socket bytes)
  (maybe-dump-bytes :recv bytes)
  (let ((conn (socket-data socket)))
    (handler-case-unless *debug-mode*
	(connection<< conn bytes)
      (t (e)
	 (report-error e)
	 (close-socket socket)))
    (delay (lambda () (pump-connection conn)))))

(defun example-server-event (ev)
  (format t "example-server-event: ~S~%" ev)
  (let ((socket (tcp-socket ev)))
    (labels ((done-with-socket ()
	       (http2::shutdown-connection (socket-data socket))
	       (unless (socket-closed-p socket)
		 (close-socket socket))))
      (handler-case
	  (error ev)
	(tcp-error ()
	  (delay #'done-with-socket))
	(tcp-eof ()
	  (delay #'done-with-socket))
	(tcp-timeout ()
	  (delay #'done-with-socket))))))

(defun example-server-accepted-socket (socket request-handler)
  (let ((conn (make-instance 'server)))
    (on conn :frame
	(lambda (bytes)
	  (maybe-dump-bytes :send bytes)
	  (write-socket-data socket (buffer-data bytes))))
      
    (on conn :goaway
	(lambda (s e m)
	  (declare (ignore s))
	  (format t "goaway error message, code: ~D, message: ~S~%" e (if (bufferp m) (buffer-string m) m))))

    (on conn :stream
	(lambda (stream)
	  (format t "connection has new stream~%")

	  (let (req
		(buffer (make-instance 'buffer)))
	      
	    (on stream :active
		(lambda ()
		  (format t "client opened new stream~%")))

	    (on stream :close
		(lambda (e)
		  (if e
		      (format t "stream closed, error: ~A~%" e)
		      (format t "stream closed~%"))))
	      
	    (on stream :headers
		(lambda (h)
		  (setf req h)
		  (format t "request headers associated with stream ~S:~%~S~%" stream req)
		  (macrolet ((req-header (name) `(cdr (assoc ,name req :test #'string=))))
		    (when (string= (req-header ":method") "CONNECT") ; END_STREAM won't be set so handle here
		      (if request-handler
			  (funcall request-handler stream req)
			  (headers stream `((":status" . "405") ("allow" . "GET, POST")) :end-stream t))))))
	      
	    (on stream :data
		(lambda (d)
		  (format t "payload chunk: <<~A>>~%" d)
		  (buffer<< buffer (getf d :payload))))
	    
	    (on stream :window
		(lambda (w)
		  (format t "stream window update received: ~A~%" w)))

	    (on stream :half-close
		(if request-handler
		    (lambda ()
		      (when (eq (stream-closed stream) :half-closed-remote)
			(funcall request-handler stream req)))
		    (lambda ()
		      (when (eq (stream-closed stream) :half-closed-remote)
			(macrolet ((req-header (name) `(cdr (assoc ,name req :test #'string=))))
			  (format t "client closed its end of the stream~%")

			  (let ((method (req-header ":method"))
				content)
			    (switch (method :test #'string=)
			      ("GET"
			       (format t "Received GET request~%")
			       (setf content (buffer-simple "Hello HTTP 2.0! GET request")))
			      ("POST"
			       (let ((post-str (buffer-string buffer)))
				 (format t "Received POST request, payload: ~A~%" post-str)
				 (setf content (buffer-simple "Hello HTTP 2.0! POST payload: " post-str))))
			      (otherwise
			       ;; should be better handled
			       (setf content (buffer-simple ""))))
		      
			    (headers stream `((":status"        . "200")
					      ("content-length" . ,(format nil "~D" (buffer-size content)))
					      ("content-type"   . "text/plain"))
				     :end-stream nil)
		      
			    ;; split content into multiple DATA frames
			    (data stream (buffer-slice! content 0 5) :end-stream nil)
			    (data stream content))))))))))
    conn))

(defmacro def-test-server (name &body body)
  (with-gensyms (rh)
    `(defun ,name (&key args)
       (flet ((,rh (req)) ,@body)
	 (apply #'example-server :request-handler #',rh args)))))
