; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol-example)

(defparameter *key-pathname*         (merge-pathnames "cl-http2-protocol/" (user-homedir-pathname)))
(defparameter *server-key-file*      (merge-pathnames "mykey.pem"  	  *key-pathname*))
(defparameter *server-cert-file*     (merge-pathnames "mycert.pem" 	  *key-pathname*))
(defparameter *server-dhparams-file* (merge-pathnames "dhparams.2048.pem" *key-pathname*))

(defparameter *next-protos-spec* '("h2-13"))

(defparameter *dump-bytes*        t   "Set to T to dump bytes as they are received/sent. Set to NIL to quieten.")
(defparameter *dump-bytes-stream* t   "Set to T for stdout, or a stream value.")
(defparameter *dump-bytes-base*   10  "Set to 10 for decimal bytes values or 16 for hexadecimal.")
(defparameter *dump-bytes-hook*   nil "Set to NIL to pass bytes to the printer, or 'VECTOR-INSPECT or a function.")

(defmacro maybe-dump-bytes (type bytes)
  "Provide the functionality to inspect what's coming in and out in a debugging session."
  `(when *dump-bytes*
     (let ((*print-base* *dump-bytes-base*))
       (format *dump-bytes-stream* ,(concatenate 'string "http2 " (string-downcase type) ": ~A~%")
	       (if *dump-bytes-hook* (funcall *dump-bytes-hook* ,bytes) ,bytes)))))

(defun socket-close-if-open (socket)
  "Close the SOCKET provided if non-NIL and SOCKET-CLOSED-P returns NIL."
  (when socket
    (unless (socket-closed-p socket)
      (close-socket socket))))

(defun example-client (&key uri proxy-uri
			 (cl-async-connect 'tcp-ssl-connect)
			 cl-async-options
			 request-generator
			 entry-handler
			 exit-handler
			 (debug-mode *debug-mode*)
			 (dump-bytes *dump-bytes*))
  "Exercise HTTP/2 to fetch a URI."
  (check-type uri (or string puri:uri))
  (when (not (uri-p uri)) (setf uri (parse-uri uri)))
  (check-type (uri-scheme uri) (member :http :https))
  (check-type proxy-uri (or null string puri:uri))
  (when (and proxy-uri (not (uri-p proxy-uri))) (setf proxy-uri (parse-uri proxy-uri)))
  (when proxy-uri
    (check-type (uri-scheme proxy-uri) (member :http :https)))
  (let* ((connect-uri (or proxy-uri uri))
	 (connect-host (uri-host connect-uri))
	 (connect-port (or (uri-port connect-uri) (if (eq (uri-scheme connect-uri) :https) 443 80)))
	 (*debug-mode* debug-mode)
	 (*dump-bytes* dump-bytes)
	 socket)
    (format t "About to connect socket to ~A port ~A...~%" connect-host connect-port)
    (cl+ssl:ensure-initialized :method 'cl+ssl::ssl-tlsv1.2-method)
    (with-event-loop ()
      (when entry-handler (as:delay entry-handler))
      (let ((ssl-ctx (cl-async-ssl::init-ssl-client-context cl+ssl::*ssl-global-context*)))
	(let ((npn-cleanup (cl-async-ssl::init-ssl-npn ssl-ctx *next-protos-spec*)))
	  (add-event-loop-exit-callback npn-cleanup))
	(apply cl-async-connect
	       connect-host connect-port
	       #'read-handler
	       #'event-handler
	       :ssl-ctx ssl-ctx
	       :connect-cb
	       (lambda (new-socket)
		 (setf socket new-socket)
		 (format t "Connected (~A)!~%" (verify-ssl socket ssl-ctx))
		 (setf (socket-data socket) (example-client-connected-socket socket uri request-generator)))
	       cl-async-options))
      (add-event-loop-exit-callback (lambda () (socket-close-if-open socket)))
      (when exit-handler (add-event-loop-exit-callback exit-handler)))
    nil))

(defun verify-ssl (socket ssl-ctx)
  "Compare the negotiated SSL NPN string to the list of acceptable protocols."
  (let ((npn (cl+ssl::get-next-proto-negotiated-from-handle ssl-ctx)))
    (unless (member npn *next-protos-spec*)
      (wrong-protocol socket npn *next-protos-spec*))
    npn))

(defun wrong-protocol (socket protocol good-protocols)
  "Signal that the wrong protocol has been negotiated so the connection cannot be used."
  (with-simple-restart (try-anyway "Try HTTP/2 anyway")
    (error 'http2-not-started
	   :other-protocol protocol
	   :format-control "Protocol ~S negotiated instead of one of ~S on ~S."
	   :format-arguments (list protocol good-protocols socket))))

(defun read-handler (socket bytes)
  "Handle incoming bytes."
  (maybe-dump-bytes :recv bytes)
  (let ((conn (socket-data socket)))
    (handler-case-unless *debug-mode*
	(connection<< conn bytes)
      (t (e)
	 (report-error e)
	 (close-socket socket)))
    (delay (lambda () (pump-connection conn)))))

(defun event-handler (event)
  "Handle a socket event."
  (let* ((socket (tcp-socket event))
	 (conn (socket-data socket)))
    (flet ((done-with-socket ()
	     (when conn
	       (shutdown-connection conn))
	     (socket-close-if-open socket)))
      (handler-case
	  (error event)
	(tcp-error ()
	  (format t "TCP Error~%")
	  (done-with-socket))
	(tcp-eof ()
	  (format t "TCP EOF~%")
	  (done-with-socket))
	(tcp-timeout ()
	  (format t "TCP Timeout~%")
	  (done-with-socket))))))

(defun example-client-connected-socket (socket uri request-generator)
  (let ((conn (make-instance 'client)))
    (on conn :frame
	(lambda (bytes)
	  (maybe-dump-bytes :send (buffer-data bytes))
	  (write-socket-data socket (buffer-data bytes))))
    
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
	    (close-socket socket)))

      (on stream :half-close
	  (lambda ()
	    (format t "closing client-end of the stream~%")))

      (on stream :headers
	  (lambda (h)
	    (format t "response headers: ~S~%" h)))

      (on stream :data
	  (lambda (d)
	    (format t "response data chunk: <<~A>>~%" (buffer-string (getf d :payload)))))

      (delay
       (if request-generator
	   (lambda ()
	     (funcall request-generator stream uri))
	   (lambda ()
	     (let* ((scheme-str (string-downcase (string (uri-scheme uri))))
		    (default-port (if (eq (uri-scheme uri) :https) 443 80))
		    (port (or (uri-port uri) default-port))
		    (authority-str (format nil "~A~@[:~A~]"
					   (uri-host uri)
					   (if (= port default-port) nil port)))
		    (path-str (or (uri-path uri) "/"))
		    (head `((":scheme"    . ,scheme-str)
			    (":method"    . "GET")
			    (":authority" . ,authority-str)
			    (":path"      . ,path-str)
			    ("accept"     . "*/*")
			    ("user-agent" . "cl-http2-protocol HTTP/2 Common Lisp Library Test Agent"))))
	       (with-simple-restart (abort-request "Abort the HTTP/2 request")
		 (format t "Sending HTTP/2 request~%~S~%" head)
		 (headers stream head :end-headers t :end-stream t))))))
      conn)))

(in-package :cl-async)

;; override to fix a bug with connect-cb WHEN statement handling (should go upstream)
(define-c-callback tcp-event-cb :void ((bev :pointer) (events :short) (data-pointer :pointer))
  "Called whenever anything happens on a TCP socket. Ties into the anonymous
   callback system to track failures/disconnects."
  (let* ((event nil)
         (dns-base (deref-data-from-pointer data-pointer))
         (bev-data (deref-data-from-pointer bev))
         (socket (getf bev-data :socket))
         (callbacks (get-callbacks data-pointer))
         (event-cb (getf callbacks :event-cb))
         (connect-cb (getf callbacks :connect-cb)))
    (check-type socket cl-async:socket)
    (catch-app-errors event-cb
      (unwind-protect
	   ;; if we just connected and we have a connect-cb, call it (only for
	   ;; outgoing connections though, since incoming are handled in the
	   ;; accept-cb)
	   (when (and connect-cb
		      (plusp (logand events le:+bev-event-connected+))
		      (let ((dir (socket-direction socket)))
			(or (eq dir 'out) (string= (symbol-name dir) "OUT"))))
	     (funcall connect-cb socket))
        ;; process any errors we received
        (cond
          ((< 0 (logand events (logior le:+bev-event-error+
                                       le:+bev-event-timeout+)))
           (multiple-value-bind (errcode errstr) (get-last-tcp-err)
             (let ((dns-err (le:bufferevent-socket-get-dns-error bev)))
               (cond
                 ;; DNS error
                 ((and (< 0 (logand events le:+bev-event-error+))
                       (not (zerop dns-err)))
                  (setf event (make-instance 'dns-error
                                             :code dns-err
                                             :msg (le:evutil-gai-strerror dns-err)))
                  (release-dns-base))

                 ;; socket timeout
                 ((< 0 (logand events le:+bev-event-timeout+))
                  (setf event (make-instance 'tcp-timeout :socket socket :code -1 :msg "Socket timed out")))

                 ;; connection reset by peer
                 ((or (eq errcode 104)
                      (< 0 (logand events le:+bev-event-eof+)))
                  (setf event (make-instance 'tcp-eof :socket socket)))

                 ;; since we don't know what the error was, just spawn a general
                 ;; error.
                 ((< 0 errcode)
                  (setf event (make-instance 'tcp-error :socket socket :code errcode :msg errstr)))
                 ;; libevent signaled an error, but nothing actually happened
                 ;; (that we know of anyway). ignore...
					;(t
					; (setf event (make-instance 'tcp-error :socket socket :code events :msg (format nil "Unkonwn error (~a): ~a" events errcode))))
                 ))))
          ;; peer closed connection.
          ((< 0 (logand events le:+bev-event-eof+))
           (setf event (make-instance 'tcp-eof :socket socket)))
          ((and dns-base
                (< 0 (logand events le:+bev-event-connected+))         
                (not (cffi:null-pointer-p dns-base)))
           (release-dns-base)))
        (when event
          (unwind-protect
	       (when event-cb (run-event-cb event-cb event))
            ;; if the app closed the socket in the event cb (perfectly fine),
            ;; make sure we don't trigger an error trying to close it again.
            (handler-case (close-socket socket)
              (socket-closed () nil))))))))

(in-package :cl-async-ssl)

(defun init-ssl-npn (ssl-ctx npn)
  "Setup NPN (next protocol negotiation) on an SSL context.
Returns a cleanup closure to be called upon disconnect."
  (let* ((spec-str (format nil "~{~C~A~}" (loop for p in npn collect (code-char (length p)) collect p)))
	 (npn-arg-fo (cffi:foreign-alloc '(:struct cl+ssl::server-tlsextnextprotoctx)))
	 (npn-str-fo (cffi:foreign-string-alloc spec-str)))
    (cffi:with-foreign-slots ((cl+ssl::data cl+ssl::len) npn-arg-fo (:struct cl+ssl::server-tlsextnextprotoctx))
      (setf cl+ssl::data npn-str-fo
	    cl+ssl::len (length spec-str)))
    (cffi:foreign-funcall "SSL_CTX_set_next_protos_advertised_cb"
			  :pointer ssl-ctx
			  :pointer (cffi:callback cl+ssl::lisp-server-next-proto-cb)
			  :pointer npn-arg-fo
			  :void)
    (lambda ()
      (cffi:foreign-free npn-arg-fo)
      (cffi:foreign-string-free npn-str-fo))))

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
       ,@(loop for (key value) in body collect `(ensuref (getf ,new ,key) ,value))
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
	     #'read-handler
	     #'event-handler
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
      (add-event-loop-exit-callback (lambda () (mapc #'close-socket-if-open sockets)))
      (when exit-handler (add-event-loop-exit-callback exit-handler)))))

(defun example-server-accepted-socket (socket request-handler)
  (let ((conn (make-instance 'server)))
    (on conn :frame
	(lambda (bytes)
	  (maybe-dump-bytes :send (buffer-data bytes))
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
			       (let ((path (req-header ":path")))
				 (setf content
				       (buffer-simple
					(switch (path :test #'string=)
					  ("/"
					   "Hello HTTP 2.0! GET request~%")
					  ("/status"
					   (let ((file (format nil "/tmp/dels~D" (random 1000000000))))
					     (format nil "Event Loop Status~%~%~A~%" (as:dump-event-loop-status file))))
					  (otherwise
					   (format nil "Received GET request for path: ~S~%" path)))))))
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
