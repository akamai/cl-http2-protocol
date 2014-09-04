; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol-example)

(defparameter *key-pathname*         (merge-pathnames "cl-http2-protocol/" (user-homedir-pathname)))
(defparameter *server-key-file*      (merge-pathnames "mykey.pem"  	  *key-pathname*))
(defparameter *server-cert-file*     (merge-pathnames "mycert.pem" 	  *key-pathname*))
(defparameter *server-dhparams-file* (merge-pathnames "dhparams.2048.pem" *key-pathname*))

(defparameter *next-protos-spec* '("h2-14"))

(defparameter *dump-bytes*        t   "Set to T to dump bytes as they are received/sent. Set to NIL to quieten.")
(defparameter *dump-bytes-stream* t   "Set to T for stdout, or a stream value.")
(defparameter *dump-bytes-base*   10  "Set to 10 for decimal bytes values or 16 for hexadecimal.")
(defparameter *dump-bytes-hook*   nil "Set to NIL to pass bytes to the printer, or 'VECTOR-INSPECT or a function.")

(defparameter *verbose-mode*      t   "Set to T for more parsed information about what's happening.")

(defmacro maybe-dump-bytes (type bytes)
  "Provide the functionality to inspect what's coming in and out in a debugging session."
  `(when *dump-bytes*
     (let ((*print-base* *dump-bytes-base*))
       (format *dump-bytes-stream* ,(concatenate 'string "http2 " (string-downcase type) ": ~A~%")
	       (if *dump-bytes-hook* (funcall *dump-bytes-hook* ,bytes) ,bytes)))))

(defmacro say (format-control &rest format-args)
  `(when *verbose-mode*
     (format t ,format-control ,@format-args)))

(defmacro say-important (format-control &rest format-args)
  `(format t ,format-control ,@format-args))

(defun close-socket-if-open (socket)
  "Close the SOCKET provided if non-NIL and SOCKET-CLOSED-P returns NIL."
  (when socket
    (unless (socket-closed-p socket)
      (close-socket socket))))

(defun example-client (&key uri proxy-uri
			 (cl-async-connect 'tcp-ssl-connect)
			 cl-async-options
			 (ssl-method 'cl+ssl::ssl-tlsv1.2-method)
			 request-generator
			 entry-handler
			 exit-handler
			 (debug-mode *debug-mode*)
			 (dump-bytes *dump-bytes*)
			 (npn *next-protos-spec*)
			 sni)
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
    (say "About to connect socket to ~A port ~A...~%" connect-host connect-port)
    (cl+ssl:ensure-initialized :method ssl-method)
    (with-event-loop ()
      (when entry-handler (as:delay entry-handler))
      (let ((npn-cleanup (cl-async-ssl::init-ssl-npn :client cl+ssl::*ssl-global-context* npn))
	    (sni-cleanup (cl-async-ssl::init-ssl-sni :client cl+ssl::*ssl-global-context* (or sni (uri-host (or proxy-uri uri))))))
	(add-event-loop-exit-callback npn-cleanup)
	(add-event-loop-exit-callback sni-cleanup))
      (let ((client-ctx (cl-async-ssl::init-ssl-client-context cl+ssl::*ssl-global-context*)))
	(apply cl-async-connect
	       connect-host connect-port
	       #'read-handler
	       #'event-handler
	       :ssl-ctx client-ctx
	       :connect-cb
	       (lambda (new-socket)
		 (setf socket new-socket)
		 (say "Connected (~A)!~%" (verify-ssl socket client-ctx))
		 (setf (socket-data socket) (example-client-connected-socket socket uri request-generator)))
	       cl-async-options)
	(add-event-loop-exit-callback (lambda () (close-socket-if-open socket)))
	(when exit-handler (add-event-loop-exit-callback exit-handler))))
    nil))

(defun verify-ssl (socket ssl-ctx)
  "Compare the negotiated SSL NPN string to the list of acceptable protocols."
  (let ((npn (cl+ssl::get-next-proto-negotiated-from-handle ssl-ctx)))
    (unless (member npn *next-protos-spec* :test #'string=)
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
	     (close-socket-if-open socket)))
      (handler-case
	  (error event)
	(tcp-error ()
	  (say "TCP Error~%")
	  (done-with-socket))
	(tcp-eof ()
	  (say "TCP EOF~%")
	  (done-with-socket))
	(tcp-timeout ()
	  (say "TCP Timeout~%")
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
		  (say "promise headers: ~S~%" h)))
	    (on promise :data
		(lambda (d)
		  (say "promise data chunk: ~D~%" (length d))))))
      
      (on stream :close
	  (lambda (e)
	    (if e
		(say "stream closed, error: ~A~%" e)
		(say "stream closed~%"))
	    (close-socket socket)))

      (on stream :half-close
	  (lambda ()
	    (say "closing client-end of the stream~%")))

      (on stream :headers
	  (lambda (h)
	    (say "response headers: ~S~%" h)))

      (on stream :data
	  (lambda (d)
	    (say "response data chunk: <<~A>>~%" (buffer-string (getf d :payload)))))

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
		 (say "Sending HTTP/2 request~%~S~%" head)
		 (headers stream head :end-headers t :end-stream t))))))
      conn)))

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
			 (ssl-method 'cl+ssl::ssl-tlsv1.2-method)
			 request-handler
			 entry-handler
			 exit-handler
			 (debug-mode *debug-mode*)
			 (dump-bytes *dump-bytes*))
  (let ((*debug-mode* debug-mode)
	(*dump-bytes* dump-bytes)
	sockets)
    (say "Starting server on port ~D~%" port)
    (with-event-loop ()
      (when entry-handler (as:delay entry-handler))
      (apply cl-async-server
	     interface port
	     (lambda (socket bytes)
	       (read-handler socket bytes))
	     #'event-handler
	     :connect-cb
	     (lambda (socket)
	       (say "New TCP connection received!~%")
	       (setf sockets (cons socket (delete-if #'socket-closed-p sockets)))
	       (setf (socket-data socket) (example-server-accepted-socket socket request-handler)))
	     (options-with-defaults cl-async-options
	       (:ssl-method  ssl-method)
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
	  (say "goaway error message from peer: code: ~D, message: ~S~%" e (if (bufferp m) (buffer-string m) m))))

    (on conn :stream
	(lambda (stream)
	  (say "connection has new stream~%")

	  (let (req
		(buffer (make-instance 'buffer)))
	      
	    (on stream :active
		(lambda ()
		  (say "client opened new stream~%")))

	    (on stream :close
		(lambda (e)
		  (if e
		      (say "stream closed, error: ~A~%" e)
		      (say "stream closed~%"))))
	      
	    (on stream :headers
		(lambda (h)
		  (setf req h)
		  ;; (format t "request headers associated with stream ~S:~%~S~%" stream req)
		  (macrolet ((req-header (name) `(cdr (assoc ,name req :test #'string=))))
		    (say-important ":authority: ~S, :path: ~S~%" (req-header ":authority") (req-header ":path"))
		    (when (string= (req-header ":method") "CONNECT") ; END_STREAM won't be set so handle here
		      (if request-handler
			  (funcall request-handler stream req)
			  (headers stream `((":status" . "405") ("allow" . "GET, POST")) :end-stream t))))))
	      
	    (on stream :data
		(lambda (d)
		  (say "payload chunk: <<~A>>~%" d)
		  (buffer<< buffer (getf d :payload))))
	    
	    (on stream :window
		(lambda (w)
		  (say "stream window update received: ~A~%" w)))

	    (on stream :half-close
		(if request-handler
		    (lambda ()
		      (when (eq (stream-closed stream) :half-closed-remote)
			(funcall request-handler stream req)))
		    (lambda ()
		      (when (eq (stream-closed stream) :half-closed-remote)
			(macrolet ((req-header (name) `(cdr (assoc ,name req :test #'string=))))
			  (say "client closed its end of the stream~%")

			  (let ((method (req-header ":method"))
				content)
			    (switch (method :test #'string=)
			      ("GET"
			       (say "Received GET request~%")
			       (let ((path (req-header ":path")))
				 (setf content
				       (buffer-simple
					(switch (path :test #'string=)
					  ("/"
					   (format nil "Hello HTTP 2.0! GET request~%"))
					  ("/elstat"
					   (let ((file (format nil "/tmp/dels~D" (random 1000000000))))
					     (format nil "Event Loop Status~%~%~A~%" (as:dump-event-loop-status file))))
					  (otherwise
					   (format nil "Received GET request for path: ~S~%" path)))))))
			      ("POST"
			       (let ((post-str (buffer-string buffer)))
				 (say "Received POST request, payload: ~A~%" post-str)
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
