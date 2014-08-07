; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol-example)

; see also net.lisp which is part of the example and not strictly necessary

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

(defun example-server (&key (interface "0.0.0.0") (port 8080)
			 (net nil net-arg-p) net-options (secure nil secure-arg-p)
			 request-handler
			 (debug-mode *debug-mode*)
			 (dump-bytes *dump-bytes*))
  (assert (or (not net-arg-p) (not secure-arg-p)) (net secure) "Provide either :NET or :SECURE")
  (ensuref net (apply #'make-instance (if secure
					  'net-ssl
					  #+sbcl 'net-plain-sb-bsd-sockets
					  #-sbcl 'net-plain-usocket)
		      net-options))
  (assert (typep net 'net) (net) ":NET object must be of type NET")
  (let ((*debug-mode* debug-mode)
	(*dump-bytes* dump-bytes))
    (handler-case
	(progn
	  (format t "Starting server on port ~D~%" port)
	  (net-socket-listen net interface port)
	  (unwind-protect
	       (loop
		  (handler-case-unless *debug-mode*
		      (example-server-inner net request-handler)
		    (t (e)
		       (report-error e)
		       (net-socket-close net))))
	    (net-socket-close net)
	    (net-socket-shutdown net)))
      (address-in-use-error ()
	(format t "Address already in use.~%")))))

(defun example-server-inner (net request-handler)
  (net-socket-accept net)
  (format t "New TCP connection!~%")
  (net-socket-prepare-server net)
  (handler-case
      (example-server-accepted-socket net request-handler)
    (connection-reset-error ()
      (format t "Connection reset.~%")
      (net-socket-close net))
    (end-of-file ()
      (format t "End of file.~%")
      (net-socket-close net))))

(defun example-server-accepted-socket (net request-handler)
  (let ((conn (make-instance 'server)))
    (on conn :frame
	(lambda (bytes)
	  ; (format t "transmitting frame now~%")
	  (send-bytes net (buffer-data bytes))))
      
    (on conn :goaway
	(lambda (s e m)
	  (declare (ignore s))
	  (format t "goaway error message, code: ~D, message: ~S~%" e (if (bufferp m) (buffer-string m) m))))

    (on conn :stream
	(lambda (stream)
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
		  (format t "request headers: ~S~%" h)
		  ))
	      
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
		      (funcall request-handler stream req))
		    (lambda ()
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
			  (data stream content)))))))))

    (format t "Entering receive loop~%")
    (restart-case
	(receive-loop net conn)
      ;; provide a general restart that may be useful during debugging:
      (goaway ()
	:report "Send GOAWAY INTERNAL_ERROR."
	(goaway conn :internal-error)
	(net-socket-close net)))
    (format t "Leaving receive loop~%")))

(defmacro def-test-server (name &body body)
  (with-gensyms (rh)
    `(defun ,name (&key args)
       (flet ((,rh (req)) ,@body)
	 (apply #'example-server :request-handler #',rh args)))))
