; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol-example)

; see also net.lisp which is part of the example and not strictly necessary

(defun example-client (uri &key (net nil net-arg-p) (secure nil secure-arg-p))
  (assert (or (not net-arg-p) (not secure-arg-p)) (net secure) "Provide either :NET or :SECURE")
  (when (not (uri-p uri))
    (setf uri (parse-uri uri)))
  (ensuref net (make-instance (if (or secure (eq (uri-scheme uri) :https))
				  'net-ssl
				  #+sbcl 'net-plain-sb-bsd-sockets
				  #-sbcl 'net-plain-usocket)))
  (assert (typep net 'net) (net) ":NET object must be of type NET")
  (handler-case
      (example-client-inner net uri)
    (connection-refused-error ()
      (format t "Connection refused: ~A~%" uri))))

(defun example-client-inner (net uri)
  (format t "About to connect socket to ~A port ~A...~%"
	  (uri-host uri) (or (uri-port uri) 443))
  (net-socket-connect net (uri-host uri) (or (uri-port uri) (if (eq (uri-scheme uri) :https) 443 80)))
  (format t "Connected!~%")
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
	    (format t "stream closed (error, if any: ~A)~%" e)
	    (error 'end-of-file :stream (net-socket net))))

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
		    (":method" . "get")
		    (":host"   . ,(format nil "~A:~A" (uri-host uri) (or (uri-port uri) 80)))
		    (":path"   . ,(or (uri-path uri) "/"))
		    ("accept"  . "*/*"))))
	(format t "Sending HTTP 2.0 request~%")
	(headers stream head :end-stream t))

      (receive-loop net conn))))

(defun example-server (&key (interface "0.0.0.0") (port 8080)
		       (net nil net-arg-p) (secure nil secure-arg-p))
  (assert (or (not net-arg-p) (not secure-arg-p)) (net secure) "Provide either :NET or :SECURE")
  (ensuref net (make-instance (if secure
				  'net-ssl
				  #+sbcl 'net-plain-sb-bsd-sockets
				  #-sbcl 'net-plain-usocket)))
  (assert (typep net 'net) (net) ":NET object must be of type NET")
  (handler-case
      (progn
	(format t "Starting server on port ~D~%" port)
	(net-socket-listen net interface port)
	(unwind-protect
	     (loop (example-server-inner net))
	  (net-socket-close net)
	  (net-socket-shutdown net)))
    (address-in-use-error ()
      (format t "Address already in use.~%"))))

(defun example-server-inner (net)
  (net-socket-accept net)
  (format t "New TCP connection!~%")
  (net-socket-prepare-server net)
  (handler-case
      (example-server-accepted-socket net)
    (connection-reset-error ()
      (format t "Connection reset.~%")
      (net-socket-close net))
    (end-of-file ()
      (format t "End of file.~%")
      (net-socket-close net))))

(defun example-server-accepted-socket (net)
  (let ((conn (make-instance 'server)))
    (on conn :frame
	(lambda (bytes)
	  (send-bytes net (buffer-data bytes))))
      
    (on conn :stream
	(lambda (stream)
	  (let (req
		(buffer (make-instance 'buffer)))
	      
	    (on stream :active
		(lambda ()
		  (format t "client opened new stream~%")))
	    (on stream :close
		(lambda (e)
		  (format t "stream closed (error, if any: ~A)~%" e)))
	      
	    (on stream :headers
		(lambda (h)
		  (setf req h)
		  (format t "request headers: ~S~%" h)))
	      
	    (on stream :data
		(lambda (d)
		  (format t "payload chunk: <<~A>>~%" d)
		  (buffer<< buffer d)))
	      
	    (on stream :half-close
		(lambda ()
		  (macrolet ((req-header (name) `(cdr (assoc ,name req :test #'string=))))
		    (format t "client closed its end of the stream~%")
		    
		    (let (response)
		      (if (string= (req-header ":method") "post")
			  (let ((post-str (buffer-string buffer)))
			    (format t "Received POST request, payload: ~A~%" post-str)
			    (setf response (buffer-simple "Hello HTTP 2.0! POST payload: " post-str)))
			  (progn
			    (format t "Received GET request~%")
			    (setf response (buffer-simple "Hello HTTP 2.0! GET request"))))
		      
		      (headers stream `((":status"        . "200")
					("content-length" . ,(format nil "~D" (buffer-size response)))
					("content-type"   . "text/plain"))
			       :end-stream nil)
		      
		      ; split response into multiple DATA frames
		      (data stream (buffer-slice! response 0 5) :end-stream nil)
		      (data stream response))))))))

    (format t "Entering receive loop~%")
    (receive-loop net conn)
    (format t "Leaving receive loop~%")))
