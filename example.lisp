(in-package :http2)

(defparameter *server-key-file* "/home/ubuntu/ruby_example/keys/mykey.pem")
(defparameter *server-cert-file* "/home/ubuntu/ruby_example/keys/mycert.pem")
(defparameter *next-protos-spec* '("HTTP-draft-06/2.0"))

; Port note: The networking diverges a little bit from the Ruby
; examples at first, then DO-CLIENT and DO-SERVER are similar as
; functions to what ruby_examples/client.rb, ruby_examples/server.rb
; do as executables.

; To abstract networking differences when we wrap in SSL versus when
; we don't, a NET class is defined that can NET-SOCKET-LISTEN,
; NET-SOCKET-ACCEPT, NET-SOCKET-PREPARE-SERVER, NET-SOCKET-CLOSE,
; NET-SOCKET-SHUTDOWN, NET-WRITE-VECTOR, NET-READ-VECTOR (non
; blocking), and NET-FINISH-OUTPUT. There are two versions, one for
; SSL, NET-SSL, and one for plain, NET-PLAIN. We normalize calls,
; housekeeping, and differences in exceptions thrown in this manner.
;
; Because eventually HTTP/2.0 will settle on whether TLS is required
; and related issues, this is likely to be simplified in the future.

(defclass net ()
  ((listener :accessor net-listener :initform nil)
   (raw-socket :accessor net-raw-socket :initform nil)
   (socket :accessor net-socket :initform nil))
  (:documentation "Abstraction for multiple network implementations"))

(defclass net-ssl (net) ()
  (:documentation "CL+SSL wrapping USOCKET"))

(defmethod net-socket-listen ((net net-ssl) host port)
  (with-slots (listener) net
    (setf listener (socket-listen
		    host port
		    :reuse-address t
		    :backlog 8
		    :element-type '(unsigned-byte 8)))))

(defmethod net-socket-accept ((net net-ssl))
  (with-slots (raw-socket listener) net
    (setf raw-socket (socket-accept listener))))

(defmethod net-socket-prepare-server ((net net-ssl))
  (with-slots (raw-socket socket) net
    (setf socket (cl+ssl:make-ssl-server-stream
		  (stream-fd (socket-stream raw-socket))
		  :key *server-key-file*
		  :certificate *server-cert-file*
		  :close-callback (lambda-ignore-args (socket-close raw-socket))
		  :next-protos-spec *next-protos-spec*))
    (let ((npn (cl+ssl::get-next-proto-negotiated socket)))
      (unless (member npn *next-protos-spec* :test #'string=)
	(error 'http2-not-started :other-protocol npn
	       :format-control "Protocol ~S negotiated instead of one of ~S."
	       :format-arguments (list npn *next-protos-spec*))))))

(defmethod net-socket-close ((net net-ssl))
  (with-slots (raw-socket socket) net
    (if socket
	(handler-case
	    (close socket)
	  (t ()
	    (if raw-socket
		(socket-close raw-socket))))
	(if raw-socket
	    (socket-close raw-socket)))))

(defmethod net-socket-shutdown ((net net-ssl))
  (with-slots (listener) net
    (if listener
	(socket-close listener))))

(defmethod net-write-vector ((net net-ssl) bytes n)
  (with-slots (socket) net
    (handler-case
	(write-sequence bytes socket :end n)
      (cl+ssl::ssl-error-syscall ()
	(error 'connection-reset-error :stream socket)))))

(defmethod net-read-vector ((net net-ssl) bytes n)
  (with-slots (socket) net
    (handler-case
	(cl+ssl::stream-read-partial-sequence socket bytes 0 n)
      (cl+ssl::ssl-error-syscall ()
	(error 'connection-reset-error :stream socket)))))

(defmethod net-finish-output ((net net-ssl))
  (with-slots (socket) net
    (handler-case
	(finish-output socket)
      (cl+ssl::ssl-error-syscall ()
	(error 'connection-reset-error :stream socket)))))

(defclass net-plain (net) ()
  (:documentation "Regular socket using SB-BSD-SOCKETS"))

#+sbcl
(defmethod net-socket-listen ((net net-plain) host port)
  (with-slots (listener) net
    (let ((server (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
      (setf (sb-bsd-sockets:sockopt-reuse-address server) t)
      (sb-bsd-sockets:socket-bind server (usocket:dotted-quad-to-vector-quad host) port)
      (sb-bsd-sockets:socket-listen server 8)
      (setf listener server))))

#+sbcl
(defmethod net-socket-accept ((net net-plain))
  (with-slots (raw-socket listener) net
    (setf raw-socket (sb-bsd-sockets:socket-accept listener))))

#+sbcl
(defmethod net-socket-prepare-server ((net net-plain))
  (with-slots (raw-socket socket) net
    (setf socket raw-socket)))

#+sbcl
(defmethod net-socket-close ((net net-plain))
  (with-slots (socket) net
    (sb-bsd-sockets:socket-close socket)))

#+sbcl
(defmethod net-socket-shutdown ((net net-plain))
  (with-slots (listener) net
    (if listener
	(sb-bsd-sockets:socket-close listener))))

#+sbcl
(defmethod net-write-vector ((net net-plain) bytes n)
  (with-slots (socket) net
    (sb-bsd-sockets:socket-send socket bytes n)))

#+sbcl
(defmethod net-read-vector ((net net-plain) bytes n)
  (with-slots (socket) net
    (multiple-value-bind (buf bytes-read peer)
	(sb-bsd-sockets:socket-receive socket bytes n)
      (declare (ignore buf peer))
      bytes-read)))

#+sbcl
(defmethod net-finish-output ((net net-plain))
  nil)

(defparameter *dump-bytes* t)
(defparameter *dump-bytes-stream* t)
(defparameter *dump-bytes-hook* nil)  ; nil or 'vector-inspect make sense

(defun send-bytes (net bytes)
  (when *dump-bytes*
    (format *dump-bytes-stream* "http2 send: ~A~%"
	    (if *dump-bytes-hook* (funcall *dump-bytes-hook* bytes) bytes)))
  (net-write-vector net bytes (length bytes))
  (net-finish-output net))

(defun receive-bytes (net)
  (let* ((bytes (make-array 1024 :element-type '(unsigned-byte 8) :fill-pointer 1024))
	 (bytes-read (net-read-vector net bytes 1024)))
    (unless bytes-read
      (error 'end-of-file :stream (net-socket net)))
    (when (plusp bytes-read)
      (setf (fill-pointer bytes) bytes-read)
      (when *dump-bytes*
	(format *dump-bytes-stream* "http2 recv: ~A~%"
		(if *dump-bytes-hook* (funcall *dump-bytes-hook* bytes) bytes)))
      bytes)))

(defun receive-loop (net conn)
  (handler-case
      (loop
	 (when-let (bytes (receive-bytes net))
	   (handler-case-unless *debug-mode*
	       (connection<< conn bytes)
	     (t (e)
		(format t "~S~%" e)
		(when (typep e 'simple-condition)
		  (apply #'format t
			 (concatenate 'string "(" (simple-condition-format-control e) ")~%")
			 (simple-condition-format-arguments e)))
		(net-socket-close net)))))
    (end-of-file ()
      nil)))

(defun do-client (uri)
  (unwind-protect
       (handler-case
	   (do-client-inner uri)
	 (connection-refused-error ()
	   (format t "Connection refused: ~A~%" uri)))
    (finish-output)))

(defun do-client-inner (uri)
  (when (not (uri-p uri))
    (setf uri (parse-uri uri)))
  (format t "About to connect socket to ~A port ~A...~%"
	  (uri-host uri) (or (uri-port uri) 443))
  (let ((socket (socket-connect (uri-host uri) (or (uri-port uri) 443)
				:protocol :stream
				:element-type '(unsigned-byte 8)
				:timeout 10))
	ssl-socket
	(conn (make-instance 'client)))

    (format t "Connected to server ~S:~S via my local connection at ~S:~S!~%"
            (get-peer-address socket) (get-peer-port socket)
            (get-local-address socket) (get-local-port socket))

    (format t "Making SSL socket...~%")
    (setf ssl-socket (cl+ssl:make-ssl-client-stream
		      (stream-fd (socket-stream socket))
		      ))
    (format t "Made SSL socket.~%")

    (on conn :frame
	(lambda (bytes)
	  (send-bytes ssl-socket (buffer-data bytes))))

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
	  (lambda ()
	    (format t "stream closed~%")
	    (error (make-condition 'end-of-file ssl-socket))))

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

      (receive-loop ssl-socket conn))))

(defun do-server (&key net (interface "0.0.0.0") (port 8080) secure)
  (unwind-protect
       (handler-case
	   (progn
	     (format t "Starting server on port ~D~%" port)
	     (ensuref net (make-instance (if secure 'net-ssl 'net-plain)))
	     (net-socket-listen net interface port)
	     (unwind-protect
		  (loop (do-server-inner net))
	       (net-socket-close net)
	       (net-socket-shutdown net)))
	 (address-in-use-error ()
	   (format t "Address already in use.~%")))
    (net-finish-output net)))

(defun do-server-inner (net)
  (net-socket-accept net)
  (format t "New TCP connection!~%")
  (net-socket-prepare-server net)
  (handler-case
      (do-server-accepted-socket net)
    (connection-reset-error ()
      (format t "Connection reset.~%")
      (net-socket-close net))
    (end-of-file ()
      (format t "End of file.~%")
      (net-socket-close net))))

(defun do-server-accepted-socket (net)
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
		(lambda ()
		  (format t "stream closed~%")))
	      
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
			    (if (string= (req-header ":path") "/")
				(setf response (buffer-simple "Hello HTTP 2.0! GET request"))
				(setf response (buffer-simple "You requested the path " (req-header ":path") " from me.")))))
		      
		      (headers stream `((":status"        . "200")
					("content-length" . ,(format nil "~D" (buffer-size response)))
					("content-type"   . "text/plain"))
			       :end-stream nil)
		      
		      ; split response into multiple DATA frames
		      (data stream (buffer-slice! response 0 5) :end-stream nil)
		      (data stream response))))))))

    (receive-loop net conn)))
