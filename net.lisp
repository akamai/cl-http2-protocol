; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol-example)

; Port note: The networking diverges a little bit from the Ruby
; because we have more housekeeping to do. To abstract networking
; differences when we wrap in SSL versus when we don't, a NET class is
; defined that can NET-SOCKET-LISTEN, NET-SOCKET-ACCEPT,
; NET-SOCKET-PREPARE-SERVER, NET-SOCKET-CLOSE, NET-SOCKET-SHUTDOWN,
; NET-WRITE-VECTOR, NET-READ-VECTOR, and NET-FINISH-OUTPUT. There are
; two versions, one for SSL, NET-SSL, and one for plain, NET-PLAIN,
; although in practice you need to pick NET-PLAIN-USOCKET or
; NET-PLAIN-SB-BSD-SOCKETS. We normalize calls, housekeeping, and
; differences in exceptions. Importantly, NET-READ-VECTOR must return
; 0 for no bytes read (think EAGAIN), a number of bytes read from 1 up
; to the N provided depending on what was available, or NIL when the
; peer has gracefully closed their end of the socket.
;
; Then we define SEND-BYTES, RECEIVE-BYTES, and RECEIVE-LOOP as
; regular functions that use a NET object to perform their duties.
;
; Because eventually HTTP/2.0 will settle on whether TLS is required
; and related issues, this is likely to be simplified in the future.

; change these paths to whatever they need to be:
(defparameter *key-pathname* (merge-pathnames "cl-http2-protocol/" (user-homedir-pathname)))
(defparameter *server-key-file* (merge-pathnames "mykey.pem" *key-pathname*))
(defparameter *server-cert-file* (merge-pathnames "mycert.pem" *key-pathname*))

(defparameter *next-protos-spec* '("h2-12"))

(defparameter *dump-bytes* nil)
(defparameter *dump-bytes-stream* t)
(defparameter *dump-bytes-base* 16)  ; 10 for decimal, 16 for hexadecimal
(defparameter *dump-bytes-hook* nil)  ; nil or 'vector-inspect make sense

(defclass net ()
  ((listener :accessor net-listener :initform nil)
   (raw-socket :accessor net-raw-socket :initform nil)
   (socket :accessor net-socket :initform nil))
  (:documentation "Abstraction for multiple network implementations"))


(defclass net-ssl (net)
  ((cert-file :initarg :cert-file :initform *server-cert-file*)
   (key-file :initarg :key-file :initform *server-key-file*))
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
  (with-slots (raw-socket socket key-file cert-file) net
    (setf socket (cl+ssl:make-ssl-server-stream
		  (stream-fd (socket-stream raw-socket))
		  :key (namestring key-file)
		  :certificate (namestring cert-file)
		  :close-callback (lambda-ignore (socket-close raw-socket))
		  :next-protos-spec *next-protos-spec*))
    (let ((npn (cl+ssl::get-next-proto-negotiated socket)))
      (unless (member npn *next-protos-spec* :test #'string=)
	(error 'http2-not-started :other-protocol npn
	       :format-control "Protocol ~S negotiated instead of one of ~S."
	       :format-arguments (list npn *next-protos-spec*))))))

(defmethod net-socket-connect ((net net-ssl) host port)
  (with-slots (raw-socket) net
    (setf raw-socket (socket-connect host port
				     :protocol :stream
				     :element-type '(unsigned-byte 8)
				     :timeout 10))))

(defmethod net-socket-prepare-client ((net net-ssl))
  (with-slots (raw-socket socket) net
    (setf socket (cl+ssl:make-ssl-client-stream
		  (stream-fd (socket-stream raw-socket))
		  :close-callback (lambda-ignore (socket-close raw-socket))
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

(defmethod net-input-ready ((net net-ssl))
  (with-slots (socket) net
    (handler-case
	(cl+ssl::stream-listen socket)
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


(defclass net-plain (net) ())

; this is not the most efficient implementation (see net-read-vector)
(defclass net-plain-usocket (net-plain) ()
  (:documentation "Regular socket using USOCKET"))

(defmethod net-socket-listen ((net net-plain-usocket) host port)
  (with-slots (listener) net
    (let ((server (usocket:socket-listen host port :reuse-address t :backlog 8 :element-type '(unsigned-byte 8))))
      (setf listener server))))

(defmethod net-socket-accept ((net net-plain-usocket))
  (with-slots (raw-socket listener) net
    (setf raw-socket (usocket:socket-accept listener))))

(defmethod net-socket-prepare-server ((net net-plain-usocket))
  (with-slots (raw-socket socket) net
    (setf socket (usocket:socket-stream raw-socket))))

(defmethod net-socket-connect ((net net-plain-usocket) host port)
  (with-slots (raw-socket) net
    (setf raw-socket (usocket:socket-connect host port
					     :protocol :stream
					     :element-type '(unsigned-byte 8)
					     :timeout 10))))

(defmethod net-prepare-client ((net net-plain-usocket))
  (with-slots (raw-socket socket) net
    (setf socket (usocket:socket-stream raw-socket))))

(defmethod net-socket-close ((net net-plain-usocket))
  (with-slots (socket) net
    (close socket)))

(defmethod net-socket-shutdown ((net net-plain-usocket))
  (with-slots (listener) net
    (if listener
	(usocket:socket-close listener))))

(defmethod net-write-vector ((net net-plain-usocket) bytes n)
  (with-slots (socket) net
    (write-sequence bytes socket)))

(defmethod net-read-vector ((net net-plain-usocket) bytes n)
  ; No method in usocket or with streams to block for 1 byte and opportunistically grab more
  ; Calling read-sequence/listen/... 1 byte at a time cannot be very efficient but it works
  (with-slots (socket) net
    (when (zerop (read-sequence bytes socket :start 0 :end 1))
      (return-from net-read-vector nil))
    (loop
       while (listen socket)
       for i from 1 below n
       with read = nil
       do (progn
	    (read-sequence bytes socket :start i :end (1+ i))
	    (setf read t))
       finally (return (if read (1+ i) 1)))))

(defmethod net-finish-output ((net net-plain-usocket))
  (with-slots (socket) net
    (finish-output socket)))


#+sbcl
(progn
  (defclass net-plain-sb-bsd-sockets (net-plain) ()
    (:documentation "Regular socket using SB-BSD-SOCKETS"))

  (defmethod net-socket-listen ((net net-plain-sb-bsd-sockets) host port)
    (with-slots (listener) net
      (let ((server (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
	(setf (sb-bsd-sockets:sockopt-reuse-address server) t)
	(sb-bsd-sockets:socket-bind server (usocket:dotted-quad-to-vector-quad host) port)
	(sb-bsd-sockets:socket-listen server 8)
	(setf listener server))))

  (defmethod net-socket-accept ((net net-plain-sb-bsd-sockets))
    (with-slots (raw-socket listener) net
      (setf raw-socket (sb-bsd-sockets:socket-accept listener))))

  (defmethod net-socket-prepare-server ((net net-plain-sb-bsd-sockets))
    (with-slots (raw-socket socket) net
      (setf socket raw-socket)))

  (defmethod net-socket-client ((net net-plain-sb-bsd-sockets) host port)
    (with-slots (raw-socket) net
      (let ((client (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
	(sb-bsd-sockets:socket-connect client host port)
	(setf raw-socket client))))

  (defmethod net-socket-prepare-client ((net net-plain-sb-bsd-sockets))
    (with-slots (raw-socket socket) net
      (setf socket raw-socket)))

  (defmethod net-socket-close ((net net-plain-sb-bsd-sockets))
    (with-slots (socket) net
      (sb-bsd-sockets:socket-close socket)))

  (defmethod net-socket-shutdown ((net net-plain-sb-bsd-sockets))
    (with-slots (listener) net
      (if listener
	  (sb-bsd-sockets:socket-close listener))))

  (defmethod net-write-vector ((net net-plain-sb-bsd-sockets) bytes n)
    (with-slots (socket) net
      (sb-bsd-sockets:socket-send socket bytes n)))

  (defmethod net-read-vector ((net net-plain-sb-bsd-sockets) bytes n)
    (with-slots (socket) net
      (unless (sb-bsd-sockets:socket-open-p socket)
	(error 'end-of-file :stream socket))
      (multiple-value-bind (buf bytes-read peer)
	  (sb-bsd-sockets:socket-receive socket bytes n)
	(declare (ignore buf peer))
	(cond ((null bytes-read) 0)
	      ((zerop bytes-read) nil)
	      (t bytes-read)))))

  (defmethod net-finish-output ((net net-plain-sb-bsd-sockets))
    nil))

; general functions

(defmacro maybe-dump-bytes (type bytes)
  `(when *dump-bytes*
     (let ((*print-base* *dump-bytes-base*))
       (format *dump-bytes-stream* ,(concatenate 'string "http2 " (string-downcase type) ": ~A~%")
	       (if *dump-bytes-hook* (funcall *dump-bytes-hook* ,bytes) ,bytes)))))

(defun send-bytes (net bytes)
  (maybe-dump-bytes :send bytes)
  (net-write-vector net bytes (length bytes))
  (net-finish-output net))

(defun receive-bytes (net)
  (let* ((bytes (make-array 1024 :element-type '(unsigned-byte 8) :fill-pointer 1024))
	 (bytes-read (net-read-vector net bytes 1024)))
    (unless bytes-read
      (error 'end-of-file :stream (net-socket net)))
    (when (plusp bytes-read)
      (setf (fill-pointer bytes) bytes-read)
      (maybe-dump-bytes :recv bytes)
      bytes)))

(defun receive-loop (net conn)
  (handler-case
      (loop
	 (while (and (http2::pump-stream-queues conn 2)
		     (not (net-input-ready net))))
	 (when-let (bytes (receive-bytes net))
	   (handler-case-unless *debug-mode*
	       (connection<< conn bytes)
	     (t (e)
		(report-error e)
		(net-socket-close net)))))
    (end-of-file ()  ; this is how to leave the loop
      nil)))
