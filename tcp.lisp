; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

;; Add & override some functions in CL-ASYNC
(in-package :cl-async)

;; convert HANDLER-CASE to HANDLER-BIND so restarts are preserved in event handlers
;; otherwise HANDLER-CASE makes this the new throwing context
(defmacro run-event-cb (event-cb &rest args)
  "Used inside of catch-app-errors, wraps the calling of an event-cb such that
   errors are caught and saved, making it so an event-cb isn't called twice with
   the same condition."
  `(handler-bind ((t (lambda (e)
		       ;; track the error so we don't re-fire (_evcb-err is defined in
		       ;; catch-app-errors)
		       (setf _evcb-err e))))
     ;; run the event handler
     (funcall ,event-cb ,@args)))

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
