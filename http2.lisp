; HTTP/2.0 draft-06 implementation in Common Lisp
; Copyright (c) 2014 Akamai Technologies, Inc.
;
; Originally a port of Ruby code (as of Feb 2014) at https://github.com/igrigorik/http-2
; Which contains the following license/copyright statement:
; (MIT License) - Copyright (c) 2013 Ilya Grigorik GA
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.

; TODO:
; -- ensure the file compiles on a clean image -BUG
; -- infinity support is SBCL-specific -PORTABILITY
; -- loop keywords are not consistent local/keyword -STYLE
; -- dependency on ALEXANDRIA -LIBRARIES
; -- dependency on USOCKET -LIBRARIES
; -- dependency on TRIVIAL-GRAY-STREAMS -LIBRARIES
; -- incomplete protection on array lengths -STABILITY
; -- array sizing and bounds checking is weak -STABILITY
; -- incomplete comment preservation from original ruby -STYLE
; -- note when initialize methods are rolled into CLOS defclass operation -STYLE
; -- make sure bytesize vs length is done OK -STABILITY
; -- use modules for namespacing as in the original, or rename some generics -STYLE
; -- @ sigil for avoiding shadowing/conflicts with CL -STYLE
; -- PUSH/SHIFT used as per Ruby but REVERSE/NREVERSE added -STYLE
; -- WHEN NOT models Ruby if ! but could become UNLESS -STYLE
; -- review DEFCLASS's for consistency -STYLE
; -- BUFFER-CONCAT-* might be rolled into a generic BUFFER-CONCAT -STYLE
; -- might be best to forego the #" reader -STYLE
; -- COPY-TREE on default tables might be able to be COPY-ALIST -SPEED
; -- not sure if BUFFER-INSPECT is exactly like Ruby .inspect -DEBUGABILITY
; -- document data structure choices in port from Ruby types -DOCUMENTATION
; -- should speed test against the Ruby -SPEED

(require :alexandria)
(require :puri)
(require :usocket)
(require :cl+ssl)
(require :trivial-gray-streams)

(defpackage :http2
  (:documentation "HTTP/2.0 draft-06 implementation.")
  (:use :common-lisp :alexandria :puri :usocket :cl+ssl :trivial-gray-streams)
  (:shadow #:stream #:stream-error))

(in-package :http2)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0) (compilation-speed 0)))

(defconstant *version* "0.6.3")
(defparameter *server-key-file* "/home/ubuntu/ruby_example/keys/mykey.pem")
(defparameter *server-cert-file* "/home/ubuntu/ruby_example/keys/mycert.pem")
(defparameter *next-protos-spec* '("HTTP-draft-06/2.0"))


; In addition to libraries already loaded, let's define some utilities.

(defun |#"-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (loop
     :with on = t
     :with s = nil
     :for c = (read-char stream)
     :until (eq c #\")
     :if (and (not on) (not (eq c #\Space))) :do (setq on t)
     :if on :do (push c s)
     :if (eq c #\Newline) :do (setq on nil)
     :finally (return (coerce (nreverse s) 'string))))

(set-dispatch-macro-character
  #\# #\" #'|#"-reader|)

(defmacro defalias (name existing)
  "Define NAME as a synonym for an EXISTING function or macro."
  `(defmacro ,name (&rest args)
     `(,',existing ,@args)))

(defmacro shift (place)
  "Like POP only at the end of the list designated by PLACE."
  (let ((end (gensym)))
    `(let ((,end (last ,place 2)))
       (cond ((null ,end) nil)
	     ((null (cdr ,end)) (setf ,place nil) (car ,end))
	     (t (prog1 (cadr ,end) (rplacd ,end nil)))))))

(defmacro unshift (place item)
  "Like PUSH only at the end of the list designated by PLACE."
  `(setf ,place (nconc ,place (list ,item))))

(defmacro while (test &body body)
  "Execute BODY while TEST is true."
  `(do () ((not ,test)) ,@body))

(defmacro while-let ((var-name test) &body body)
  "Execute BODY while TEST is true, binding TEST value to VAR-NAME."
  `(let (,var-name)
     (while (setq ,var-name ,test)
       ,@body)))

(defmacro ensuref (place value)
  "Set PLACE to VALUE only if PLACE is NIL."
  `(unless ,place
     (setf ,place ,value)))

(defmacro deletef-if (place test &rest deletef-args)
  "Simplify ALEXANDRIA:DELETEF when using a TEST."
  `(deletef ,place nil :test (lambda (match item)
			       (declare (ignore match))
			       (funcall ,test item))
	    ,@deletef-args))

(defun to-sym (name)
  "Ensure NAME is a SYMBOL and intern in the KEYWORD package if necessary."
  (if (symbolp name) name (intern name "KEYWORD")))

(defun string-to-bytes (string)
  "Coerce a string into a vector of (unsigned-byte 8) element type."
  (make-array (length string)
	      :element-type '(unsigned-byte 8)
	      :initial-contents (loop for c across string collect (char-code c))))

(defun flatten-n (x &optional n)
  "Flattens a tree, wholly or partially. Allows you to specify N, a maximum descent level."
  (labels ((rec (x acc n)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (if (or (not n) (not (minusp n)))
			  (rec (car x) (rec (cdr x) acc n) (if n (1- n) nil))
			  (cons x acc))))))
    (rec x nil n)))

(defmacro dohash ((key-name value-name hash-table-name) &rest body)
  #"Similar to DOLIST, but for hashes. Perform the BODY once for each key/value pair.
    Set KEY-NAME and VALUE-NAME appropriately for each iteration."
  (let ((iterator (gensym "HASHITER"))
	(value-present (gensym "HASHNEXT")))
    `(with-hash-table-iterator (,iterator ,hash-table-name)
       (loop
	  (multiple-value-bind (,value-present ,key-name ,value-name) (,iterator)
	    (declare (ignorable ,key-name ,value-name))
	    (unless ,value-present (return nil))
	    ,@body)))))

(defconstant +infinity
  #+sbcl sb-ext:double-float-positive-infinity
  #+(or abcl cmu scl) ext:double-float-positive-infinity
  #+allegro excl::*infinity-double*
  #+ecl si:double-float-positive-infinity)

(defconstant -infinity
  #+sbcl sb-ext:double-float-negative-infinity
  #+(or abcl cmu scl) ext:double-float-negative-infinity
  #+allegro excl::*negative-infinity-double*
  #+ecl si:double-float-negative-infinity)

(defmacro lambda-ignore (&body body)
  "Convenience macro to make a LAMBDA form that ignores its arguments."
  `(lambda (&rest args-to-be-ignored)
     (declare (ignore args-to-be-ignored))
     ,@body))

(defmacro lambda-apply (&body body)
  "Convenience macro to make a LAMBDA form that APPLY's its arguments to the end of the first body expression."
  `(lambda (&rest args-to-be-applied)
     (apply (function ,(caar body)) ,@(cdar body) args-to-be-applied)
     ,@(cdr body)))

(defmacro raise (error-type &optional error-msg &rest error-args)
  "Convenience macro to raise an exception of ERROR-TYPE with ERROR-MSG and optional ERROR-ARGS."
  `(error (make-condition ,error-type
			  :format-control ,error-msg
			  :format-arguments (list ,@error-args))))

(defmacro pack (control values)
  #"A macro that expands into code to pack VALUES into bytes per the template in CONTROL.
    CONTROL is a string of characters which can be of these:
    n - 16-bit integer
    N - 32-bit integer
    C - character"
  (when (symbolp control)
    (setf control (eval control)))
  (with-gensyms (values-ptr value bytes position)
    (let* ((bits-and-sets
	    (loop
	       for code across control
	       for size-set =
	       (ecase code
		 (#\n
		  `(16
		    (setf ,value (coerce ,value '(unsigned-byte 16)))
		    (setf (aref ,bytes ,position) (ldb (byte 8 #+big-endian 0 #+little-endian 8) ,value))
		    (incf ,position)
		    (setf (aref ,bytes ,position) (ldb (byte 8 #+big-endian 8 #+little-endian 0) ,value))
		    (incf ,position)))
		 (#\N
		  `(32
		    (setf ,value (coerce ,value '(unsigned-byte 32)))
		    (setf (aref ,bytes ,position) (ldb (byte 8 #+big-endian  0 #+little-endian 24) ,value))
		    (incf ,position)
		    (setf (aref ,bytes ,position) (ldb (byte 8 #+big-endian  8 #+little-endian 16) ,value))
		    (incf ,position)
		    (setf (aref ,bytes ,position) (ldb (byte 8 #+big-endian 16 #+little-endian  8) ,value))
		    (incf ,position)
		    (setf (aref ,bytes ,position) (ldb (byte 8 #+big-endian 24 #+little-endian  0) ,value))
		    (incf ,position)))
		 (#\C
		  `(8
		    (setf ,value (coerce (if (characterp ,value) (char-code ,value) ,value) '(unsigned-byte 8)))
		    (setf (aref ,bytes ,position) (ldb (byte 8 0) ,value))
		    (incf ,position))))
	       sum (car size-set) into bits
	       collect (cdr size-set) into sets
	       finally (return (list bits sets))))
	   (bit-size (first bits-and-sets))
	   (sets (second bits-and-sets)))
      `(let* ((,values-ptr ,values)
	      (,value (car ,values-ptr))
	      (,bytes (make-array ,(/ bit-size 8) :element-type '(unsigned-byte 8)))
	      (,position (the fixnum 0)))
	 ,@(butlast (cdr (mappend (lambda (set)
				    `((setf ,values-ptr (cdr ,values-ptr)
					    ,value (car ,values-ptr))
				      ,@set)) sets)))
	 ,bytes))))

(defmacro unpack (control bytes-form)
  #"A macro that expands into code to unpack BYTES into values per the template in CONTROL.
    CONTROL is a string of characters which can be of these:
    n - 16-bit integer
    N - 32-bit integer
    C - character"
  (when (symbolp control)
    (setf control (eval control)))
  (with-gensyms (bytes values value position)
    (let* ((sets
	    (loop
	       for code across control
	       for size-set =
	       (ecase code
		 (#\n
		  `(16
		    (setf ,value (coerce 0 '(unsigned-byte 16)))
		    (setf (ldb (byte 8 #+big-endian 0 #+little-endian 8) ,value) (aref ,bytes ,position))
		    (incf ,position)
		    (setf (ldb (byte 8 #+big-endian 8 #+little-endian 0) ,value) (aref ,bytes ,position))
		    (incf ,position)))
		 (#\N
		  `(32
		    (setf ,value (coerce 0 '(unsigned-byte 32)))
		    (setf (ldb (byte 8 #+big-endian  0 #+little-endian 24) ,value) (aref ,bytes ,position))
		    (incf ,position)
		    (setf (ldb (byte 8 #+big-endian  8 #+little-endian 16) ,value) (aref ,bytes ,position))
		    (incf ,position)
		    (setf (ldb (byte 8 #+big-endian 16 #+little-endian  8) ,value) (aref ,bytes ,position))
		    (incf ,position)
		    (setf (ldb (byte 8 #+big-endian 24 #+little-endian  0) ,value) (aref ,bytes ,position))
		    (incf ,position)))
		 (#\C
		  `(8
		    (setf ,value (aref ,bytes ,position))
		    (incf ,position))))
	       collect (cdr size-set))))
      (if (> (length control) 1)
	  `(let ((,bytes ,bytes-form)
		 ,values
		 ,value
		 (,position (the fixnum 0)))
	     ,@(mappend (lambda (set)
			  `(,@set
			    (push ,value ,values))) sets)
	     (nreverse ,values))
	  `(let ((,bytes ,bytes-form)
		 ,value
		 (,position (the fixnum 0)))
	     ,@(first sets)
	     ,value)))))

(defvar *debug-mode* t)

(defmacro handler-case-unless (var expression &body clauses)
  #"Expands into code that gives two paths, depending on the run-time value of a variable.
    When the variable is true, exceptions bubble to the control plane; when false, they are handled.
    A global such as *debug-mode* can be used throughout as the variable."
  (with-gensyms (fn)
    `(flet ((,fn () ,expression))
       (if ,var
	   (,fn)
	   (handler-case
	       (,fn)
	     ,@clauses)))))

; ------------------------------------------------------------
; buffer.rb port
; ------------------------------------------------------------
; Port notes: We cannot subclass STRING in CL to accomplish what we want. We have less utility
; methods out-of-the-box, so we'll define a bunch of methods similar to the Ruby String ones.

(defun make-data-array (n)
  (when (> n #.(* 16 1024))
    (raise 'simple-condition "MAKE-DATA-ARRAY called with ~A as N value, greater than safety maximum." n))
  (make-array (max n 1024) :element-type '(unsigned-byte 8) :adjustable t :fill-pointer n))

(defun make-data-array-with-contents (n contents)
  (when (> n #.(* 16 1024))
    (raise 'simple-condition "MAKE-DATA-ARRAY-WITH-CONTENTS called with ~A as N value, greater than safety maximum." n))
  (make-array (max n 1024) :element-type '(unsigned-byte 8) :adjustable t :fill-pointer n :initial-contents contents))

(defclass buffer ()
  ((arraydata :accessor buffer-data :initarg :data :initform (make-data-array 0))))

(defun buffer-simple (&rest items)
  #"Create a new BUFFER object and populate it with contents of each of ITEMS.
    Each of ITEMS may be an integer, string, or vector of bytes."
  (let ((buffer (make-instance 'buffer)))
    (dolist (item items)
      (buffer<< buffer item))
    buffer))

(defmethod buffer-adjust ((buffer buffer) size)
  (adjust-array (buffer-data buffer) size)
  (setf (fill-pointer (buffer-data buffer)) size)
  buffer)

(defmethod buffer-empty-p ((buffer buffer))
  "Pedicate to indicate T if BUFFER is empty or NIL if BUFFER contains bytes."
  (zerop (length (buffer-data buffer))))

(defmethod buffer<< ((buffer buffer) (integer integer))
  "Modifies BUFFER by concatenating a character designated by INTEGER, and returns BUFFER."
  (assert (<= 0 integer 255) (integer) "Integer provided is ~A but must be from 0-255.")
  (vector-push-extend integer (buffer-data buffer))
  buffer)

(defmethod buffer<< ((buffer buffer) (vector vector))
  "Modifies BUFFER by concatenating the byte numbers in VECTOR, and returns BUFFER."
  (array-concat vector (buffer-data buffer))
  buffer)

(defmethod buffer<< ((buffer buffer) (string string))
  "Modifies BUFFER by concatenating a STRING, and returns BUFFER."
  (buffer<< buffer (babel:string-to-octets string)))

(defun array-concat (src dest)
  #"Modifies DEST by concatenating the elements of SRC to the end, and returns DEST.
    DEST should be a vector will a fill pointer. SRC should be a vector."
  (declare (type (vector (unsigned-byte 8)) src dest))
  (let* ((src-len (length src))
	 (dest-len (length dest))
	 (new-dest-len (+ src-len dest-len)))
    (when (> new-dest-len (array-dimension dest 0))
      (adjust-array dest new-dest-len))
    (loop
       for i from 0 below src-len
       for j from dest-len below new-dest-len
       do (setf (aref dest j) (aref src i)))
    (setf (fill-pointer dest) new-dest-len))
  dest)

(defmethod buffer<< ((buffer buffer) (buffer2 buffer))
  "Modifies BUFFER by concatenating the contents of BUFFER2 to BUFFER, and returns BUFFER."
  (buffer<< buffer (buffer-data buffer2)))

(defmethod buffer-prepend ((buffer buffer) (vector vector))
  "Modifies BUFFER by prepending BUFFER with the byte numbers in VECTOR, and returns BUFFER."
  (array-prepend vector (buffer-data buffer))
  buffer)

(defun array-prepend (src dest)
  #"Modifies DEST by prepending the elements of SRC to the beginning, and returns DEST.
    DEST should be a vector with a fill pointer. SRC should be a vector."
  (declare (type (vector (unsigned-byte 8)) src dest))
  (let* ((src-len (length src))
	 (dest-len (length dest))
	 (dest-new-len (+ dest-len src-len)))
    (when (> dest-new-len (array-dimension dest 0))
      (adjust-array dest dest-new-len))
    (loop
       for i downfrom (1- dest-new-len) to src-len
       for j downfrom (1- dest-len) to 0
       do (setf (aref dest i) (aref dest j)))
    (loop
       for i from 0 below src-len
       do (setf (aref dest i) (aref src i)))
    (setf (fill-pointer dest) dest-new-len))
  dest)

(defmethod buffer-firstbyte ((buffer buffer))
  "Returns the first byte number in BUFFER without making any modification."
  (aref (buffer-data buffer) 0))

(defmethod buffer-firstchar ((buffer buffer))
  "Returns the first byte in BUFFER as a character, without making any modification"
  (code-char (aref (buffer-data buffer) 0)))

(defmethod buffer-getbyte ((buffer buffer) &optional (remove t))
  "Returns the first byte number in BUFFER. Modifies BUFFER to remove it if REMOVE is true."
  (aref (buffer-data (if remove (buffer-read buffer 1) buffer)) 0))

(defalias buffer-readbyte buffer-getbyte)

(defmethod buffer-setbyte ((buffer buffer) index value)
  "Modifies BUFFER to set a particular byte at INDEX in BUFFER to VALUE, and returns BUFFER."
  (setf (aref (buffer-data buffer) index) value)
  buffer)

(defmethod buffer-size ((buffer buffer))
  "Returns the length of BUFFER without modifying it."
  (length (buffer-data buffer)))

(defun array-splice (src dest n)
  #"Splice N bytes from the beginning of SRC into DEST and return DEST. Destroys any existing value
    in DEST; the bytes are copied into the front and the fill pointer is reset to N. Removes the
    bytes copied from SRC by copying the remaining bytes forward and resetting the fill pointer to
    the new length (original length - N). SRC should be a vector with a fill pointer. DEST should
    be a vector with a fill pointer, and must either definitely have a size equal or greater to N
    or be an adjustable vector. If SRC does not have at least N bytes, NO bytes are copied, and the
    fill pointer of DEST is set to zero and DEST is returned. If DEST is not long enough to contain
    N bytes, ADJUST-ARRAY is called to expand it, and the operation proceeds as normal."
  (declare (type (vector (unsigned-byte 8)) src dest))
  (let* ((src-len (length src))
	 (src-new-len (- src-len n)))
    (when (< src-len n)
      (setf (fill-pointer dest) 0)
      (return-from array-splice dest))
    (when (> n (array-dimension dest 0))
      (adjust-array dest n))
    (loop
       for i below n
       do (setf (aref dest i) (aref src i)))
    (setf (fill-pointer dest) n)
    (loop
       for i below src-new-len
       for j from n below src-len
       do (setf (aref src i) (aref src j)))
    (setf (fill-pointer src) src-new-len)
    dest))

(defmethod buffer-read ((buffer buffer) n)
  #"Modifies BUFFER by splicing N bytes from the front into a new buffer which is returned.
    If N is greater than the size of BUFFER, only the remaining bytes in BUFFER are spliced."
  (if (< n (length (buffer-data buffer)))
      (let ((spliced (make-instance 'buffer :data (make-data-array n))))
	(array-splice (buffer-data buffer) (buffer-data spliced) n)
	spliced)
      (let ((spliced (make-instance 'buffer :data (make-data-array 0))))
	(rotatef (buffer-data buffer) (buffer-data spliced))  ; faster than copying bytes
	spliced)))

(defun array-delete (array start length)
  "Modifies ARRAY by deleting bytes at position START for length LENGTH."
  (let* ((old-len (length array))
	 (new-len (- old-len length))
	 (start2 (+ start length)))
    (loop
       for i from start below new-len
       for j from start2 below old-len
       do (setf (aref array i) (aref array j)))
    (setf (fill-pointer array) new-len)
    array))

(defun array-delete-at (array index)
  "Modifies ARRAY by deleting the single byte at position INDEX."
  (array-delete array index 1))

(defmethod buffer-delete-section ((buffer buffer) start length)
  "Modifies BUFFER by deleting bytes from position START of length LENGTH, and returns BUFFER."
  (array-delete (buffer-data buffer) start length)
  buffer)

(defun array-slice (src start length &optional (dest (make-data-array length)))
  #"Copies bytes from SRC to DEST from position START of length LENGTH, and returns DEST.
    SRC should be a vector of bytes. DEST should be a vector of bytes with a fill pointer
    that either definitely has a size equal or greater than LENGTH or is adjustable."
  (declare (type (vector (unsigned-byte 8)) src dest))
  (when (> length (array-dimension dest 0))
    (adjust-array dest length))
  (loop
     for i from 0 below length
     for j from start below (+ start length)
     do (setf (aref dest i) (aref src j)))
  (setf (fill-pointer dest) length)
  dest)

(defmethod buffer-slice ((buffer buffer) start length)
  "Copies bytes from BUFFER from position START of length LENGTH into a new buffer which is returned."
  (let ((data (array-slice (buffer-data buffer) start length)))
    (make-instance 'buffer :data data)))

(defmethod buffer-slice! ((buffer buffer) start length)
  #"Modifies BUFFER by removing the bytes from position START of length LENGTH.
    Returns a new buffer containing the deleted bytes."
  (prog1 (buffer-slice buffer start length)
    (buffer-delete-section buffer start length)))

(defmethod buffer-read-uint32 ((buffer buffer))
  #"Modifies BUFFER by removing 4 bytes from the front, and returning them as a 32-bit integer.
    The bytes are assumed to be in network order in the buffer."
  (unpack "N" (buffer-data (buffer-read buffer 4))))

(defmethod buffer-string ((buffer buffer))
  "Returns a string representation of BUFFER, decoding UTF-8. Does not modify BUFFER."
  (babel:octets-to-string (buffer-data buffer)))

(defmethod buffer-ascii ((buffer buffer))
  "Returns a string representation of BUFFER, treating it as ASCII only. Does not modify BUFFER."
  (map 'string #'code-char (buffer-data buffer)))

(defun vector-inspect (bytes &key (max-length 512))
  #"Produce a string which is a mixture of characters and escaped values.
    Blackslashes and double-quotes are escaped with a leading backslash.
    Characters with codepoints 32-126 are added literally.
    For codes outside that range, \\xXX is used where XX is the hexadecimal
    representation of the code. The bytes are decoded for UTF-8, but that
    is done ignoring errors, so it may bail and use the raw bytes."
  (let ((src (or (ignore-errors (babel:octets-to-string bytes))
		 (coerce (loop for c across bytes collect (code-char c)) 'string)))
	truncated)
    (when (> (length src) max-length)
      (setq src (subseq src 0 max-length)
	    truncated t))
    (loop
       with string = (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)
       initially (vector-push-extend #\" string)
       for char across src
       for c = (char-code char)
       do (cond
	    ((= c 34)			; "
	     (vector-push-extend #\\ string)
	     (vector-push-extend #\" string))
	    ((= c 92)			; \
	     (vector-push-extend #\\ string)
	     (vector-push-extend #\\ string))
	    ((<= 32 c 126)
	     (vector-push-extend char string))
	    (t
	     (loop
		for h across (format nil "\\x~2,'0X" c)
		do (vector-push-extend h string))))
       finally (progn
		 (vector-push-extend #\" string)
		 (when truncated
		   (loop
		      for h across (format nil "...(truncated)")
		      do (vector-push-extend h string)))
		 (return string)))))

(defmethod buffer-inspect ((buffer buffer) &key (max-length 512))
  "Calls VECTOR-INSPECT on the contents of BUFFER."
  (vector-inspect (buffer-data buffer) :max-length max-length))


; ------------------------------------------------------------
; flow_buffer.rb
; ------------------------------------------------------------

(defparameter *max-frame-size* (1- (expt 2 14)))

(defclass flowbuffer-include ()
  ((send-buffer :accessor send-buffer :initarg :send-buffer)))

(defmethod buffered-amount ((obj flowbuffer-include))
  (or (reduce #'+ (mapcar (lambda (f) (getf f :length)) (send-buffer obj))) 0))

(defgeneric encode (obj frame))
(defgeneric emit (obj obj &rest args))

(defmethod send-data ((obj flowbuffer-include) &optional frame encode)
  (with-slots (send-buffer window) obj
    (when frame
      (push frame send-buffer))
    (while (and (plusp window) send-buffer)
      (let ((frame (shift send-buffer))
	    (sent 0)
	    (frame-size (buffer-size (getf frame :payload))))
	(if (> frame-size window)
	    (let* ((payload (remf frame :payload))
		   (chunk (copy-list frame))) ; dup***
	      (setf (getf frame :payload) (buffer-slice! payload 0 window))
	      (setf (getf chunk :length) (buffer-size payload))
	      (setf (getf chunk :payload) payload)
	      (deletef (getf frame :flags) :end-stream)
	      (unshift send-buffer chunk)
	      (setf sent window))
	    (setf sent frame-size))
	(when encode
	  (setf frame (encode obj frame)))
	(emit obj :frame frame)
	(decf window sent)))))


; ------------------------------------------------------------
; emitter.rb
; ------------------------------------------------------------

(defclass emitter-include ()
  ((listeners :accessor listeners :initarg :listeners :initform (make-hash-table))))

(defmethod add-listener ((emitter emitter-include) event block)
  (unless block
    (error "must provide callback"))
  (push block (gethash (to-sym event) (listeners emitter) nil)))

(defalias on add-listener)

(defmethod once ((emitter emitter-include) event block)
  (add-listener emitter event
		(lambda (&rest args) (apply block args) :delete)))

(defmethod emit ((emitter emitter-include) event &rest args)
  (deletef-if (gethash event (listeners emitter))
	      (lambda (cb) (eq (apply cb args) :delete))))

; no listeners function because we rolled the gethash into
; add-listener/emit, which is necessary because we're using things
; like push which need a place


; ------------------------------------------------------------
; error.rb
; ------------------------------------------------------------

(defclass error-include () ())

(define-condition http2-error (simple-error) ())

(define-condition http2-handshake-error (http2-error) ()
  (:documentation "Raised if connection header is missing or invalid indicating that
this is an invalid HTTP 2.0 request - no frames are emitted and the
connection must be aborted."))

(define-condition http2-protocol-error (http2-error) ()
  (:documentation "Raised by stream or connection handlers, results in GOAWAY frame
which signals termination of the current connection. You *cannot*
recover from this exception, or any exceptions subclassed from it."))

(define-condition http2-compression-error (http2-protocol-error) ()
  (:documentation "Raised on any header encoding / decoding exception."))

(define-condition http2-header-exception (http2-protocol-error) ()
  (:documentation "Raised on invalid reference for current compression context: the
client and server contexts are out of sync."))

(define-condition http2-flow-control-error (http2-protocol-error) ()
  (:documentation "Raised on invalid flow control frame or command."))

(define-condition http2-stream-error (http2-protocol-error) ()
  (:documentation "Raised on invalid stream processing: invalid frame type received or
sent, or invalid command issued."))

(define-condition http2-stream-closed (http2-error) ()
  (:documentation "Raised if stream has been closed and new frames cannot be sent."))

(define-condition http2-connection-closed (http2-error) ()
  (:documentation "Raised if connection has been closed (or draining) and new stream
cannot be opened."))

(define-condition http2-stream-limit-exceeded (http2-error) ()
  (:documentation "Raised if stream limit has been reached and new stream cannot be opened."))


; ------------------------------------------------------------
; connection.rb
; ------------------------------------------------------------

(defparameter *default-flow-window* 65535
  "Default connection and stream flow control window (64KB)")

(defparameter *default-priority* (expt 2 30)
  "Default stream priority (lower values are higher priority)")

(defparameter *connection-header-string*
  (concatenate 'string "PRI * HTTP/2.0" #1='(#\Return #\Linefeed) #1# "SM" #1# #1#)
  "Default connection \"fast-fail\" preamble string as defined by the spec")

(defparameter *connection-header* (string-to-bytes *connection-header-string*))

(defclass connection (flowbuffer-include emitter-include error-include)
  ((state :reader conn-state)
   (error :reader conn-error :initform nil)
   (window :reader conn-window :initarg :window :initform *default-flow-window*)
   (stream-limit :reader conn-stream-limit :initarg :streams :initform 100)
   (active-stream-count :reader conn-active-stream-count :initform 0)
   (streams :initform (make-hash-table))
   (framer :initform (make-instance 'framer))
   (window-limit :initarg :window-limit)
   (recv-buffer :initform (make-instance 'buffer))
   (send-buffer :initform nil)
   (continuation :initform nil)
   (stream-id :initform nil)))

(defmethod initialize-instance :after ((connection connection) &key)
  (setf (slot-value connection 'window-limit) (slot-value connection 'window)))

(defgeneric send (obj frame))

(defmethod new-stream ((connection connection) &optional (priority *default-priority*) (parent nil))
  (with-slots (state active-stream-count stream-limit stream-id) connection
    (cond ((eq state :closed)                   (raise 'http2-connection-closed))
	  ((= active-stream-count stream-limit) (raise 'http2-stream-limit-exceeded))
	  (t (prog1
		 (activate-stream connection stream-id priority parent)
	       (incf stream-id 2))))))

(defmethod ping ((connection connection) payload blk)
  (send connection (list :type :ping :stream 0 :payload payload))
  (if blk (once connection :pong blk)))

(defmethod goaway ((connection connection) &optional (error :no-error) (payload nil))
  (with-slots (streams) connection
    (let ((last-stream (or (loop for k being the hash-keys of streams maximize k) 0)))
      (send connection (list :type :goaway :last-stream last-stream
			     :error error :payload payload)))))

(defmethod settings ((connection connection) &optional (stream-limit (slot-value connection 'stream-limit)) (window-limit (slot-value connection 'window-limit)))
  (with-slots (window) connection
    (let ((payload (list :settings-max-concurrent-streams stream-limit)))
      (if (eql window +infinity)
	  (appendf payload (list :settings-flow-control-options 1))
	  (appendf payload (list :settings-initial-window-size window-limit))
	  ; semantically the following pair makes a bit more sense but it
	  ; results in the order of the settings (and thus the bytes)
	  ; coming out in reverse order to the Ruby code
	  ;  (setf (getf payload :settings-flow-control-options) 1)
	  ;  (setf (getf payload :settings-initial-window-size) window-limit)
	  )
      (send connection (list :type :settings :stream 0 :payload payload)))))

; these have to appear here to compile (receive connection ...) properly
(defgeneric receive (obj data))
(defalias stream<< receive)

(defmethod receive ((connection connection) data)
  #"Decodes incoming bytes into HTTP 2.0 frames and routes them to
    appropriate receivers: connection frames are handled directly, and
    stream frames are passed to appropriate stream objects."

  (handler-case-unless *debug-mode*
      (with-slots (state recv-buffer stream-limit window-limit continuation streams framer) connection
	(buffer<< recv-buffer data)

	; Upon establishment of a TCP connection and determination that
	; HTTP/2.0 will be used by both peers, each endpoint MUST send a
	; connection header as a final confirmation and to establish the
	; initial settings for the HTTP/2.0 connection.
	; Client connection header is 24 byte connection header followed by
	; SETTINGS frame. Server connection header is SETTINGS frame only.
	(when (eq state :new)
	  (if (< (buffer-size recv-buffer) #.(length *connection-header*))
	      (if (mismatch (buffer-data recv-buffer)
			    *connection-header*
			    :end2 (buffer-size recv-buffer))
		  (raise 'http2-handshake-error)
		  (return-from receive))
	      (if (mismatch (buffer-data (buffer-read recv-buffer #.(length *connection-header*)))
			    *connection-header*
			    :end1 #.(length *connection-header*))
		  (raise 'http2-handshake-error)
		  (progn
		    (setf state :connection-header)
		    (settings connection stream-limit window-limit)))))

	(while-let (frame (parse framer recv-buffer))
	  ; Header blocks MUST be transmitted as a contiguous sequence of frames
	  ; with no interleaved frames of any other type, or from any other stream.
	  (when continuation
	    (when (or (not (eq (getf frame :type) :continuation))
		      (not (equal (getf frame :stream) (getf (first continuation) :stream)))) ; *** equal ?
	      (connection-error connection))

	    (push frame continuation)
	    (when (not (member :end-headers (getf frame :flags)))
	      (return-from receive))

	    (let ((headers (flatten-n (mapcar (lambda (chunk)
						(decode-headers connection chunk)
						(getf chunk :payload))
					      continuation) 1)))

	      (setf frame (shift continuation))
	      (setf continuation nil)

	      (remf frame :length)
	      (setf (getf frame :payload) headers)
	      (push (if (eq (getf frame :type) :push-promise)
			:end-push-promise
			:end-headers)
		    (getf frame :flags))))

	  ; SETTINGS frames always apply to a connection, never a single stream.
	  ; The stream identifier for a settings frame MUST be zero.  If an
	  ; endpoint receives a SETTINGS frame whose stream identifier field is
	  ; anything other than 0x0, the endpoint MUST respond with a connection
	  ; error (Section 5.4.1) of type PROTOCOL_ERROR.
	  (if (connection-frame-p connection frame)
	      (connection-management connection frame)
	      (case (getf frame :type)
		(:headers
	         ; The last frame in a sequence of HEADERS/CONTINUATION
	         ; frames MUST have the END_HEADERS flag set.
		 (when (not (member :end-headers (getf frame :flags)))
		   (push frame continuation)
		   (return-from receive))

	  	 ; After sending a GOAWAY frame, the sender can discard frames
	  	 ; for new streams.  However, any frames that alter connection
	  	 ; state cannot be completely ignored.  For instance, HEADERS,
	  	 ; PUSH_PROMISE and CONTINUATION frames MUST be minimally
	  	 ; processed to ensure a consistent compression state
		 (decode-headers connection frame)
		 (when (eq state :closed)
		   (return-from receive))

		 (let ((stream (gethash (getf frame :stream) streams)))
		   (when (null stream)
		     (setf stream (activate-stream connection
				   (getf frame :stream)
				   (or (getf frame :priority) *default-priority*)))
		     (emit connection :stream stream))

		   (stream<< stream frame)))
		(:push-promise
		 ; The last frame in a sequence of PUSH_PROMISE/CONTINUATION
		 ; frames MUST have the END_PUSH_PROMISE/END_HEADERS flag set
		 (when (not (member :end-push-promise (getf frame :flags)))
		   (push frame continuation)
		   (return-from receive))
	     
		 (decode-headers connection frame)
		 (when (eq state :closed)
		   (return-from receive))
	     
		 ; PUSH_PROMISE frames MUST be associated with an existing, peer-
		 ; initiated stream... A receiver MUST treat the receipt of a
		 ; PUSH_PROMISE on a stream that is neither "open" nor
		 ; "half-closed (local)" as a connection error (Section 5.4.1) of
		 ; type PROTOCOL_ERROR. Similarly, a receiver MUST treat the
		 ; receipt of a PUSH_PROMISE that promises an illegal stream
		 ; identifier (Section 5.1.1) (that is, an identifier for a stream
		 ; that is not currently in the "idle" state) as a connection error
		 ; (Section 5.4.1) of type PROTOCOL_ERROR, unless the receiver
		 ; recently sent a RST_STREAM frame to cancel the associated stream.
		 (let ((parent (gethash (getf frame :stream) streams))
		       (pid (getf frame :promise-stream)))

		   (when (null parent)
		     (connection-error connection :msg "missing parent ID"))

		   (if (not (or (eq (state parent) :open)
				(eq (state parent) :half-closed-local)))
		       ; An endpoint might receive a PUSH_PROMISE frame after it sends
		       ; RST_STREAM.  PUSH_PROMISE causes a stream to become "reserved".
		       ; The RST_STREAM does not cancel any promised stream.  Therefore, if
		       ; promised streams are not desired, a RST_STREAM can be used to
		       ; close any of those streams.
		       (if (eq (closed parent) :local-rst)
			   ; We can either (a) 'resurrect' the parent, or (b) RST_STREAM
			   ; ... sticking with (b), might need to revisit later.
			   (send connection (list :type :rst-stream :stream pid :error :refused-stream))
			   (connection-error connection)))

		   (let ((stream (activate-stream pid *default-priority* parent)))
		     (emit connection :promise stream)
		     (stream<< stream frame))))
		(otherwise
		 (if-let (stream (gethash (getf frame :stream) streams))
		   (stream<< stream frame)
		   ; An endpoint that receives an unexpected stream identifier
		   ; MUST respond with a connection error of type PROTOCOL_ERROR.
		   (connection-error connection)))))))
    (t (e) (declare (ignore e)) (connection-error connection))))

(defalias connection<< receive)

(defmethod send ((connection connection) frame)
  #"Send an outgoing frame. DATA frames are subject to connection flow
    control and may be split and / or buffered based on current window size.
    All other frames are sent immediately."
  (if (eq (getf frame :type) :data)
      (send-data connection frame t)
      ; An endpoint can end a connection at any time. In particular, an
      ; endpoint MAY choose to treat a stream error as a connection error.
      (if (eq (getf frame :type) :rst-stream)
	  (when (eq (getf frame :error) :protocol-error)
	    (goaway connection (getf frame :error)))
	  (emit connection :frame (encode connection frame)))))

(defmethod encode ((connection connection) frame)
  "Applies HTTP 2.0 binary encoding to the frame."
  (with-slots (framer) connection
    (when (member (getf frame :type) '(:headers :push-promise))
      (encode-headers connection frame))
    (generate framer frame)))

(defmethod connection-frame-p ((connection connection) frame)
  #"Check if frame is a connection frame: SETTINGS, PING, GOAWAY, and any
    frame addressed to stream ID = 0."
  (or (= (getf frame :stream) 0)
      (member (getf frame :type) '(:settings :ping :goaway))))

(defmethod connection-management ((connection connection) frame)
  #"Process received connection frame (stream ID = 0).
    - Handle SETTINGS updates
    - Connection flow control (WINDOW_UPDATE)
    - Emit PONG auto-reply to PING frames
    - Mark connection as closed on GOAWAY"
  (with-slots (state window) connection
    (case state
      (:connection-header
       ; SETTINGS frames MUST be sent at the start of a connection.
       (connection-settings connection frame)
       (setf state :connected))

      (:connected
       (case (getf frame :type)
	 (:settings
	  (connection-settings connection frame))
	 (:window-update
	  (flow-control-allowed-p connection)
	  (incf window (getf frame :increment))
	  (send-data connection nil t))
	 (:ping
	  (if (member :pong (getf frame :flags))
	      (emit connection :pong (getf frame :payload))
	      (send connection
		    (list :type :ping :stream 0 :flags (list :pong)
			  :payload (getf frame :payload)))))
	 (:goaway
	  ; Receivers of a GOAWAY frame MUST NOT open additional streams on
          ; the connection, although a new connection can be established
          ; for new streams.
	  (setf state :closed)
	  (emit connection :goaway (getf frame :last-stream) (getf frame :error) (getf frame :payload)))
	 (otherwise
	  (connection-error connection))))
      (otherwise
       (connection-error connection)))))

(defmethod connection-settings ((connection connection) frame)
  "Update local connection settings based on parameters set by the peer."
  (with-slots (stream-limit window window-limit streams) connection
    (when (or (not (eq (getf frame :type) :settings))
	      (/= (getf frame :stream) 0))
      (connection-error connection))

    (when (getf frame :payload)
      (doplist (key v (getf frame :payload))
	(case key
	  (:settings-max-concurrent-streams
	   (setf stream-limit v))

	  ; A change to SETTINGS_INITIAL_WINDOW_SIZE could cause the available
	  ; space in a flow control window to become negative. A sender MUST
	  ; track the negative flow control window, and MUST NOT send new flow
	  ; controlled frames until it receives WINDOW_UPDATE frames that cause
	  ; the flow control window to become positive.
	  (:settings-initial-window-size
	   (flow-control-allowed-p connection)
	   (setf window (+ (- window window-limit) v))
	   (dohash (id stream streams)
	     (emit stream :window (+ (- (window stream) window-limit) v)))
	   (setf window-limit v))

	  (:settings-flow-control-options
	   (flow-control-allowed-p connection)
	   (when (= v 1)
	     (setf window +infinity
		   window-limit +infinity))))))))

(defmethod decode-headers ((connection connection) frame)
  #"Decode headers payload and update connection decompressor state.
    The receiver endpoint reassembles the header block by concatenating
    the individual fragments, then decompresses the block to reconstruct
    the header set - aka, header payloads are buffered until END_HEADERS,
    or an END_PROMISE flag is seen."
  (handler-case-unless *debug-mode*
      (progn
	(with-slots (decompressor) connection
	  (when (not (vectorp (getf frame :payload)))
	    (setf (getf frame :payload) (decode decompressor (getf frame :payload))))))
    (t (e) (connection-error connection :type :compression-error :msg e)))) ; ***

(defmethod encode-headers ((connection connection) frame)
  "Encode headers payload and update connection compressor state."
  (handler-case-unless *debug-mode*
      (with-slots (compressor) connection
	(when (not (vectorp (getf frame :payload)))
	  (setf (getf frame :payload) (encode compressor (getf frame :payload)))))
    (t (e) (connection-error connection :type :compression-error :msg e))))

(defmethod flow-control-allowed-p ((connection connection))
  "Once disabled, no further flow control operations are permitted."
  (with-slots (window-limit) connection
    (when (= window-limit +infinity)
      (connection-error connection :type :flow-control-error))))

(defmethod activate-stream ((connection connection) id priority &optional parent)
  #"Activates new incoming or outgoing stream and registers appropriate
    connection management callbacks."
  (with-slots (streams window-limit active-stream-count) connection
    (when (gethash id streams)
      (connection-error connection :msg "Stream ID already exists"))

    (let ((stream (make-instance 'stream :id id :priority priority
				 :window window-limit :parent parent)))

      ; Streams that are in the "open" state, or either of the "half closed"
      ; states count toward the maximum number of streams that an endpoint is
      ; permitted to open.
      (once stream :active (lambda-ignore (incf active-stream-count)))
      (once stream :close  (lambda-ignore (decf active-stream-count)))
      (when (typep connection 'server)
	(on stream :promise (lambda-apply (promise connection))))
      (on stream :frame (lambda-apply (send connection)))

      (setf (gethash id streams) stream))))

(defmethod connection-error ((connection connection) &key (type :protocol-error) (msg "Connection error"))
  #"Emit GOAWAY error indicating to peer that the connection is being
    aborted, and once sent, raise a local exception."
  (with-slots (state error) connection
    (when (and (not (eq state :closed)) (not (eq state :new)))
      (goaway connection type))
    
    (setf state :closed
	  error type)

    (when (not (stringp msg))
      (setf msg (apply #'format nil
		       (simple-condition-format-control msg)
		       (simple-condition-format-arguments msg))))

    ; in the Ruby code introspection is used to raise an Error subclass
    ; similarly we convert the keyword symbol to a defined condition
    (raise (find-symbol (symbol-name type)) msg)))


; ------------------------------------------------------------
; framer.rb
; ------------------------------------------------------------

(defparameter *max-payload-size* (1- (expt 2 16))
  "Maximum frame size (65535 bytes)")

(defparameter *max-stream-id* #x7FFFFFFF
  "Maximum stream ID (2^31)")

(defparameter *max-windowinc* #x7FFFFFFF
  "Maximum window increment value (2^31)")

(defparameter *frame-types* '(:data          #x0
			      :headers       #x1
			      :priority      #x2
			      :rst-stream    #x3
			      :settings      #x4
			      :push-promise  #x5
			      :ping          #x6
			      :goaway        #x7
			      :window-update #x9
			      :continuation  #xA))

(defparameter *frame-flags* '(:data          (:end-stream 0 :reserved 1)
			      :headers       (:end-stream 0 :reserved 1
					      :end-headers 2 :priority 3)
			      :priority      ()
			      :rst-stream    ()
			      :settings      ()
			      :push-promise  (:end-push-promise 0)
			      :ping          (:pong 0)
			      :goaway        ()
			      :window-update ()
			      :continuation  (:end-stream 0 :end-headers 1)))

(defparameter *defined-settings* '(:settings-max-concurrent-streams 4
				   :settings-initial-window-size    7
				   :settings-flow-control-options   10))

(defparameter *defined-errors* '(:no-error           0
				 :protocol-error     1
				 :internal-error     2
				 :flow-control-error 3
				 :http-stream-closed 5
				 :frame-too-large    6
				 :refused-stream     7
				 :cancel             8
				 :compression-error  9))

(defparameter *rbit*  #x7FFFFFFF)
(defparameter *rbyte* #x0FFFFFFF)
(defparameter *headerpack* "nCCN")
(defparameter *uint32* "N")

(defclass framer () ())

(defmethod common-header ((framer framer) frame)
  #"Generates common 8-byte frame header.
    - http://tools.ietf.org/html/draft-ietf-httpbis-http2-04#section-4.1"
  (let (header)

    (when (not (getf *frame-types* (getf frame :type)))
      (raise 'http2-compression-error "Invalid frame type (~A)" (getf frame :type)))

    (when (> (getf frame :length) *max-payload-size*)
      (raise 'http2-compression-error "Frame size is too large: ~D" (getf frame :length)))

    (when (> (getf frame :stream) *max-stream-id*)
      (raise 'http2-compression-error "Stream ID (~A) is too large" (getf frame :stream)))

    (when (and (eq (getf frame :type) :window-update)
	       (> (getf frame :increment) *max-windowinc*))
      (raise 'http2-compression-error "Window increment (~D) is too large" (getf frame :increment)))

    (push (getf frame :length) header)
    (push (getf *frame-types* (getf frame :type)) header)
    (push (reduce (lambda (acc f)
		    (let ((position (getf (getf *frame-flags* (getf frame :type)) f)))
		      (when (null position)
			(raise 'http2-compression-error "Invalid frame flag (~A) for ~A" f (getf frame :type)))
		      (setf (ldb (byte 1 position) acc) 1)
		      acc))
		  (getf frame :flags) :initial-value 0) header)

    (push (getf frame :stream) header)
    (pack *headerpack* (nreverse header))))

(defmethod read-common-header ((framer framer) (buf buffer))
  (let (frame)
    (destructuring-bind (flength type flags stream)
	(unpack *headerpack* (buffer-data (buffer-slice buf 0 8)))
      (setf (getf frame :length) flength)

      (setf (getf frame :type)
	    (loop for (ft pos) on *frame-types* by #'cddr if (= type pos) return ft))
      (setf (getf frame :flags)
	    (loop for (name pos) on (getf *frame-flags* (getf frame :type)) by #'cddr
	          if (logbitp pos flags) collect name))

      (setf (getf frame :stream) (logand stream *rbit*)))
    frame))

(defmethod generate ((framer framer) frame)
  #"Generates encoded HTTP 2.0 frame.
    - http://tools.ietf.org/html/draft-ietf-httpbis-http2"
  (let ((bytes (make-instance 'buffer))
	(length 0))

    (ensuref (getf frame :flags) nil)
    (ensuref (getf frame :stream) 0)
    
    (case (getf frame :type)
      (:data
       (buffer<< bytes (getf frame :payload))
       (incf length (buffer-size (getf frame :payload))))

      (:headers
       (when-let (priority (getf frame :priority))
	 (when (not (member :priority (getf frame :flags)))
	   (appendf (getf frame :flags) (list :priority))))

       (when (member :priority (getf frame :flags))
	 (buffer<< bytes (pack *uint32* (list (logand (getf frame :priority) *rbit*))))
	 (incf length 4))

       (buffer<< bytes (getf frame :payload))
       (incf length (buffer-size (getf frame :payload))))

      (:priority
       (buffer<< bytes (pack *uint32* (list (logand (getf frame :priority) *rbit*))))
       (incf length 4))

      (:rst-stream
       (buffer<< bytes (pack-error (getf frame :error)))
       (incf length 4))
      
      (:settings
       (when (/= (getf frame :stream) 0)
	 (raise 'http2-compression-error "Invalid stream ID (~A)" (getf frame :stream)))

       (doplist (k v (getf frame :payload))
	 (when (not (integerp k))
	   (setf k (getf *defined-settings* k))
	   
	   (when (null k)
	     (raise 'http2-compression-error "Unknown settings ID for ~A" k)))
	 
	 (buffer<< bytes (pack *uint32* (list (logand k *rbyte*))))
	 (buffer<< bytes (pack *uint32* (list v)))
	 (incf length 8)))

      (:push-promise
       (buffer<< bytes (pack *uint32* (list (logand (getf frame :promise-stream) *rbit*))))
       (buffer<< bytes (buffer-data (getf frame :payload)))
       (incf length (+ 4 (buffer-size (getf frame :payload)))))

      (:ping
       (when (/= (buffer-size (getf frame :payload)) 8)
	 (raise 'http2-compression-error "Invalid payload size (~D != 8 bytes)"
		(buffer-size (getf frame :payload))))

       (buffer<< bytes (getf frame :payload))
       (incf length 8))

      (:goaway
       (buffer<< bytes (pack *uint32* (list (logand (getf frame :last-stream)) *rbit*)))
       (buffer<< bytes (pack-error (getf frame :error)))
       (incf length 8))

      (:window-update
       (buffer<< bytes (pack *uint32* (list (getf frame :increment))))
       (incf length 4))

      (:continuation
       (buffer<< bytes (getf frame :payload))
       (incf length (buffer-size (getf frame :payload)))))

    (setf (getf frame :length) length)
    (buffer-prepend bytes (common-header framer frame))))

(defmethod parse ((framer framer) (buf buffer))
  #"Decodes complete HTTP 2.0 frame from provided buffer. If the buffer
    does not contain enough data, no further work is performed."
  (when (< (buffer-size buf) 8)
    (return-from parse nil))
  (let ((frame (read-common-header framer buf)))
    (when (< (buffer-size buf) (+ 8 (getf frame :length)))
      (return-from parse nil))

    (buffer-read buf 8)
    (let ((payload (buffer-read buf (getf frame :length))))

      (case (getf frame :type)
	(:data
	 (setf (getf frame :payload) (buffer-read payload (getf frame :length))))
	(:headers
	 (let ((size (getf frame :length)))
	   (when (member :priority (getf frame :flags))
	     (setf (getf frame :priority) (logand (buffer-read-uint32 payload) *rbit*))
	     (decf size 4))
	   (setf (getf frame :payload) (buffer-read payload size))))
	(:priority
	 (setf (getf frame :priority) (logand (buffer-read-uint32 payload) *rbit*)))
	(:rst-stream
	 (getf frame :error (unpack-error (buffer-read-uint32 payload))))

	(:settings
	 (setf (getf frame :payload) nil)
	 (loop
	    :repeat (/ (getf frame :length) 8)
	    :for id = (logand (buffer-read-uint32 payload) *rbyte*)
	    :for val = (buffer-read-uint32 payload)
	    ; Unsupported or unrecognized settings MUST be ignored.
	    :do (when-let (name (loop for (name v) on *defined-settings* by #'cddr
				      if (= v id) return name))
		  (setf (getf (getf frame :payload) name) val))))
	(:push-promise
	 (setf (getf frame :promise-stream) (logand (buffer-read-uint32 payload) *rbit*))
	 (setf (getf frame :payload) (buffer-read payload (- (getf frame :length) 4))))
	(:ping
	 (setf (getf frame :payload) (buffer-read payload (getf frame :length))))
	(:goaway
	 (setf (getf frame :last-stream) (logand (buffer-read-uint32 payload) *rbit*))
	 (setf (getf frame :error) (unpack-error (buffer-read-uint32 payload)))

	 (let ((size (- (getf frame :length) 8)))
	   (when (plusp size)
	     (setf (getf frame :payload) (buffer-read payload size)))))
	(:window-update
	 (setf (getf frame :increment) (logand (buffer-read-uint32 payload) *rbit*)))
	(:continuation
	 (setf (getf frame :payload) (buffer-read payload (getf frame :length))))))
    frame))

(defun pack-error (e)
  (when (not (integerp e))
    (if-let (d (getf *defined-errors* e))
      (setf e d)
      (raise 'http2-compression-error "Unknown error ID for ~A" e))

  (pack *uint32* (list e))))

(defun unpack-error (e)
  (or (loop for (name v) on *defined-errors* by #'cddr if (= v e) return name) e))


; ------------------------------------------------------------
; compressor.rb
; ------------------------------------------------------------

; Default request working set as defined by the spec.
(defparameter *req-defaults*
  '((":scheme"             . "http")
    (":scheme"             . "https") 
    (":host"               . "")
    (":path"               . "/")
    (":method"             . "GET")
    ("accept"              . "")
    ("accept-charset"      . "")
    ("accept-encoding"     . "")
    ("accept-language"     . "")
    ("cookie"              . "")
    ("if-modified-since"   . "")
    ("user-agent"          . "")
    ("referer"             . "")
    ("authorization"       . "")
    ("allow"               . "")
    ("cache-control"       . "")
    ("connection"          . "")
    ("content-length"      . "")
    ("content-type"        . "")
    ("date"                . "")
    ("expect"              . "")
    ("from"                . "")
    ("if-match"            . "")
    ("if-none-match"       . "")
    ("if-range"            . "")
    ("if-unmodified-since" . "")
    ("max-forwards"        . "")
    ("proxy-authorization" . "")
    ("range"               . "")
    ("via"                 . "")))

(defparameter *resp-defaults*
  '((":status"                     . "200")
    ("age"                         . "")
    ("cache-control"               . "")
    ("content-length"              . "")
    ("content-type"                . "")
    ("date"                        . "")
    ("etag"                        . "")
    ("expires"                     . "")
    ("last-modified"               . "")
    ("server"                      . "")
    ("set-cookie"                  . "")
    ("vary"                        . "")
    ("via"                         . "")
    ("access-control-allow-origin" . "")
    ("accept-ranges"               . "")
    ("allow"                       . "")
    ("connection"                  . "")
    ("content-disposition"         . "")
    ("content-encoding"            . "")
    ("content-language"            . "")
    ("content-location"            . "")
    ("content-range"               . "")
    ("link"                        . "")
    ("location"                    . "")
    ("proxy-authenticate"          . "")
    ("refresh"                     . "")
    ("retry-after"                 . "")
    ("strict-transport-security"   . "")
    ("transfer-encoding"           . "")
    ("www-authenticate"            . "")))

(defclass encoding-context (error-include)
  ((type :initarg :type)
   (table :reader table)
   (limit :initarg :limit :initform 4096)
   (refset :reader refset :initform (make-array 128 :element-type t :adjustable t :fill-pointer 0))))

(defmethod initialize-instance :after ((encoding-context encoding-context) &key)
  (with-slots (table type) encoding-context
    (setf table (if (eq type :request)
		    (copy-tree *req-defaults*)
		    (copy-tree *resp-defaults*)))))

(defmethod process ((encoding-context encoding-context) cmd)
  #"Performs differential coding based on provided command type.
    - http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-03#section-3.2"
  (with-slots (refset table) encoding-context
    (let (emit)

      ; indexed representation
      (if (eq (getf cmd :type) :indexed)
          ; An indexed representation corresponding to an entry not present
          ; in the reference set entails the following actions:
          ; - The header corresponding to the entry is emitted.
          ; - The entry is added to the reference set.
          ;
          ; An indexed representation corresponding to an entry present in
          ; the reference set entails the following actions:
          ;  - The entry is removed from the reference set.
          ;
	  (let* ((idx (getf cmd :name))
	 	 (cur (position idx refset :key #'car)))

	    (if cur
		(array-delete-at refset cur)
		(progn
		  (setf emit (elt table idx))
		  (vector-push-extend (cons idx emit) refset))))

          ; A literal representation that is not added to the header table
          ; entails the following action:
          ;  - The header is emitted.
          ;
          ; A literal representation that is added to the header table entails
          ; the following actions:
          ;  - The header is emitted.
          ;  - The header is added to the header table, at the location
          ;    defined by the representation.
          ;  - The new entry is added to the reference set.
          ;
	  (progn

	    (when (integerp (getf cmd :name))
	      (destructuring-bind (k . v)
		  (elt table (getf cmd :name))

		(ensuref (getf cmd :index) (getf cmd :name))
		(ensuref (getf cmd :value) v)
		(setf (getf cmd :name) k)))

	    (setf emit (cons (getf cmd :name) (getf cmd :value)))

	    (when (not (eq (getf cmd :type) :noindex))
	      (when (size-check encoding-context cmd)

		(case (getf cmd :type)
		  (:incremental
		   (setf (getf cmd :index) (length table))
		   (appendf table (list :replace-below)))
		  (:substitution
		   (when (null (assoc (getf cmd :index) table :test #'equal))
		     (raise 'http2-header-expection "invalid index")))
		  (:prepend
		   (push emit table)))

		(setf (elt table (getf cmd :index)) emit)
		(vector-push-extend (cons (getf cmd :index) emit) refset)))))
      emit)))

(defmethod add-cmd ((encoding-context encoding-context) header)
  "Emits best available command to encode provided header."
  (with-slots (table) encoding-context
    ; check if we have an exact match in header table
    (when-let (idx (position header table :test #'equal))
      (when (not (activep encoding-context idx))
	(return-from add-cmd (list :name idx :type :indexed))))

    ; check if we have a partial match on header name
    (when-let (idx (position (car header) table :key #'car :test #'equal))
      ; # default to incremental indexing
      (let ((cmd (list :name idx :value (cdr header) :type :incremental)))

	; TODO: implement literal without indexing strategy
	; TODO: implement substitution strategy (if it makes sense)
	; if default? idx
	;   cmd[:type] = :incremental
	; else
	;   cmd[:type] = :substitution
	;   cmd[:index] = idx
	; end

	(return-from add-cmd cmd)))

    (list :name (car header) :value (cdr header) :type :incremental)))

(defmethod remove-cmd ((encoding-context encoding-context) idx)
  "Emits command to remove current index from working set."
  (list :name idx :type :indexed))

(defmethod size-check ((encoding-context encoding-context) cmd)
  #"Before doing such a modification, it has to be ensured that the header
    table size will stay lower than or equal to the
    SETTINGS_HEADER_TABLE_SIZE limit. To achieve this, repeatedly, the
    first entry of the header table is removed, until enough space is
    available for the modification.
   
    A consequence of removing one or more entries at the beginning of the
    header table is that the remaining entries are renumbered.  The first
    entry of the header table is always associated to the index 0."
  (with-slots (table limit) encoding-context
    (let ((cursize (loop for (x . y) in table sum (+ (length x) (length y) 32)))
	  (cmdsize (+ (length (getf cmd :name)) (length (getf cmd :value)) 32)))

      ; The addition of a new entry with a size greater than the
      ; SETTINGS_HEADER_TABLE_SIZE limit causes all the entries from the
      ; header table to be dropped and the new entry not to be added to the
      ; header table.  The replacement of an existing entry with a new entry
      ; with a size greater than the SETTINGS_HEADER_TABLE_SIZE has the same
      ; consequences.
      (when (> cmdsize limit)
	(setf table nil)
	(return-from size-check nil))

      (let ((cur 0))
	(while (> (+ cursize cmdsize) limit)
	  (let ((e (pop table)))
	    
	    ; When the modification of the header table is the replacement of an
	    ; existing entry, the replaced entry is the one indicated in the
            ; literal representation before any entry is removed from the header
            ; table. If the entry to be replaced is removed from the header table
            ; when performing the size adjustment, the replacement entry is
            ; inserted at the beginning of the header table.
	    (when (and (eq (getf cmd :type) :substitution) (= cur (getf cmd :index)))
	      (setf (getf cmd :type) :prepend))

	    (decf cursize (+ (length (car e)) (length (cdr e)) 32))))

	t))))

(defmethod activep ((encoding-context encoding-context) idx)
  (with-slots (refset) encoding-context
    (not (null (find idx refset :key #'car :test #'equal)))))

(defmethod defaultp ((encoding-context encoding-context) idx)
  (with-slots (type) encoding-context
    (< idx (length (if (eq type :request) *req-defaults* *resp-defaults*)))))

; Header representation as defined by the spec.
(defparameter *headrep*
  '(:indexed      (:prefix 7 :pattern #x80)
    :noindex      (:prefix 5 :pattern #x60)
    :incremental  (:prefix 5 :pattern #x40)
    :substitution (:prefix 6 :pattern #x00)))

(defclass compressor ()
  ((cc-type :initarg :type)
   (cc)))

(defmethod initialize-instance :after ((compressor compressor) &key)
  (with-slots (cc cc-type) compressor
    (setf cc (make-instance 'encoding-context :type cc-type))))

(defmethod @integer ((compressor compressor) i n)
  #"Encodes provided value via integer representation.
    - http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-03#section-4.1.1
   
     If I < 2^N - 1, encode I on N bits
     Else
         encode 2^N - 1 on N bits
         I = I - (2^N - 1)
         While I >= 128
              Encode (I % 128 + 128) on 8 bits
              I = I / 128
         encode (I) on 8 bits"
  (let ((limit (1- (expt 2 n))))
    (when (< i limit)
      (return-from @integer (pack "C" (list i))))
    
    (let ((bytes (make-data-array 0)))
      (when (not (zerop n))
	(vector-push-extend limit bytes))

      (decf i limit)
      (while (>= i 128)
	(vector-push-extend (+ (mod i 128) 128) bytes)
	(setf i (/ i 128)))
      
      (vector-push-extend i bytes)
      bytes)))

(defmethod @string ((compressor compressor) str)
  #"Encodes provided value via string literal representation.
    - http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-03#section-4.1.3
  
    * The string length, defined as the number of bytes needed to store
      its UTF-8 representation, is represented as an integer with a zero
      bits prefix. If the string length is strictly less than 128, it is
      represented as one byte.
    * The string value represented as a list of UTF-8 character"
  (let ((bytes (@integer compressor (length str) 0)))
    (loop for char across str do (vector-push-extend (char-code char) bytes))
    bytes))

(defmethod header ((compressor compressor) h &optional (buffer (make-instance 'buffer)))
  (macrolet ((<<integer (i n) `(buffer<< buffer (@integer compressor ,i ,n)))
	     (<<string (s) `(buffer<< buffer (@string compressor ,s))))

    (let ((rep (getf *headrep* (getf h :type))))

      (if (eq (getf h :type) :indexed)
	  (progn
	    (<<integer (getf h :name) (getf rep :prefix)))

	  (progn
	    (if (integerp (getf h :name))
		(progn
		  (<<integer (1+ (getf h :name)) (getf rep :prefix)))
		(progn
		  (<<integer 0 (getf rep :prefix))
		  (<<string (getf h :name))))
	    
	    (when (eq (getf h :type) :substitution)
	      (<<integer (getf h :index) 0))
	    
	    (if (integerp (getf h :value))
		(progn
		  (<<integer (getf h :value) 0))
		(<<string (getf h :value)))))

      ; set header representation pattern on first byte
      (let ((fb (logior (buffer-firstbyte buffer) (getf rep :pattern))))
	(buffer-setbyte buffer 0 fb))))

  buffer)

(defmethod encode ((compressor compressor) headers)
  "Encodes provided list of HTTP headers."
  (with-slots (cc) compressor
    (let ((buffer (make-instance 'buffer))
	  (commands nil))

      ; Literal header names MUST be translated to lowercase before
      ; encoding and transmission.
      ; (setf headers (mapcar (lambda (h) (cons (string-downcase (car h)) (cdr h))) headers))
    
      ; Generate remove commands for missing headers
      (loop
	 for (idx . (wk . wv)) across (refset cc)
	 if (not (find (cons wk wv) headers :test #'equal))
	 do (push (remove-cmd cc idx) commands))

      ; Generate add commands for new headers
      (loop
	 for (hk . hv) in headers
	 if (not (find (cons hk hv) (refset cc) :key #'cdr :test #'equal))
	 do (push (add-cmd cc (cons hk hv)) commands))

      (loop
	 for cmd in (nreverse commands)
	 do (progn (process cc (copy-list cmd))
		   (let ((x (header compressor cmd)))
		     (buffer<< buffer x))))

      buffer)))

(defclass decompressor ()
  ((cc-type :initarg :type)
   (cc)))

(defmethod initialize-instance :after ((decompressor decompressor) &key)
  (with-slots (cc cc-type) decompressor
    (setf cc (make-instance 'encoding-context :type cc-type))))

(defmethod @integer ((decompressor decompressor) buf n)
  "Decodes integer value from provided buffer."
  (let* ((limit (1- (expt 2 n)))
	 (i (if (not (zerop n))
		(logand (buffer-getbyte buf) limit)
		0))
	 (m 0))

    (when (= i limit)
      (while-let (byte (buffer-getbyte buf))
	(incf i (ash (logand byte 127) m))
	(incf m 7)
	(when (zerop (logand byte 128))
	  (return))))

    i))

(defmethod @string ((decompressor decompressor) buf)
  "Decodes string value from provided buffer."
  (buffer-string (buffer-read buf (@integer decompressor buf 0))))

(defmethod header ((decompressor decompressor) buf &optional header)
  "Decodes header command from provided buffer."
  (let ((peek (buffer-getbyte buf nil)))

    (let (type)
      (loop
	 for (tt desc) on *headrep* by #'cddr
	 for prefix = (getf desc :prefix)
	 for mask = (ash (ash peek (- prefix)) prefix)
	 if (= mask (getf desc :pattern))
	 do (progn
	      (setf (getf header :type) tt
		    type desc)
	      (return)))

      (setf (getf header :name) (@integer decompressor buf (getf type :prefix)))
      (when (not (eq (getf header :type) :indexed))
	(decf (getf header :name) 1)
	 
	(when (= (getf header :name) -1)
	  (setf (getf header :name) (@string decompressor buf)))

	(when (eq (getf header :type) :substitution)
	  (setf (getf header :index) (@integer decompressor buf 0)))
	 
	(setf (getf header :value) (@string decompressor buf)))

      header)))

(defmethod decode ((decompressor decompressor) buf)
  #"Decodes and processes header commands within provided buffer.
    
    Once all the representations contained in a header block have been
    processed, the headers that are in common with the previous header
    set are emitted, during the reference set emission.
    
    For the reference set emission, each header contained in the
    reference set that has not been emitted during the processing of the
    header block is emitted.
    
    - http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-03#section-3.2.2"
  (with-slots (cc) decompressor
    (let (set)
      (while (not (buffer-empty-p buf))
	(push (process cc (header decompressor buf)) set))
      (loop
	 for (i . header) across (refset cc)
	 if (not (find header set :test #'equal))
	 do (push header set))

      (delete-if #'null (nreverse set)))))


; ------------------------------------------------------------
; stream.rb
; ------------------------------------------------------------

(defclass stream (flowbuffer-include emitter-include error-include)
  ((id :reader id :initarg :id)
   (priority :reader priority :initarg :priority)
   (window :reader window :initarg :window)
   (parent :reader parent :initarg :parent :initform nil)
   (state :reader state :initform :idle)
   (error :initform nil)
   (closed :reader closed :initform nil)
   (send-buffer :initform nil)))

(defmethod initialize-instance :after ((stream stream) &key)
  (with-slots (window) stream
    (on stream :window (lambda (v) (setf window v)))))

(defmethod receive ((stream stream) frame)
  "Processes incoming HTTP 2.0 frames. The frames must be decoded upstream."
  (with-slots (priority window) stream
    (transition stream frame nil)
  
    (case (getf frame :type)
      (:data
       (when (not (getf frame :ignore))
	 (emit stream :data frame)))
      ((or :headers :push-promise)
       (if (listp (getf frame :payload))
	   (when (not (getf frame :ignore))
	     (emit stream :headers (plist-alist (flatten (getf frame :payload)))))
	   (when (not (getf frame :ignore))
	     (emit stream :headers (getf frame :payload)))))
      (:priority
       (setf priority (getf frame :priority))
       (emit stream :priority priority))
      (:window-update
       (incf window (getf frame :increment))
       (send-data stream)))

    (complete-transition stream frame)))

(defmethod send ((stream stream) frame)
  #"Processes outgoing HTTP 2.0 frames. Data frames may be automatically
    split and buffered based on maximum frame size and current stream flow
    control window size."
  (with-slots (id priority) stream
    (transition stream frame t)
    (ensuref (getf frame :stream) id)
    
    (when (eq (getf frame :type) :priority)
      (setf priority (getf frame :priority)))

    (if (eq (getf frame :type) :data)
	(send-data stream frame)
	(emit stream :frame frame))

    (complete-transition stream frame)))

(defmethod headers ((stream stream) headers &key (end-headers t) (end-stream nil))
  "Sends a HEADERS frame containing HTTP response headers."
  (let (flags)
    (when end-headers
      (push :end-headers flags))
    (when end-stream
      (push :end-stream flags))
    (send stream (list :type :headers
		       :flags (nreverse flags)
		       :payload headers))))

(defmethod stream-promise ((stream stream) headers &optional (end-push-promise t) block) ; ***
  (when (null block)
    (error "must provide callback"))

  (let ((flags (if end-push-promise (list :end-push-promise) nil)))
    (emit stream :promise stream headers flags block)))

(defmethod reprioritize ((stream stream) p)
  #"Sends a PRIORITY frame with new stream priority value (can only be
    performed by the client)."
  (with-slots (id) stream
    (when (evenp id)
      (stream-error stream))
    (send stream (list :type :priority :priority p))))

(defmethod data ((stream stream) payload &key (end-stream t))
  "Sends DATA frame containing response payload."
  (let (flags)
    (when end-stream
      (push :end-stream flags))
    
    (while (> (buffer-size payload) *max-frame-size*)
      (let ((chunk (buffer-slice! payload 0 *max-frame-size*)))
	(send stream (list :type :data :payload chunk))))
    
    (send stream (list :type :data :flags flags :payload payload))))

(defmethod stream-close ((stream stream) &optional (error :stream-closed)) ; @ ***
  #"Sends a RST_STREAM frame which closes current stream - this does not
    close the underlying connection."
  (send stream (list :type :rst-stream :error error)))

(defmethod cancel ((stream stream))
  "Sends a RST_STREAM indicating that the stream is no longer needed."
  (send stream (list :type :rst-stream :error :cancel)))

(defmethod refuse ((stream stream))
  #"Sends a RST_STREAM indicating that the stream has been refused prior
    to performing any application processing."
  (send stream (list :type :rst-stream :error :refused-stream)))

(defmethod transition ((stream stream) frame sending)
  (with-slots (state closed) stream
    (case state
      ; All streams start in the "idle" state.  In this state, no frames
      ; have been exchanged.
      ; *  Sending or receiving a HEADERS frame causes the stream to
      ;    become "open".  The stream identifier is selected as described
      ;    in Section 5.1.1.
      ; *  Sending a PUSH_PROMISE frame marks the associated stream for
      ;    later use.  The stream state for the reserved stream
      ;    transitions to "reserved (local)".
      ; *  Receiving a PUSH_PROMISE frame marks the associated stream as
      ;    reserved by the remote peer.  The state of the stream becomes
      ;    "reserved (remote)".
      (:idle
       (if sending
	   (case (getf frame :type)
	     (:push-promise
	      (event stream :reserved-local))
	     (:headers
	      (if (end-stream-p stream frame)
		  (event stream :half-closed-local)
		  (event stream :open)))
	     (:rst-stream
	      (event stream :local-rst))
	     (otherwise
	      (stream-error stream)))
	   (case (getf frame :type)
	     (:push-promise
	      (event stream :reserved-remote))
	     (:headers
	      (if (end-stream-p stream frame)
		  (event stream :half-closed-remote)
		  (event stream :open)))
	     (otherwise
	      (stream-error stream :type :protocol-error)))))

      ; A stream in the "reserved (local)" state is one that has been
      ; promised by sending a PUSH_PROMISE frame.  A PUSH_PROMISE frame
      ; reserves an idle stream by associating the stream with an open
      ; stream that was initiated by the remote peer (see Section 8.2).
      ; *  The endpoint can send a HEADERS frame.  This causes the stream
      ;    to open in a "half closed (remote)" state.
      ; *  Either endpoint can send a RST_STREAM frame to cause the stream
      ;    to become "closed".  This also releases the stream reservation.
      ; An endpoint MUST NOT send any other type of frame in this state.
      ; Receiving any frame other than RST_STREAM or PRIORITY MUST be
      ; treated as a connection error (Section 5.4.1) of type
      ; PROTOCOL_ERROR.
      (:reserved-local
       (if sending
	   (setf state (case (getf frame :type)
			 (:headers (event stream :half-closed-remote))
			 (:rst-stream (event stream :local-rst))
			 (otherwise (stream-error stream))))
	   (setf state (case (getf frame :type)
			 (:rst-stream (event stream :remote-rst))
			 (:priority state)
			 (otherwise (stream-error stream))))))
      
      ; A stream in the "reserved (remote)" state has been reserved by a
      ; remote peer.
      ; *  Receiving a HEADERS frame causes the stream to transition to
      ;    "half closed (local)".
      ; *  Either endpoint can send a RST_STREAM frame to cause the stream
      ;    to become "closed".  This also releases the stream reservation.
      ; Receiving any other type of frame MUST be treated as a stream
      ; error (Section 5.4.2) of type PROTOCOL_ERROR.  An endpoint MAY
      ; send RST_STREAM or PRIORITY frames in this state to cancel or
      ; reprioritize the reserved stream.
      (:reserved-remote
       (if sending
	   (setf state (case (getf frame :type)
			 (:rst-stream (event stream :local-rst))
			 (:priority state)
			 (otherwise (stream-error stream))))
	   (setf state (case (getf frame :type)
			 (:headers (event stream :half-closed-local))
			 (:rst-stream (event stream :remote-rst))
			 (otherwise (stream-error stream))))))
      
      ; The "open" state is where both peers can send frames of any type.
      ; In this state, sending peers observe advertised stream level flow
      ; control limits (Section 5.2).
      ; * From this state either endpoint can send a frame with a END_STREAM
      ;   flag set, which causes the stream to transition into one of the
      ;   "half closed" states: an endpoint sending a END_STREAM flag causes
      ;   the stream state to become "half closed (local)"; an endpoint
      ;   receiving a END_STREAM flag causes the stream state to become
      ;   "half closed (remote)".
      ; * Either endpoint can send a RST_STREAM frame from this state,
      ;   causing it to transition immediately to "closed".
      (:open
       (if sending
	   (case (getf frame :type)
	     ((:data :headers :continuation)
	      (when (end-stream-p stream frame)
		(event stream :half-closed-local)))
	     (:rst-stream
	      (event stream :local-rst)))
	   (case (getf frame :type)
	     ((:data :headers :continuation)
	      (when (end-stream-p stream frame)
		(event stream :half-closed-remote)))
	     (:rst-stream
	      (event stream :remote-rst)))))
      
      ; A stream that is "half closed (local)" cannot be used for sending
      ; frames.
      ; A stream transitions from this state to "closed" when a frame that
      ; contains a END_STREAM flag is received, or when either peer sends
      ; a RST_STREAM frame.
      ; A receiver can ignore WINDOW_UPDATE or PRIORITY frames in this
      ; state.  These frame types might arrive for a short period after a
      ; frame bearing the END_STREAM flag is sent.
      (:half-closed-local
       (if sending
	   (if (eq (getf frame :type) :rst-stream)
	       (event stream :local-rst)
	       (stream-error stream))
	   (case (getf frame :type)
	     ((:data :headers :continuation)
	      (when (end-stream-p stream frame)
		(event stream :remote-closed)))
	     (:rst-stream
	      (event stream :remote-rst))
	     ((:window-update :priority)
	      ; *** In the Ruby :ignore on the next line is misspelled :igore
	      (setf (getf frame :ignore) t)))))

      ; A stream that is "half closed (remote)" is no longer being used by
      ; the peer to send frames.  In this state, an endpoint is no longer
      ; obligated to maintain a receiver flow control window if it
      ; performs flow control.
      ; If an endpoint receives additional frames for a stream that is in
      ; this state it MUST respond with a stream error (Section 5.4.2) of
      ; type STREAM_CLOSED.
      ; A stream can transition from this state to "closed" by sending a
      ; frame that contains a END_STREAM flag, or when either peer sends a
      ; RST_STREAM frame.
      (:half-closed-remote
       (if sending
	   (case (getf frame :type)
	     ((:data :headers :continuation)
	      (when (end-stream-p stream frame)
		(event stream :local-closed)))
	     (:rst-stream
	      (event stream :local-rst)))
	   (case (getf frame :type)
	     (:rst-stream
	      (event stream :remote-rst))
	     (:window-update
	      (setf (getf frame :ignore) t))
	     (otherwise
	      (stream-error stream :type :stream-closed)))))

      ; An endpoint MUST NOT send frames on a closed stream. An endpoint
      ; that receives a frame after receiving a RST_STREAM or a frame
      ; containing a END_STREAM flag on that stream MUST treat that as a
      ; stream error (Section 5.4.2) of type STREAM_CLOSED.
      ;
      ; WINDOW_UPDATE or PRIORITY frames can be received in this state for
      ; a short period after a a frame containing an END_STREAM flag is
      ; sent.  Until the remote peer receives and processes the frame
      ; bearing the END_STREAM flag, it might send either frame type.
      ;
      ; If this state is reached as a result of sending a RST_STREAM
      ; frame, the peer that receives the RST_STREAM might have already
      ; sent - or enqueued for sending - frames on the stream that cannot
      ; be withdrawn. An endpoint MUST ignore frames that it receives on
      ; closed streams after it has sent a RST_STREAM frame.
      ;
      ; An endpoint might receive a PUSH_PROMISE or a CONTINUATION frame
      ; after it sends RST_STREAM. PUSH_PROMISE causes a stream to become
      ; "reserved". If promised streams are not desired, a RST_STREAM can
      ; be used to close any of those streams.
      (:closed
       (if sending
	   (case (getf frame :type)
	     (:rst-stream nil)
	     (otherwise
	      (when (not (eq (getf frame :type) :rst-stream))
		(stream-error stream :type :stream-closed))))
	   (case closed
	     ((:remote-rst :remote-closed)
	      (when (not (eq (getf frame :type) :rst-stream))
		(stream-error stream :type :stream-closed)))
	     ((or :local-rst :local-closed)
	      (setf (getf frame :ignore) t))))))))

(defmethod event ((stream stream) newstate)
  (with-slots (state closed) stream
    (case newstate
      (:open
       (setf state newstate)
       (emit stream :active))
      ((:reserved-local :reserved-remote)
       (setf state newstate)
       (emit stream :reserved))
      ((:half-closed-local :half-closed-remote)
       (setf closed newstate)
       (unless (eq state :open)
	 (emit stream :active))
       (setf state :half-closing))
      (((or :local-closed :remote-closed :local-rst :remote-rst)
	(setf closed newstate)
	(setf state :closing))))
    state))

(defmethod complete-transition ((stream stream) frame)
  (with-slots (state closed) stream
    (case state
      (:closing
       (setf state :closed)
       (emit stream :close (getf frame :error)))
      (:half-closing
       (setf state closed)
       (emit stream :half-close)))))

(defmethod end-stream-p ((stream stream) frame)
  (case (getf frame :type)
    ((:data :headers :continuation)
     (if (member :end-stream (getf frame :flags)) t nil))
    (otherwise nil)))

(defmethod stream-error ((stream stream) &key (type :http-stream-error) (msg "Stream error"))
  (with-slots (error state) stream
    (setf stream type)
    (when (not (eq state :closed))
      (stream-close stream type))

    ; in the Ruby code introspection is used to raise an Error subclass
    ; similarly we convert the keyword symbol to a defined condition
    (raise (find-symbol (symbol-name type)) msg)))


; ------------------------------------------------------------
; client.rb
; ------------------------------------------------------------

(defclass client (connection)
  ((stream-id :initform 1)
   (state :initform :connection-header)
   (compressor :accessor client-compressor :initarg :compressor :initform (make-instance 'compressor :type :request))
   (decompressor :accessor client-decompressor :initarg :decompressor :initform (make-instance 'decompressor :type :response))))

(defmethod send :before ((client client) frame)
  (with-slots (state stream-limit window-limit) client
    (when (eq state :connection-header)
      (emit client :frame (buffer-simple *connection-header*))
      (setf state :connected)
      (settings client stream-limit window-limit))))


; ------------------------------------------------------------
; server.rb
; ------------------------------------------------------------

(defclass server (connection)
  ((stream-id :initform 2)
   (state :initform :new)
   (compressor :accessor compressor :initarg :compressor :initform (make-instance 'compressor :type :response))
   (decompressor :accessor decompressor :initarg :decompressor :initform (make-instance 'decompressor :type :request))))

; what initialize accomplishes has been rolled into the defclass form above

(defmethod promise ((server server) &rest args &aux (callback (shift args)))
  (destructuring-bind (parent headers flags) args
    (let ((promise (new-stream server :parent parent)))
      (send promise (list :type :push-promise
			  :flags flags
			  :stream (id parent)
			  :promise-stream (id promise)
			  :payload headers))

      (funcall callback promise))))

; ------------------------------------------------------------
; testing
; ------------------------------------------------------------

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
  (server-len (:pointer :unsigned-int))
  (client (:pointer :unsigned-char))
  (client-len :unsigned-int))

(cffi:defcfun ("next_protos_parse" next-protos-parse)
    :string
  (outlen (:pointer :unsigned-short))
  (in :string))

(defconstant +OPENSSL_NPN_UNSUPPORTED+ 0)
(defconstant +OPENSSL_NPN_NEGOTIATED+ 1)
(defconstant +OPENSSL_NPN_NO_OVERLAP+ 2)
(defconstant +SSL_TLSEXT_ERR_OK+ 0)

(cffi:defcstruct tlsextnextprotoctx
  (data :string)
  (len :unsigned-int))

(cffi:defcallback lisp-next-proto-cb :int
    ((s ssl-pointer) (data (:pointer :pointer)) (len (:pointer :unsigned-int)) (arg (:pointer tlsextnextprotoctx)))
  (declare (ignore s))
  (let (tmp-data tmp-len)
    (cffi:with-foreign-slots ((data len) arg tlsextnextprotoctx)
      (setf tmp-data data
	    tmp-len len))
    (setf (cffi:mem-ref data :string) tmp-data
	  (cffi:mem-ref len :unsigned-int) tmp-len))
  +SSL_TLSEXT_ERR_OK+)

(defun make-ssl-client-stream
    (socket &key certificate key password (method 'ssl-v23-method) external-format
     close-callback (unwrap-stream-p t) next-protos-spec)
  "Returns an SSL stream for the client socket descriptor SOCKET.
CERTIFICATE is the path to a file containing the PEM-encoded certificate for
 your client. KEY is the path to the PEM-encoded key for the client, which
may be associated with the passphrase PASSWORD."
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
      (cffi:with-foreign-object (arg 'tlsextnextprotoctx)
	(setf (cffi:foreign-slot-value arg 'tlsextnextprotoctx 'data) nps)
	(setf (cffi:foreign-slot-value arg 'tlsextnextprotoctx 'len) (length nps))
	(ssl-ctx-set-next-protos-advertised-cb *ssl-global-context*
					       (cffi:callback lisp-next-proto-cb)
					       arg)
	(ensure-ssl-funcall stream handle #'ssl-connect handle)))

    (when (ssl-check-verify-p)
      (ssl-stream-check-verify stream))
    (handle-external-format stream external-format)))

(defun pack-next-protos-spec (next-protos-spec)
  "Convert a list of NPN protocol names into a single concatenated string of length-prefixed names."
  (with-output-to-string (s)
    (dolist (p next-protos-spec)
      (assert (and (stringp p) (plusp (length p)) p "Entry in NPN list must be a string with 1+ characters."))
      (princ (code-char (length p)) s)
      (princ p s))))

(defun make-ssl-server-stream
    (socket &key certificate key password (method 'ssl-v23-method) external-format
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

    (let ((nps (pack-next-protos-spec next-protos-spec)))
      (cffi:with-foreign-object (arg 'tlsextnextprotoctx)
	(setf (cffi:foreign-slot-value arg 'tlsextnextprotoctx 'data) nps)
	(setf (cffi:foreign-slot-value arg 'tlsextnextprotoctx 'len) (length nps))
	(ssl-ctx-set-next-protos-advertised-cb *ssl-global-context*
					       (cffi:callback lisp-next-proto-cb)
					       arg)
	(ensure-ssl-funcall stream handle #'ssl-accept handle)))
    
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

(in-package :http2)

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
      (format t "NPN: ~A~%" npn)
      (unless (member npn *next-protos-spec* :test #'string=)
	; this can happen if the other end wants to do HTTP/1.1
	(raise 'http2-handshake-error)))))

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
  (:documentation "Regular socket"))

(defmethod net-socket-listen ((net net-plain) host port)
  (with-slots (listener) net
    (let ((server (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
      (setf (sb-bsd-sockets:sockopt-reuse-address server) t)
      (sb-bsd-sockets:socket-bind server (usocket:dotted-quad-to-vector-quad host) port)
      (sb-bsd-sockets:socket-listen server 8)
      (setf listener server))))

(defmethod net-socket-accept ((net net-plain))
  (with-slots (raw-socket listener) net
    (setf raw-socket (sb-bsd-sockets:socket-accept listener))))

(defmethod net-socket-prepare-server ((net net-plain))
  (with-slots (raw-socket socket) net
    (setf socket raw-socket)))

(defmethod net-socket-close ((net net-plain))
  (with-slots (socket) net
    (sb-bsd-sockets:socket-close socket)))

(defmethod net-socket-shutdown ((net net-plain))
  (with-slots (listener) net
    (if listener
	(sb-bsd-sockets:socket-close listener))))

(defmethod net-write-vector ((net net-plain) bytes n)
  (with-slots (socket) net
    (sb-bsd-sockets:socket-send socket bytes n)))

(defmethod net-read-vector ((net net-plain) bytes n)
  (with-slots (socket) net
    (multiple-value-bind (buf bytes-read peer)
	(sb-bsd-sockets:socket-receive socket bytes n)
      (declare (ignore buf peer))
      bytes-read)))

(defmethod net-finish-output ((net net-plain))
  nil)

(defparameter *dump-bytes* t)
(defparameter *dump-bytes-stream* t)
(defparameter *dump-bytes-hook* nil)

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
	   (do-client-actual uri)
	 (connection-refused-error ()
	   (format t "Connection refused: ~A~%" uri)))
    (finish-output)))

(defun do-client-actual (uri)
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

(defun do-server (&key net (interface "0.0.0.0") (port 8080) secure-p)
  (unwind-protect
       (handler-case
	   (progn
	     (unless net
	       (setf net (make-instance (if secure-p 'net-ssl 'net-plain))))
	     (format t "Starting server on port ~D~%" port)
	     (net-socket-listen net interface port)
	     (unwind-protect
		  (loop
		     (do-server-actual net))
	       (net-socket-close net)
	       (net-socket-shutdown net)))
	 (address-in-use-error ()
	   (format t "Address already in use.~%")))
    (net-finish-output net)))

(defun do-server-actual (net)
  (net-socket-accept net)
  (format t "New TCP connection!~%")
  (net-socket-prepare-server net)

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



; --- HTTP PUSH TEST:

(defun do-server-actual (net)
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
		  (format t "client closed its end of the stream~%")
		  (let ((responses (loop
				      (restart-case
					  (return (dispatcher req))
					(restart-request ()
					  :report "Restart HTTP request processing.")
					(service-unavailable ()
					  :report "Resolve HTTP request by issuing a 503"
					  (return (list (convenience-status 503 req "Service Unavailable"))))))))
		    (send-responses-by-policy stream responses 'policy-1)))))))
      
    (receive-loop net conn)))

; dispatcher returns 1 or more responses, the first being the main response and 
; subsequent responses being promises. Each response is a list containing the
; headers in an alist form, and the payload as a buffer object or a function
; that will yield a buffer object when called with the headers as a parameter
(defun send-responses (stream responses)
  (destructuring-bind ((main-request main-headers main-payload) . promises) responses
    (declare (ignore main-request))
    (flet ((send-response (rstream rheaders rpayload)
	     (when (functionp rpayload)
	       (setf rpayload (funcall rpayload rheaders)))
	     (headers rstream rheaders)
	     (data rstream rpayload :end-stream t)))
      (let (promise-streams)
	(format t "About to send promises~%")
	(loop
	   for (request headers payload) in promises
	   do (stream-promise stream request t (lambda (p) (push p promise-streams))))
	(format t "About to send main response~%")
	(send-response stream main-headers main-payload)
	(format t "About to send promised responses~%")
	(loop
	   for (request headers payload) in promises
	   for pstream in (nreverse promise-streams)
	   do (send-response pstream headers payload))))))


; accepts a mini-language:
;
;  (for promises (send (request promise)))
;  (send (headers main) (data main))
;  (for promises (send (headers promise)))
;  (for promises (send (data promise)))
;
(defmacro define-promise-policy (name &body order)
  (labels ((preprocess (tree)
	     (cond
	       ((atom tree) tree)
	       ((listp tree)
		(if (eq (car tree) 'send)
		    (subst 's-request 'request* (subst 's-headers 'headers* (subst 's-data 'data* tree)))
		    (let ((tree (subst 'request* 'request (subst 'headers* 'headers (subst 'data* 'data tree)))))
		      (cons (preprocess (car tree))
			    (preprocess (cdr tree)))))))))
    (let ((doc (if (stringp (car order)) (pop order) "Send responses.")))
      `(defmethod send-responses-by-policy (stream responses (policy-name (eql ',name)))
	 ,doc
	 (format t "Reponses:~%~S~%" responses)
	 (format t "Executing promise-policy ~A:~%~S~%" policy-name ',order)
	 (let ((main (car responses)) promises promises-loaded)
	   (declare (ignorable promises promises-loaded))
	   (macrolet ((load-promises ()
			`(progn
			   (unless promises-loaded
			     (setf promises
				   (mapcar (lambda (p)
					     (if (and (listp (second p)) (typep (third p) 'buffer))
						 p
						 (convenience (mapcar (lambda (x) (if (functionp x) (funcall x) x)) p)
							      (first main))))
					   (cdr responses))
				   promises-loaded t))
			   promises))
		      (for (list &body body)
			(case list
			  (promises `(progn
				       (load-promises)
				       (format t "Looping over ~D promises~%" (length promises))
				       (dolist (promise promises) ,@body)))
			  (responses `(progn
					(load-promises)
					(format t "Looping over ~D responses~%" (length responses))
					(dolist (response responses) ,@body)))))
		      (send* (item)
			(destructuring-bind (type place) item
			  (case type
			    (s-request `(progn
					  (format t "Sending promise~%")
					  (stream-promise stream (first ,place) t (lambda (p) (appendf ,place p)))))
			    (s-headers `(progn
					  (format t "Sending headers~%")
					  (headers (or (fourth ,place) stream) (second ,place)
						   :end-stream (zerop (buffer-size (third ,place)))
						   )))
			    (s-data    `(when (plusp (buffer-size (third ,place)))
					  (format t "Sending data~%")
					  (data (or (fourth ,place) stream) (third ,place) :end-stream t))))))
		      (send (&rest items)
			`(progn ,@(loop for item in items collect `(send* ,item))))
		      (request* (item)
			`(first ,item))
		      (headers* (item)
			`(second ,item))
		      (data* (item)
			`(third ,item))
		      (get-header (item name)
			`(cdr (assoc ,name ,item :test #'string=))))
	     ,@(preprocess order)))))))

(define-promise-policy policy-0
  (send (headers main) (data main)))

(define-promise-policy policy-0b
  (send (headers main) (data main))
  (for promises (send (request promise)))
  (for promises (send (headers promise)))
  (for promises (send (data promise))))

(define-promise-policy policy-1
  (for promises (send (request promise)))
  (send (headers main) (data main))
  (for promises (send (headers promise) (data promise))))

(define-promise-policy policy-2
  (send (headers main))
  (for promises (send (request promise)))
  (send (data main))
  (for promises (send (headers promise) (data promise))))

(define-promise-policy policy-3
  (for promises (send (request promise)))
  (send (headers main) (data main))
  (for promises (send (headers promise)))
  (for promises (send (data promise))))

(define-promise-policy policy-4
  (send (headers main))
  (for promises (send (request promise)))
  (send (data main))
  (for promises (send (headers promise)))
  (for promises (send (data promise))))

(define-promise-policy policy-5
  (send (headers main))
  (for promises (send (request promise)))
  (for promises (send (headers promise)))
  (send (data main))
  (for promises (send (data promise))))

(define-promise-policy policy-6
  (send (headers main))
  (setf promises (remove-if-not
		  (lambda (p)
		    (let ((extension (pathname-type (get-header (request p) ":path"))))
		      (member extension '("css" "js") :test #'string=))) (load-promises)))
  (for promises (send (request promise)))
  (send (data main))
  (for promises (send (headers promise) (data promise))))


(defun dispatcher (request-headers)
  (mapcar (lambda (response) (convenience response request-headers)) (file-dispatcher request-headers)))

(defun convenience (response original-request-headers)
  (destructuring-bind (request-headers response-headers response-payload) response
    ; convert non-strings
    (dolist (set (list request-headers response-headers))
      (dolist (header set)
	(symbol-macrolet ((name (car header)) (value (cdr header)))
	  (unless (stringp name) (setf name (format nil "~(~S~)" name)))
	  (unless (stringp value) (setf value (format nil "~A" value))))))
    ; populate non-existent entries
    (macrolet ((get-h (set name &optional default) `(or (cdr (assoc ,name ,set :test #'string=)) ,default))
	       (set-h (set name value) `(setf ,set (acons ,name ,value ,set)))
	       (default (set name value) `(unless (get-h ,set ,name) (set-h ,set ,name ,value))))
      ; populate request (for push promises, this is new data coming back)
      (default request-headers ":path"      "/")
      (default request-headers ":host"      (get-h original-request-headers ":host"
						   (get-h original-request-headers ":authority")))
      (default request-headers ":authority" (get-h original-request-headers ":authority"
						   (get-h request-headers ":host")))
      (default request-headers ":scheme"    (get-h original-request-headers ":scheme" "http"))
      (default request-headers ":method"    "GET")
      ; populate response
      (default response-headers "content-type" "text/plain")
      (ensuref response-payload (make-instance 'buffer))
      (when (typep response-payload 'string)
	(setf response-payload (buffer-simple response-payload)))
      (when (typep response-payload 'buffer)
	(default response-headers "content-length" (format nil "~D" (buffer-size response-payload))))
      (default response-headers ":status"   "200")
      ; return a response list
      (list request-headers response-headers response-payload))))

(defun convenience-status (status original-request message)
  (convenience (list original-request
		     (list (cons :status status))
		     (if message (buffer-simple message)))
	       original-request))

(defun file-dispatcher (request)
  (macrolet ((req-header (name) `(cdr (assoc ,name request :test #'string=)))
	     (forbid () `(return-from file-dispatcher `((,request ((":status" . "403")) "Forbidden")))))
    (let ((path (req-header ":path")))
      (when (char= (char path (1- (length path))) #\/)
	(setf (req-header ":path") (concatenate 'string path "index.html"))
	(invoke-restart 'restart-request))
      (when (search "/../" path)
	(forbid))
      (when (search "kerry.jpg" path)
	(forbid))
      `((,request ,@(file-response path))
	(((":path" . "/kerry.jpg")) ,@(file-response "/kerry.jpg"))))))

(defun file-response (name)
  (let ((path (concatenate 'string "/home/ubuntu/wwwroot" name)))
    (with-open-file (stream path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
      (if stream
	  (let ((buffer (make-instance 'buffer)))
	    (read-sequence (buffer-data (buffer-adjust buffer (file-length stream)))
			   stream :end (file-length stream))
	    `((("content-type" . ,(mime-type-for-ext (pathname-type path)))
	       ("cache-control" . "max-age=5")) ,buffer))
	  `(((":status" . "404")) "File Not Found")))))

(defun mime-type-for-ext (ext)
  "Just a short list of example MIME types to make testing easy."
  (or (cdr (assoc ext '(("htm"  . "text/html")
			("html" . "text/html")
			("txt"  . "text/plain")
			("gif"  . "image/gif")
			("jpg"  . "image/jpeg")
			("jpeg" . "image/jpeg")
			("png"  . "image/png")
			("webp" . "image/webp")
			("css"  . "text/css")
			("js"   . "application/javascript")
			("avi"  . "video/avi")
			("mpg"  . "video/mpeg")
			("mpeg" . "video/mpeg")
			("mp4"  . "video/mp4")
			("ogg"  . "video/ogg")
			("qt"   . "video/quicktime")
			("xml"  . "text/xml")
			("json" . "application/json")
			("rtf"  . "text/rtf")
			("pdf"  . "application/pdf")
			("csv"  . "text/csv")
			("gz"   . "application/gzip")
			("bin"  . "application/octet-stream")
			("out"  . "application/octet-stream"))
		  :test #'string=))
      "text/plain"))
