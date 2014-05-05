; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol-util)

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

(defun reverse-plist (plist)
  "Reverse PLIST non-destructively, e.g. (:X 1 :Y 2 :Z 3) becomes (:Z 3 :Y 2 :X 1)"
  (loop with n for (k v) on plist by #'cddr do (push v n) do (push k n) finally (return n)))

(defmacro dohash ((key-name value-name hash-table-name) &rest body)
  "Similar to DOLIST, but for hashes. Perform the BODY once for each key/value pair.
Set KEY-NAME and VALUE-NAME appropriately for each iteration."
  (let ((iterator (gensym "HASHITER"))
	(value-present (gensym "HASHNEXT")))
    `(with-hash-table-iterator (,iterator ,hash-table-name)
       (loop
	  (multiple-value-bind (,value-present ,key-name ,value-name) (,iterator)
	    (declare (ignorable ,key-name ,value-name))
	    (unless ,value-present (return nil))
	    ,@body)))))

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

(defmacro pack (control values)
  "A macro that expands into code to pack VALUES into bytes per the template in CONTROL.
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
  "A macro that expands into code to unpack BYTES into values per the template in CONTROL.
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


(defun split-if (predicate string)
  "Split STRING on delimiter groups wherein each character is true for function PREDICATE."
  (loop
     for beg = 0 then (position-if-not predicate string :start (1+ end))
     for end = (position-if predicate string :start beg)
     collect (subseq string beg end)
     while end))

(defvar *debug-mode* t)
(defvar *debug-stream* t)

(defmacro handler-case-unless (var expression &body clauses)
  "Expands into code that gives two paths, depending on the run-time value of a variable.
When the variable is true, exceptions bubble to the control plane; when false, they are handled.
A global such as *debug-mode* can be used throughout as the variable."
  (with-gensyms (fn)
    `(flet ((,fn () ,expression))
       (if ,var
	   (,fn)
	   (handler-case
	       (,fn)
	     ,@clauses)))))

(defun report-error (e)
  (format *debug-stream* "Error: ~A " (type-of e))
  (if (typep e 'simple-condition)
      (progn
	(apply #'format *debug-stream*
	       (simple-condition-format-control e)
	       (simple-condition-format-arguments e))
	(format *debug-stream* "~%"))
      (format *debug-stream* "~A~%" e)))
