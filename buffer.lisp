; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol)

; Port notes: We cannot subclass STRING in CL to accomplish what we
; want. We have less utility methods out-of-the-box, so we'll define a
; bunch of methods similar to the Ruby String ones. We start with some
; vector primitives, and then define the BUFFER class, which will have
; a slot carrying the actual binary data.

(defun vector-overwrite (src dest n)
  "Modifies vector DEST by writing the elements of vector SRC to the beginning, and returns DEST."
  (declare (type (vector (unsigned-byte 8)) src dest))
  (loop
     for i below n
     do (setf (aref dest i) (aref src i)))
  dest)

(defun vector-concat (src dest)
  "Modifies vector DEST by concatenating the elements of vector SRC to the end, and returns DEST.
If DEST is too small, it will be adjusted. If DEST has a fill pointer, it will be set to the new end."
  (declare (type (vector (unsigned-byte 8)) src dest))
  (let* ((src-len (length src))
	 (dest-len (length dest))
	 (new-dest-len (+ src-len dest-len)))
    (when (> new-dest-len (array-dimension dest 0))
      (adjust-array dest new-dest-len))
    (loop
       for i below src-len
       for j from dest-len below new-dest-len
       do (setf (aref dest j) (aref src i)))
    (when (array-has-fill-pointer-p dest)
      (setf (fill-pointer dest) new-dest-len)))
  dest)

(defun vector-prepend (src dest)
  "Modifies vector DEST by prepending the elements of vector SRC to the beginning, and returns DEST.
If DEST is too small, it will be adjusted. If DEST has a fill pointer, it will be set to the new end."
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
       for i below src-len
       do (setf (aref dest i) (aref src i)))
    (when (array-has-fill-pointer-p dest)
      (setf (fill-pointer dest) dest-new-len)))
  dest)

(defun vector-splice (src dest n)
  "Splice N bytes from the beginning of vector SRC into vector DEST, and return DEST. Destroys any
existing value in DEST; the bytes are copied into the front and the fill pointer is reset to N.
Removes the bytes copied from SRC by copying the remaining bytes forward and resetting the fill
pointer to the new length (original length - N). If SRC does not have at least N bytes, NO bytes are
copied, and the fill pointer of DEST is set to zero and DEST is returned. If DEST is not long enough
to contain N bytes, ADJUST-ARRAY is called to expand it, and the operation proceeds as normal."
  (declare (type (vector (unsigned-byte 8)) src dest))
  (let* ((src-len (length src))
	 (src-new-len (- src-len n)))
    (when (< src-len n)
      (when (array-has-fill-pointer-p dest)
	(setf (fill-pointer dest) 0))
      (return-from vector-splice dest))
    (when (> n (array-dimension dest 0))
      (adjust-array dest n))
    (loop
       for i below n
       do (setf (aref dest i) (aref src i)))
    (when (array-has-fill-pointer-p dest)
      (setf (fill-pointer dest) n))
    (loop
       for i below src-new-len
       for j from n below src-len
       do (setf (aref src i) (aref src j)))
    (when (array-has-fill-pointer-p src)
      (setf (fill-pointer src) src-new-len))
    dest))

(defun vector-delete (vector start length)
  "Modifies VECTOR by deleting bytes at position START for length LENGTH, and returns VECTOR.
The deleted bytes are not saved. The fill pointer for VECTOR is updated to the new, shorter length."
  (let* ((old-len (length vector))
	 (new-len (- old-len length))
	 (start2 (+ start length)))
    (loop
       for i from start below new-len
       for j from start2 below old-len
       do (setf (aref vector i) (aref vector j)))
    (when (array-has-fill-pointer-p vector)
      (setf (fill-pointer vector) new-len))
    vector))

(defun vector-delete-at (vector index)
  "Modifies VECTOR by deleting the single byte at position INDEX."
  (vector-delete vector index 1))

(defun vector-slice (src start length &optional (dest (make-data-vector length)))
  "Copies bytes from SRC to DEST from position START of length LENGTH, and returns DEST.
SRC should be a vector of bytes. DEST should be a vector of bytes with a fill pointer
that either definitely has a size equal or greater than LENGTH or is adjustable."
  (declare (type (vector (unsigned-byte 8)) src dest))
  (when (> length (array-dimension dest 0))
    (adjust-array dest length))
  (loop
     for i from 0 below length
     for j from start below (+ start length)
     do (setf (aref dest i) (aref src j)))
  (when (array-has-fill-pointer-p dest)
    (setf (fill-pointer dest) length))
  dest)

(defun vector-inspect (bytes &key (max-length 512))
  "Produce a string which is a mixture of characters and escaped values.
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

(defun make-data-vector (n)
  "Call MAKE-ARRAY for size N (but not less than a standard size) for an adjustable fill-pointer byte array."
  (when (> n #.(* 64 1024))
    (raise simple-condition "MAKE-DATA-VECTOR called with ~A as N value, greater than safety maximum." n))
  (make-array (max n 1024) :element-type '(unsigned-byte 8) :adjustable t :fill-pointer n))

; buffer operations

(defclass buffer ()
  ((vectordata :accessor buffer-data :initarg :data :initform (make-data-vector 0)))
  (:documentation "Class for byte data operations"))

(defmethod bufferp ((buffer buffer))
  t)

(defmethod bufferp (non-buffer)
  nil)

(defmethod buffer-empty-p ((buffer buffer))
  "Pedicate to indicate T if BUFFER is empty or NIL if BUFFER contains bytes."
  (zerop (length (buffer-data buffer))))

(defmethod buffer-adjust ((buffer buffer) size)
  "Ensure BUFFER is at least SIZE large and set the fill pointer to SIZE."
  (when (< (array-dimension (buffer-data buffer) 0) size)
    (adjust-array (buffer-data buffer) size))
  (setf (fill-pointer (buffer-data buffer)) size)
  buffer)

(defmethod buffer<< ((buffer buffer) (vector vector))
  "Modifies BUFFER by concatenating the byte numbers in VECTOR, and returns BUFFER."
  (vector-concat vector (buffer-data buffer))
  buffer)

(defmethod buffer<< ((buffer buffer) (vector simple-vector))
  "Modifies BUFFER by concatenating the byte numbers in VECTOR, and returns BUFFER."
  (vector-concat (make-array (length vector) :element-type '(unsigned-byte 8) :initial-contents vector) (buffer-data buffer))
  buffer)

(defmethod buffer<< ((buffer buffer) (string string))
  "Modifies BUFFER by concatenating a STRING, and returns BUFFER."
  (buffer<< buffer (babel:string-to-octets string)))

(defmethod buffer<< ((buffer buffer) (integer integer))
  "Modifies BUFFER by concatenating an ASCII character designated by INTEGER, and returns BUFFER."
  (assert (<= 0 integer 255) (integer) "Integer provided is ~A but must be from 0-255.")
  (vector-push-extend integer (buffer-data buffer))
  buffer)

(defmethod buffer<< ((buffer buffer) (character character))
  "Modifies BUFFER by concatenating an ASCII character designated by CHARACTER, and returns BUFFER."
  (buffer<< buffer (char-code character)))

(defmethod buffer<< ((buffer buffer) (buffer2 buffer))
  "Modifies BUFFER by concatenating the contents of BUFFER2 to BUFFER, and returns BUFFER."
  (buffer<< buffer (buffer-data buffer2)))

(defmethod buffer-overwrite ((buffer buffer) (vector vector))
  (vector-overwrite vector (buffer-data buffer) (length vector))
  buffer)

(defmethod buffer-overwrite ((buffer buffer) (buffer2 buffer))
  (vector-overwrite (buffer-data buffer2) (buffer-data buffer) (buffer-size buffer2))
  buffer)

; a BUFFER<< call that has (PACK ...) as its parameter will be written to a PACK call
; using the BUFFER-DATA array directly, instead of creating a temporary array which
; is then appended onto the BUFFER-DATA array
;
; e.g.
;  (BUFFER<< BUFFER (PACK *UINT32* NUM))
; becomes
;  (LET ((#:G (BUFFER-DATA BUFFER)))
;    (PACK *UINT32* NUM :ARRAY #:G :START (FILL-POINTER #:G))
;
; ...which is slightly faster because a temporary array is not created, but rather the
; bytes are being added by the PACK maco directly into the BUFFER-DATA
;
(define-compiler-macro buffer<< (&whole whole buffer thing)
  (if (and (listp thing) (eq (car thing) 'pack))
      (with-gensyms (data)
	`(let ((,data (buffer-data ,buffer)))
	   ,(append thing `(:array ,data :start (fill-pointer ,data)))
	   ,buffer))
      whole))

(defun buffer-simple (&rest items)
  "Create a new BUFFER object and populate it with contents of each of ITEMS.
Each of ITEMS may be another BUFFER, a vector of bytes, a string, or an integer 0-255."
  (let ((buffer (make-instance 'buffer)))
    (dolist (item items buffer)
      (buffer<< buffer item))))

(defmethod buffer-prepend ((buffer buffer) (vector vector))
  "Modifies BUFFER by prepending BUFFER with the byte numbers in VECTOR, and returns BUFFER."
  (vector-prepend vector (buffer-data buffer))
  buffer)

(defmethod buffer-firstbyte ((buffer buffer))
  "Returns the first byte number in BUFFER without making any modification."
  (aref (buffer-data buffer) 0))

(defmethod buffer-firstchar ((buffer buffer))
  "Returns the first byte in BUFFER as a character, without making any modification."
  (code-char (aref (buffer-data buffer) 0)))

(defmethod buffer-getbyte ((buffer buffer) &optional (remove t))
  "Returns the first byte number in BUFFER. Modifies BUFFER to remove it if REMOVE is true."
  (prog1 (aref (buffer-data buffer) 0)
    (when remove (vector-delete-at (buffer-data buffer) 0))))

(defalias buffer-readbyte buffer-getbyte)

(defmethod buffer-setbyte ((buffer buffer) index value)
  "Modifies BUFFER to set a particular byte at INDEX in BUFFER to VALUE, and returns BUFFER."
  (setf (aref (buffer-data buffer) index) value)
  buffer)

(defmethod buffer-size ((buffer buffer))
  "Returns the length of BUFFER without modifying it."
  (length (buffer-data buffer)))

(defmethod buffer-read ((buffer buffer) n)
  "Modifies BUFFER by splicing N bytes from the front into a new buffer which is returned.
If N is greater than the size of BUFFER, only the remaining bytes in BUFFER are spliced."
  (if (< n (length (buffer-data buffer)))
      (let ((spliced (make-instance 'buffer :data (make-data-vector n))))
	(vector-splice (buffer-data buffer) (buffer-data spliced) n)
	spliced)
      ;; most likely case for else branch is n = length
      (let ((spliced (make-instance 'buffer :data (make-data-vector 0))))
	(rotatef (buffer-data buffer) (buffer-data spliced))  ; faster than copying bytes
	spliced)))

(defmethod buffer-delete-section ((buffer buffer) start length)
  "Modifies BUFFER by deleting bytes from position START of length LENGTH, and returns BUFFER."
  (vector-delete (buffer-data buffer) start length)
  buffer)

(defmethod buffer-slice ((buffer buffer) start length)
  "Copies bytes from BUFFER from position START of length LENGTH into a new buffer which is returned."
  (let ((data (vector-slice (buffer-data buffer) start length)))
    (make-instance 'buffer :data data)))

(defmethod buffer-slice! ((buffer buffer) start length)
  "Modifies BUFFER by removing the bytes from position START of length LENGTH.
Returns a new buffer containing the deleted bytes."
  (prog1 (buffer-slice buffer start length)
    (buffer-delete-section buffer start length)))

(defmethod buffer-read-uint32 ((buffer buffer))
  "Modifies BUFFER by removing 4 bytes from the front, and returning them as a 32-bit integer.
The bytes are assumed to be in network order in the buffer."
  (unpack "N" (buffer-data (buffer-read buffer 4))))

(defmethod buffer-read-uint16 ((buffer buffer))
  "Modifies BUFFER by removing 2 bytes from the front, and returning them as a 16-bit integer.
The bytes are assumed to be in network order in the buffer."
  (unpack "n" (buffer-data (buffer-read buffer 2))))

(defmethod buffer-mismatch ((buffer1 buffer) (buffer2 buffer)
			    &key (start1 0) end1 (start2 0) end2 from-end)
  "Returns NIL if the buffers match, or the position number where they differ (like CL:MISMATCH)."
  (mismatch (buffer-data buffer1) (buffer-data buffer2)
	    :start1 start1 :end1 end1 :start2 start2 :end2 end2 :from-end from-end))

(defmethod buffer-string ((buffer buffer))
  "Returns a string representation of BUFFER, decoding UTF-8. Does not modify BUFFER.
May signal BABEL-ENCODINGS:CHARACTER-DECODING-ERROR in the event of a decoding error."
  (babel:octets-to-string (buffer-data buffer)))

(defmethod buffer-ascii ((buffer buffer))
  "Returns a string representation of BUFFER, treating it as ASCII only. Does not modify BUFFER."
  (map 'string #'code-char (buffer-data buffer)))

(defmethod buffer-inspect ((buffer buffer) &key (max-length 512))
  "Calls VECTOR-INSPECT on the contents of BUFFER."
  (vector-inspect (buffer-data buffer) :max-length max-length))

(defmethod print-object ((buffer buffer) stream)
  (print-unreadable-object (buffer stream :type t :identity t)
    (format stream ":LENGTH ~D" (buffer-size buffer))))
