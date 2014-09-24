;; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol)

;; Implementation of header compression for HTTP/2 (HPACK) format adapted
;; to efficiently represent HTTP headers in the context of HTTP/2.
;;
;; - http://tools.ietf.org/html/draft-ietf-httpbis-header-compression

(defparameter *static-table*
  '((":authority"                  . "")
    (":method"                     . "GET")
    (":method"                     . "POST")
    (":path"                       . "/")
    (":path"                       . "/index.html")
    (":scheme"                     . "http")
    (":scheme"                     . "https")
    (":status"                     . "200")
    (":status"                     . "204")
    (":status"                     . "206")
    (":status"                     . "304")
    (":status"                     . "400")
    (":status"                     . "404")
    (":status"                     . "500")
    ("accept-charset"              . "")
    ("accept-encoding"             . "gzip, deflate")
    ("accept-language"             . "")
    ("accept-ranges"               . "")
    ("accept"                      . "")
    ("access-control-allow-origin" . "")
    ("age"                         . "")
    ("allow"                       . "")
    ("authorization"               . "")
    ("cache-control"               . "")
    ("content-disposition"         . "")
    ("content-encoding"            . "")
    ("content-language"            . "")
    ("content-length"              . "")
    ("content-location"            . "")
    ("content-range"               . "")
    ("content-type"                . "")
    ("cookie"                      . "")
    ("date"                        . "")
    ("etag"                        . "")
    ("expect"                      . "")
    ("expires"                     . "")
    ("from"                        . "")
    ("host"                        . "")
    ("if-match"                    . "")
    ("if-modified-since"           . "")
    ("if-none-match"               . "")
    ("if-range"                    . "")
    ("if-unmodified-since"         . "")
    ("last-modified"               . "")
    ("link"                        . "")
    ("location"                    . "")
    ("max-forwards"                . "")
    ("proxy-authenticate"          . "")
    ("proxy-authorization"         . "")
    ("range"                       . "")
    ("referer"                     . "")
    ("refresh"                     . "")
    ("retry-after"                 . "")
    ("server"                      . "")
    ("set-cookie"                  . "")
    ("strict-transport-security"   . "")
    ("transfer-encoding"           . "")
    ("user-agent"                  . "")
    ("vary"                        . "")
    ("via"                         . "")
    ("www-authenticate"            . ""))
  "Default working set as defined by the spec.")

(defparameter *static-table-length* (length *static-table*)
  "Must equal the length of *STATIC-TABLE*")
;; *static-table* and (table encoding-context) should probably be
;; *vectors instead, then keeping this length would not be necessary

(defparameter *extra-size-per-table-entry* 32
  "The size of an entry is the sum of its name's length in octets,
its value's length in octets, plus 32.")

(defclass encoding-context (error-include)
  ((type :initarg :type)
   (table :reader table :initform nil
	  :documentation "Running set of headers used as a compression dictionary, in addition to *STATIC-TABLE*.")
   (settings-limit :accessor settings-limit :initarg :settings-limit :initform 4096)
   (limit :accessor limit :initarg :limit :initform 4096))
  (:documentation "Encoding context: a header table and related state for one direction"))

(defmethod initialize-instance :after ((encoding-context encoding-context) &key)
  (with-slots (settings-limit limit) encoding-context
    (unless limit
      (setf limit settings-limit))))

(defmethod print-object ((encoding-context encoding-context) stream)
  (with-slots (type) encoding-context
    (print-unreadable-object (encoding-context stream :type t :identity t)
      (format stream ":TYPE ~S" type))))

(defmethod process ((encoding-context encoding-context) cmd)
  "Performs differential coding based on provided command type"
  (with-slots (table settings-limit limit) encoding-context
    (let (emit evicted)
      ;; helper macro to extract and check 1-based index to 0-based:
      (macrolet ((with-lowered-one-based-idx ((var place zero-error-msg) &body body)
		   (let ((idx1 (gensym)))
		     `(let ((,idx1 ,place))
			(declare ((integer 0 *) ,idx1))
			(if (plusp ,idx1)
			    (let ((,var (1- ,idx1)))
			      ,@body)
			    (raise 'http2-compression-error ,(format nil "Zero used in ~A." zero-error-msg)))))))
	;; helper commands for table management:
	(flet ((lookup-entry (idx)
		 (declare ((integer 0 *) idx))
		 (if (< idx *static-table-length*)
		     (elt *static-table* idx)
		     (let ((dynamic-idx (- idx *static-table-length*)))
		       (if (< dynamic-idx (length table))
			   (elt table dynamic-idx)
			   (raise 'http2-compression-error "Index ~D too high (~D static, ~D dynamic)."
				  idx *static-table-length* (length table))))))
	       (maybe-add-to-table-and-do-evictions (entry)
		 (declare (cons entry))
		 (multiple-value-bind (ok-to-add this-evicted)
		     (size-check-evictions encoding-context (list :name (car entry) :value (cdr entry)))
		   (when ok-to-add
		     (push entry table))
		   (when this-evicted
		     (setf evicted this-evicted)))))
	  ;; switch on command type and run the appropriate logic:
	  (ecase (getf cmd :type)
	    (:context
	     (ecase (getf cmd :context-type)
	       (:new-max-size
		(when (> (getf cmd :value) settings-limit)
		  (raise 'http2-compression-error "Attempt to set table limit above SETTINGS_HEADER_TABLE_SIZE."))
		(setf limit (getf cmd :value))
		(size-check-evictions encoding-context nil))))
	    (:indexed
	     (with-lowered-one-based-idx (idx (getf cmd :name) "indexed representation")
	       (setf emit (lookup-entry idx))))
	    ((:incremental :noindex :neverindex)
	     (let ((cmd (copy-tree cmd)))
	       (when (integerp (getf cmd :name))
		 (ensuref (getf cmd :index) (getf cmd :name))
		 (with-lowered-one-based-idx (idx (getf cmd :index) "literal incremental representation")
		   (let ((entry (lookup-entry idx)))
		     (setf (getf cmd :name) (car entry))
		     (ensuref (getf cmd :value) (cdr entry)))))
	       (setf emit (cons (getf cmd :name) (getf cmd :value)))
	       (when (eq (getf cmd :type) :incremental)
		 (maybe-add-to-table-and-do-evictions emit)))))))
      (values emit evicted))))

(defmethod add-cmd ((encoding-context encoding-context) header)
  "Emits best available command to encode provided header."
  (with-slots (table) encoding-context
    ;; check if we have an exact match in header table
    (when-let (idx (or (position header *static-table* :test #'equal)
		       (awhen (position header table :test #'equal)
			  (+ it *static-table-length*))))
      (return-from add-cmd (list :name (1+ idx) :type :indexed)))

    ;; check if we have a partial match on header name
    (when-let (idx (or (position (car header) *static-table* :key #'car :test #'equal)
		       (awhen (position (car header) table :key #'car :test #'equal)
			 (+ it *static-table-length*))))
      ;; default to incremental indexing
      ;; TODO: implement literal without indexing strategy
      (return-from add-cmd (list :name (1+ idx) :value (cdr header) :type :incremental)))

    (list :name (car header) :value (cdr header) :type :incremental)))

(defmethod header-size ((encoding-context encoding-context) header)
  (if (null header)
      0
      (+ (length (car header)) (length (cdr header)) *extra-size-per-table-entry*)))

(defmethod headers-size ((encoding-context encoding-context) headers)
  (loop for header in headers sum (header-size encoding-context header)))

(defmethod size-check-evictions ((encoding-context encoding-context) cmd)
  "Before doing such a modification, it has to be ensured that the header
table size will stay lower than or equal to the
SETTINGS_HEADER_TABLE_SIZE limit. To achieve this, repeatedly, the
first entry of the header table is removed, until enough space is
available for the modification."
  (with-slots (table limit) encoding-context
    (let ((cursize (headers-size encoding-context table))
	  (cmdsize (header-size encoding-context (cons (getf cmd :name) (getf cmd :value))))
	  ok-to-add-p
	  evicted-table-entries)
      (if (> cmdsize limit)
	  ;; The addition of a new entry with a size greater than the
	  ;; SETTINGS_HEADER_TABLE_SIZE limit causes all the entries from the
	  ;; header table to be dropped and the new entry not to be added to the
	  ;; header table.
	  (setf ok-to-add-p nil
		evicted-table-entries table
		table nil)
	  ;; could fit, evict one or more entries from end of table
	  (progn
	    (setf ok-to-add-p t)
	    (while (> (+ cursize cmdsize) limit)
	      (let ((e (shift table)))
		(decf cursize (header-size encoding-context e))
		(push e evicted-table-entries)))))
      (values ok-to-add-p evicted-table-entries))))

(defparameter *headrep*
  '(:indexed      (:prefix 7 :pattern #x80)
    :noindex      (:prefix 4 :pattern #x00)
    :incremental  (:prefix 6 :pattern #x40)
    :neverindex   (:prefix 4 :pattern #x10)
    :context      (:prefix 5 :pattern #x20))
  "Header representation as defined by the spec.")

(defparameter *contextrep*
  '(:new-max-size (:prefix 4 :pattern #x00))
  "Context update representation as defined by the spec.")

(defclass compressor ()
  ((cc-type :initarg :type :initform :request :type (member :request :response))
   (cc))
  (:documentation "Responsible for encoding header key-value pairs using HPACK algorithm.
Compressor must be initialized with appropriate starting context based
on local role: client (:TYPE :REQUEST) or server (:TYPE :RESPONSE)."))

(defmethod initialize-instance :after ((compressor compressor) &key)
  (with-slots (cc cc-type) compressor
    (setf cc (make-instance 'encoding-context :type cc-type))))

(defmethod @integer ((compressor compressor) i n)
  "Encodes provided value via integer representation.

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
      (return-from @integer (pack "B" i :array (make-array 128 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))))
    
    (let ((bytes (make-array 128 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
      (when (not (zerop n))
	(vector-push-extend limit bytes))

      (decf i limit)
      (while (>= i 128)
	(vector-push-extend (+ (mod i 128) 128) bytes)
	(setf i (ash i -7)))
      
      (vector-push-extend i bytes)
      bytes)))

(defmethod @string ((compressor compressor) str)
  "Encodes provided value via string literal representation.

 * The string length, defined as the number of bytes needed to store
   its UTF-8 representation, is represented as an integer with a zero
   bits prefix. If the string length is strictly less than 128, it is
   represented as one byte.
 * The string value represented as a list of UTF-8 characters"
  (let ((bytes (@integer compressor (length str) 7)))
    (loop for char across str do (vector-push-extend (char-code char) bytes))
    bytes))

(defmethod header ((compressor compressor) h &optional (buffer (make-instance 'buffer)))
  (macrolet ((<<integer (i n) `(buffer<< buffer (@integer compressor ,i ,n)))
	     (<<string (s) `(buffer<< buffer (@string compressor ,s)))
	     (+pattern (b p) (with-gensyms (b*) `(let ((,b* ,b)) (setf (aref ,b* 0) (logior (aref ,b* 0) ,p)) ,b*)))
	     (<<integer+ (i n p) `(buffer<< buffer (+pattern (@integer compressor ,i ,n) ,p))))

    (let ((rep (getf *headrep* (getf h :type))))
      (macrolet ((firstinteger (&rest cmd) `(<<integer+ ,@(cdar cmd) (getf rep :pattern))))

	(if (eq (getf h :type) :context)
	    (let ((crep (getf *contextrep* (getf h :context-type))))
	      (firstinteger (+pattern (<<integer (or (getf h :value) 0) (getf crep :prefix)) (getf crep :pattern))))

	    (if (eq (getf h :type) :indexed)
		(firstinteger (<<integer (getf h :name) (getf rep :prefix)))

		(progn
		  (if (integerp (getf h :name))
		      (firstinteger (<<integer (getf h :name) (getf rep :prefix)))
		      (progn
			(firstinteger (<<integer 0 (getf rep :prefix)))
			(<<string (getf h :name))))
	    
		  (if (integerp (getf h :value))
		      (<<integer (getf h :value) 0)
		      (<<string (getf h :value)))))))))
  buffer)

(defmethod preprocess-headers (headers)
  "Performs three corrections to header sets:
1. Collects psuedo-headers (those that begin with a colon like :method) to the front.
2. Splits cookie headers by colon, space, null in order to make each one separate for compression.
In the event that headers is not in need of changes, it is passed back and is EQ to the one passed in;
otherwise, a fresh list is passed back, although header entries may share structure with the original."
  (labels ((is-pseudo-header-key (key)
	     (char= #\: (char key 0)))
	   (sort-pseudo-headers-to-front (headers)
	     (loop
		for header in headers
		if (is-pseudo-header-key (car header))
		collect header into pseudo-headers
		else
		collect header into other-headers
		finally (return (nconc pseudo-headers other-headers))))
	   (split-cookies (headers)
	     (loop
		for header in headers
		for (key . value) = header
		if (string= key "cookie")
		append (mapcar (lambda (v) (cons key v))
			       (split-if (lambda (c) (member c '(#\; #\Space #\Null) :test #'char=)) value)) into cookies
		else
		collect header into new-headers
		finally (return (nconc new-headers cookies)))))
    (loop
       with in-pseudo-headers = t
       for (key . value) in headers
       for i from 0
       when (zerop need-sort)
       count (and (or (is-pseudo-header-key key) (setf in-pseudo-headers nil))
		  (not in-pseudo-headers))
       into need-sort
       when (zerop has-multi-cookie)
       count (and (string= key "cookie")
		  (find-if (lambda (c) (member c '(#\; #\Space #\Null) :test #'char=)) value))
       into has-multi-cookie
       finally (return (progn
			 (when (plusp need-sort)
			   (setf headers (sort-pseudo-headers-to-front headers)))
			 (when (plusp has-multi-cookie)
			   (setf headers (split-cookies headers)))
			 headers)))))

(defmethod preprocess ((compressor compressor) headers)
  (preprocess-headers headers))

(defmethod encode ((compressor compressor) headers)
  "Encodes provided list of HTTP headers."
  (with-slots (cc) compressor
    (loop
       with buffer = (make-instance 'buffer)
       for header in headers
       for cmd = (add-cmd cc header)
       for (emit this-evicted) = (multiple-value-list (process cc cmd))
       append this-evicted into evicted
       do (buffer<< buffer (header compressor cmd))
       finally (return (values buffer evicted)))))

(defclass decompressor ()
  ((cc-type :initarg :type :initform :response :type (member :request :response))
   (cc))
  (:documentation "Responsible for decoding header key-value pairs using HPACK algorithm.
Decompressor must be initialized with appropriate starting context based
on local role: client (:TYPE :REQUEST) or server (:TYPE :RESPONSE)."))

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
  (let* ((peek (buffer-getbyte buf nil))
	 (huffman-p (logbitp 7 peek))
	 (length (@integer decompressor buf 7))
	 (bytes (buffer-read buf length)))
    (if huffman-p
	(huffman-decode-buffer-to-string bytes length)
	(handler-case
	    (buffer-string bytes)
	  (babel-encodings:character-decoding-error ()
	    (when *debug-mode*
	      (warn "UTF-8 decoding failed: ~S" (buffer-data bytes)))
	    (buffer-ascii bytes))))))

(defmethod header ((decompressor decompressor) buf &optional header)
  "Decodes header command from provided buffer."
  (let ((peek (buffer-getbyte buf nil)))

    (let (type regular-p)
      (loop
	 for (tt desc) on *headrep* by #'cddr
	 for prefix = (getf desc :prefix)
	 for mask = (ash (ash peek (- prefix)) prefix)
	 if (= mask (getf desc :pattern))
	 do (progn
	      (setf (getf header :type) tt
		    regular-p (not (eq tt :context))
		    type desc)
	      (return)))

      (if regular-p
	  (progn
	    (setf (getf header :name) (@integer decompressor buf (getf type :prefix)))
	    (when (not (eq (getf header :type) :indexed))
	      (when (zerop (getf header :name))
		(setf (getf header :name) (@string decompressor buf)))
	      (setf (getf header :value) (@string decompressor buf))))

	  ;; else context update (:reset/:new-max-size):
	  (let ((peek-short (logand peek (1- (expt 2 (getf type :prefix)))))
		ctype)
	    (setf (getf header :type) :context)
	    (loop
	       for (ct cdesc) on *contextrep* by #'cddr
	       for prefix = (getf cdesc :prefix)
	       for mask = (ash (ash peek-short (- prefix)) prefix)
	       if (= mask (getf cdesc :pattern))
	       do (progn
		    (setf (getf header :context-type) ct
			  ctype cdesc)
		    (return)))
	    (setf (getf header :value) (@integer decompressor buf (getf ctype :prefix)))))

      header)))

(defmethod decode ((decompressor decompressor) buf)
  "Decodes and processes header commands within provided buffer."
  (with-slots (cc) decompressor
    (loop
       until (buffer-empty-p buf)
       for (emit this-evicted) = (multiple-value-list (process cc (header decompressor buf)))
       collect emit into headers
       append this-evicted into evicted
       finally (return (values headers evicted)))))

(defun postprocess-headers (headers)
  "Join cookies together with a colon that are in separate headers or have null-separators.
In the event that headers is not in need of changes, it is passed back and is EQ to the one passed in;
otherwise, a fresh list is passed back, although header entries may share structure with the original."
  (if (let ((c 0))
	(find-if (lambda (h) (or (if (string= (car h) "cookie") (>= (incf c) 2))
				 (find #\Null (cdr h) :test #'char=))) headers))
      (loop
	 for header in headers
	 for (key . value) = header
	 if (string= key "cookie")
	 append (split-if (lambda (c) (char= c #\Null)) value) into cookie-values
	 else
	 collect header into new-headers
	 finally (return (nconc new-headers (list (cons "cookie" (format nil "~{~A~^; ~}" cookie-values))))))
      headers))

(defmethod postprocess ((decompressor decompressor) headers)
  (postprocess-headers headers))
