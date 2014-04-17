; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol)

; Implementation of header compression for HTTP 2.0 (HPACK) format adapted
; to efficiently represent HTTP headers in the context of HTTP 2.0.
;
; - http://tools.ietf.org/html/draft-ietf-httpbis-header-compression

(defparameter *static-table*
  '((":authority"                  . "")
    (":method"                     . "GET")
    (":method"                     . "POST")
    (":path"                       . "/")
    (":path"                       . "/index.html")
    (":scheme"                     . "http")
    (":scheme"                     . "https")
    (":status"                     . "200")
    (":status"                     . "500")
    (":status"                     . "404")
    (":status"                     . "403")
    (":status"                     . "400")
    (":status"                     . "401")
    ("accept-charset"              . "")
    ("accept-encoding"             . "")
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

; The set of components used to encode or decode a header set form an
; encoding context: an encoding context contains a header table and a
; reference set - there is one encoding context for each direction.

(defclass encoding-context (error-include)
  ((type :initarg :type)
   (table :reader table :initform nil)
   (limit :accessor table-limit :initarg :limit :initform 4096)
   (refset :reader refset :initform (make-array 128 :element-type t :adjustable t :fill-pointer 0)))
  (:documentation "Encoding context: a header table and reference set for one direction"))

(defmethod process ((encoding-context encoding-context) cmd)
  "Performs differential coding based on provided command type.
- http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-03#section-3.2"
  (with-slots (refset table) encoding-context
    (let (emit)

      ; indexed representation
      (if (eq (getf cmd :type) :indexed)
	  ; An indexed representation with an index value of 0 entails the
	  ; following actions:
	  ; - The reference set is emptied.
	  ;
	  ; An indexed representation corresponding to an entry not present
	  ; in the reference set entails the following actions:
	  ;
	  ; - If referencing an element of the static table:
	  ;   - The header field corresponding to the referenced entry is
	  ;     emitted.
	  ;
	  ;   - The referenced static entry is inserted at the beginning of the
	  ;     header table.
	  ;
	  ;   - A reference to this new header table entry is added to the
	  ;     reference set (except if this new entry didn't fit in the
	  ;     header table).
	  ;
	  ; - If referencing an element of the header table:
	  ;   - The header field corresponding to the referenced entry is
	  ;     emitted.
	  ;
	  ; - The referenced header table entry is added to the reference
	  ;   set.
	  ;
	  ; An indexed representation corresponding to an entry not present
	  ; in the reference set entails the following actions:
	  ; - The header corresponding to the entry is emitted.
	  ; - The entry is added to the reference set.
	  ;
	  ; An indexed representation corresponding to an entry present in
	  ; the reference set entails the following actions:
	  ;  - The entry is removed from the reference set.
	  ; 
	  (let ((idx (1- (getf cmd :name))))
	    (if (= idx -1)
		(setf (fill-pointer refset) 0)

		(let ((cur (position idx refset :key #'car)))
		  (if cur
		      (vector-delete-at refset cur)
		      (if (>= idx (length table))
			  (progn
			    (setf emit (elt *static-table* (- idx (length table))))
			    (when (size-check encoding-context (list :name (car emit) :value (cdr emit)))
			      (push emit table)
			      (loop for r across refset do (incf (car r)))
			      (vector-push-extend (cons 0 emit) refset)))
			  (progn
			    (setf emit (elt table idx))
			    (vector-push-extend (cons idx emit) refset)))))))

	  ; A literal representation that is not added to the header table
	  ; entails the following action:
	  ;  - The header is emitted.
	  ;
	  ; A literal representation that is added to the header table entails
	  ; the following actions:
	  ;  - The header is emitted.
	  ;  - The header is inserted at the beginning of the header table.
	  ;  - A reference to the new entry is added to the reference set
	  ;    (except if this new entry didn't fit in the header table).
	  ;
	  (progn
	    (when (eq (getf cmd :type) :incremental)
	      (when (integerp (getf cmd :name))
		(setf (getf cmd :index) (1- (getf cmd :name)))
		(let ((replacement (car (if (>= (getf cmd :index) (length table))
					    (elt *static-table* (- (getf cmd :index) (length table)))
					    (elt table (getf cmd :index))))))
		  (setf (getf cmd :name) replacement))))

	    (setf emit (cons (getf cmd :name) (getf cmd :value)))
	      
	    (when (eq (getf cmd :type) :incremental)
	      (when (size-check encoding-context (list :name (car emit) :value (cdr emit)))
		(push emit table)
		(loop for r across refset do (incf (car r)))
		(vector-push-extend (cons (getf cmd :index) emit) refset)))))
      emit)))

(defmethod add-cmd ((encoding-context encoding-context) header)
  "Emits best available command to encode provided header."
  (with-slots (table) encoding-context
    ; check if we have an exact match in header table
    (when-let (idx (or (position header table :test #'equal)
		       (awhen (position header *static-table* :test #'equal)
			 (+ it (length table)))))
      (when (not (activep encoding-context idx))
	(return-from add-cmd (list :name (1+ idx) :type :indexed))))

    ; check if we have a partial match on header name
    (when-let (idx (or (position (car header) table :key #'car :test #'equal)
		       (awhen (position (car header) *static-table* :key #'car :test #'equal)
			 (+ it (length table)))))
      ; default to incremental indexing
      ; TODO: implement literal without indexing strategy
      (let ((cmd (list :name (1+ idx) :value (cdr header) :type :incremental)))
	(return-from add-cmd cmd)))

    (list :name (car header) :value (cdr header) :type :incremental)))

(defmethod remove-cmd ((encoding-context encoding-context) idx)
  "Emits command to remove current index from working set."
  (list :name idx :type :indexed))

(defmethod size-check ((encoding-context encoding-context) cmd)
  "Before doing such a modification, it has to be ensured that the header
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
      ; header table.
      (when (> cmdsize limit)
	(setf table nil)
	(return-from size-check nil))

      (while (> (+ cursize cmdsize) limit)
	(let ((e (shift table)))
	  (decf cursize (+ (length (car e)) (length (cdr e)) 32))))

      t)))

(defmethod activep ((encoding-context encoding-context) idx)
  (with-slots (refset) encoding-context
    (not (null (find idx refset :key #'car :test #'equal)))))

(defparameter *headrep*
  '(:indexed      (:prefix 7 :pattern #x80)
    :noindex      (:prefix 6 :pattern #x40)
    :incremental  (:prefix 6 :pattern #x00))
  "Header representation as defined by the spec.")

; Responsible for encoding header key-value pairs using HPACK algorithm.
; Compressor must be initialized with appropriate starting context based
; on local role: client or server.
(defclass compressor ()
  ((cc-type :initarg :type)
   (cc)))

(defmethod initialize-instance :after ((compressor compressor) &key)
  (with-slots (cc cc-type) compressor
    (setf cc (make-instance 'encoding-context :type cc-type))))

(defmethod @integer ((compressor compressor) i n)
  "Encodes provided value via integer representation.
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
    
    (let ((bytes (make-data-vector 0)))
      (when (not (zerop n))
	(vector-push-extend limit bytes))

      (decf i limit)
      (while (>= i 128)
	(vector-push-extend (+ (mod i 128) 128) bytes)
	(setf i (/ i 128)))
      
      (vector-push-extend i bytes)
      bytes)))

(defmethod @string ((compressor compressor) str)
  "Encodes provided value via string literal representation.
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
	  (<<integer (getf h :name) (getf rep :prefix))

	  (progn
	    (if (integerp (getf h :name))
		(<<integer (1+ (getf h :name)) (getf rep :prefix))
		(progn
		  (<<integer 0 (getf rep :prefix))
		  (<<string (getf h :name))))
	    
	    (if (integerp (getf h :value))
		(<<integer (getf h :value) 0)
		(<<string (getf h :value)))))

      ; set header representation pattern on first byte
      (let ((fb (logior (buffer-firstbyte buffer) (getf rep :pattern))))
	(buffer-setbyte buffer 0 fb))))

  buffer)

(defmethod split-cookies ((compressor compressor) headers)
  (if (find "cookie" headers :key #'car :test #'string=)
      (loop
	 with new-headers = nil
	 for header in headers
	 for (k . v) = header
	 if (string= k "cookie")
	 do (dolist (v* (split-if (lambda (c) (or (char= c #\;) (char= c #\Space) (char= c #\Null))) v))
	      (push (cons k v*) new-headers))
	 else
	 do (push header new-headers)
	 finally (return (nreverse new-headers)))
      headers))

(defmethod combine ((compress compressor) headers)
  ; this code is longer than necessary because it optimizes speed and memory for no/few duplicates
  ; in the case of no duplicates, there is no cons'ing and headers is simply returned with one pass
  ; as duplicates are found some structures grow, and a second pass is necessary to cons up the structure
  ; individual header cons's will be reused in the new structure if they are not dup's
  (loop
     with dups = nil   ; each entry is a list: the original index integer, followed by all values
     with dupidx = nil ; each entry is an index integer
     with l = (length headers)
     for i below l
     for current-start on headers
     for (current-k . current-v) = (car current-start)
     if (and (not (find i dupidx :test #'=))
	     (not (string= current-k "set-cookie")))
     do (loop
	   for j from (1+ i) below l
	   for (k . v) in (cdr current-start)
	   when (string= k current-k)
	   collect j into js and
	   collect v into vs
	   finally (when js
		     (push (cons i (cons current-v vs)) dups)
		     (nconc dupidx (cons i js))))
     finally (return (if dups
			 (loop
			    for i below l
			    for dup = (find i dups :key #'car :test #'=)
			    for header in headers
			    if dup
			    collect (cons (car header) (format nil #.(format nil "~~{~~A~~^~C~~}" #\Null) (cdr dup)))
			    else
			    unless (find i dupidx :test #'=)
			    collect header)
			 headers))))

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

(defmethod encode-with-ordering ((compressor compressor) headers)
  (split-cookies compressor (combine compressor headers)))

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
  (let* ((peek (buffer-getbyte buf nil))
	 (huffman-p (logand peek 128))
	 (length (@integer decompressor buf 7))
	 (bytes (buffer-read buf length)))
    (if huffman-p
	(huffman-decode bytes length)
	(buffer-string bytes))))

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
	(when (zerop (getf header :name))
	  (setf (getf header :name) (@string decompressor buf)))

	(setf (getf header :value) (@string decompressor buf)))

      header)))

(defmethod decode ((decompressor decompressor) buf)
  "Decodes and processes header commands within provided buffer.

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
