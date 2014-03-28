(in-package :http2)

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
    ("via"                 . ""))
  "Default request working set as defined by the spec.")

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
  "Performs differential coding based on provided command type.
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
		(vector-delete-at refset cur)
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
