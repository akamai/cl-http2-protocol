(in-package :http2)

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0) (compilation-speed 0)))

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

; Port note: no listeners function because we rolled the gethash into
; add-listener/emit, which is necessary because we're using things
; like push which need a place
