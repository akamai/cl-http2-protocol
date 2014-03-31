; Copyright (c) 2014 Akamai Technologies, Inc. (MIT License)

(in-package :cl-http2-protocol)

(defclass emitter-include ()
  ((listeners :accessor listeners :initarg :listeners :initform (make-hash-table)))
  (:documentation "Basic event emitter with support for persistent and one-time event callbacks."))

(defmethod add-listener ((emitter emitter-include) event block)
  "Subscribe to all future events for specified type."
  (assert (and block (functionp block)) (block) "Must provide callback")
  (push block (gethash (to-sym event) (listeners emitter) nil)))

(defalias on add-listener)

(defmethod once ((emitter emitter-include) event block)
  "Subscribe to next event (at most once) for specified type."
  (add-listener emitter event
		(lambda (&rest args) (apply block args) :delete)))

(defmethod emit ((emitter emitter-include) event &rest args)
  "Emit event with provided arguments."
  (deletef-if (gethash event (listeners emitter))
	      (lambda (cb) (eq (apply cb args) :delete))))

; Port note: no listeners function because we rolled the gethash into
; add-listener/emit, which is necessary because we're using things
; like push which need a place
