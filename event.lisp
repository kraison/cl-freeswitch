(in-package #:cl-freeswitch)

(define-condition event-handler-error (error)
  ((reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (reason) error
               (format stream "Event handler error: ~A" reason)))))


(defun log-recognizer (ok? input)
  ;(add-history (list :input input))
  (when (or (eql ok? :ok) (eql ok? :sleep-ok) (eql ok? :bridge-ok)
            (eql ok? :user-failure) (eql ok? :dtmf) (eql ok? :hangup)
            (eql ok? :not-ok))
    (logger :debug "Recognizer got ~A on ~A." ok? (recognizer *session*))))
;    (logger :debug "Recognizer got ~A on ~A for ~{~A~^, ~}"
;	    ok?
;	    (recognizer *session*)
;	    (mapcar #'(lambda (kv) (format nil "(~A: ~A)"
;					   (car kv) (cdr kv))) input))))

(defmethod handle-incoming-event (raw-input)
  ;;(logger :debug "handle-incoming-event for session ~A" *session*)
  (let ((done? nil))
    (handler-case
	(if (recognizer *session*)
	    (with-recursive-lock-held ((lock *session*))
	      (dolist (parsed-input (fs-parse-new raw-input))
		(let ((ok? (fs-command-ok? (recognizer *session*) (uuid *session*)
                                           parsed-input)))
		  (log-recognizer ok? parsed-input)
		  (case ok?
		    (:ok             (funcall (continuation *session*) parsed-input :ok))
		    (:sleep-ok       (funcall (continuation *session*) parsed-input :ok))
		    (:bridge-ok      (funcall (continuation *session*) parsed-input :ok))
		    (:user-failure   (funcall (continuation *session*) parsed-input :user-failure))
		    (:hangup
		     (progn
		       (dolist (input parsed-input)
			 (logger :debug "HANGUP: ~A" input))
		       (return-from handle-incoming-event :hangup)))
		    (:abandon        (return-from handle-incoming-event :abandon))
		    (:not-ok         (funcall (continuation *session*) parsed-input :not-ok))
		    (:dtmf           (if (functionp (dtmf-handler *session*))
					 (funcall (dtmf-handler *session*) parsed-input :dtmf)
					 nil))
		    (:ignore         nil)
		    (:unknown        nil)
		    (otherwise       (progn
				       (logger :warning
					       "Recognizer returned strange values for ~A:"
					       *session*)
				       (dolist (input parsed-input)
					 (logger :debug "~A" input))))))))
	    (error 'event-handler-error :reason
		   (format nil "No recognizer set for session ~A" *session*)))
      (error (c)
	(logger :debug "RAW-INPUT: ~A" raw-input)
	(logger :err "Unhandled error in handle-incoming-event for ~A: ~A" *session* c)
	:hangup))))
