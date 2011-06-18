;; This software is Copyright (c) Chatsubo.net, LLC, May 1, 2011.
;; Chatsubo.net, LLC grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:cl-freeswitch)

;;; Sessions

(define-condition session-error (error)
  ((reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (reason)
                 error
               (format stream "Session error: ~A." reason)))))

(defstruct (session 
	     (:print-function print-session) 
	     (:predicate session?)
	     (:conc-name nil))
  (uuid nil)
  (thread nil)
  (lock (make-recursive-lock (format nil "SESSION-LOCK-~A" (gensym))))
  (ipc-lock (make-recursive-lock (format nil "SESSION-IPC-LOCK-~A" (gensym))))
  (stream-lock (make-recursive-lock (format nil "SESSION-STREAM-LOCK-~A" (gensym))))
  (finished? nil)
  (fs-stream nil)
  (fs-host nil)
  (sock nil)
  (buffer (make-array +buflen+
		      :element-type 'character
		      :adjustable nil
		      :fill-pointer t))
  (recognizer nil)
  (continuation nil)
  (dtmf-handler nil)
  (end-call-handler nil)
  (error-handler #'(lambda (s)
		     (logger :err "DEFAULT ERROR HANDLER: ERROR IN SESSION ~A." s)))
  (symbol-table (make-hash-table :synchronized t))
  (caller-id nil)
  (destination nil)
  (db-connection nil)
  (raw-connect-info nil)
  (history nil))

(defun print-session (session stream depth)
  (declare (ignore depth))
  (format stream "#<SESSION ~A FROM ~A TO ~A>" 
	  (uuid session) (caller-id session) (destination session)))

(defun all-calls ()
  (handler-case
      (trivial-timeout:with-timeout (2)
	(sb-ext:with-locked-hash-table (*sessions*)
	  (maphash #'(lambda (id session)
		       (format t "~A~%  Continuation: ~A~%  recognizer: ~A~%" 
			       session (continuation session) (recognizer session)))
		   *sessions*)))
    (error (c)
      (declare (ignore c))
      (format t "Unable to get hash table lock for *sessions*~%"))))

(defun apply-sessions (func)
  (sb-ext:with-locked-hash-table (*sessions*)
    (maphash #'(lambda (id session)
		 (funcall func session))
	     *sessions*)))

(defun kill-all-sessions ()
  (apply-sessions #'shutdown-session))

(defmethod session-history ((session session))
  (reverse (history session)))

(defun add-history (entry)
  "Entries should be a list: '(:input blah blah) or '(:output blah blah)"
  (when (and (session? *session*) 
	     (or *session-history-recording* (get-session-var :record-session?)))
    (push (cons (get-universal-time) entry) (history *session*))))

(defmethod reset-history ((session session))
  (setf (history session) nil))

(defun set-continuation (continuation recognizer)
  (sb-ext:compare-and-swap (recognizer *session*) (recognizer *session*) recognizer)
  (sb-ext:compare-and-swap (continuation *session*) (continuation *session*) continuation))

(defun set-error-handler (func)
  (sb-ext:compare-and-swap (error-handler *session*) (error-handler *session*) func))

(defun unset-error-handler ()
  (sb-ext:compare-and-swap (error-handler *session*) (error-handler *session*) nil))

(defun set-end-call-handler (func)
  (sb-ext:compare-and-swap (end-call-handler *session*) (end-call-handler *session*) func))

(defun unset-end-call-handler ()
  (sb-ext:compare-and-swap (end-call-handler *session*) (end-call-handler *session*) nil))

(defun unset-dtmf-handler ()
  (sb-ext:compare-and-swap (dtmf-handler *session*) (dtmf-handler *session*) nil))

(defun get-session-vars (&rest keys)
  (values-list 
   (sb-ext:with-locked-hash-table ((symbol-table *session*))
     (mapcar #'(lambda (key) 
		 (gethash key (symbol-table *session*))) 
	     keys))))

(defun get-session-var (key)
  (gethash key (symbol-table *session*)))

(defun set-session-vars (alist)
  (sb-ext:with-locked-hash-table ((symbol-table *session*))
    (mapcar #'(lambda (i)
		(setf (gethash (car i) (symbol-table *session*)) (cdr i)))
	    alist)))

(defun set-session-var (key val)
  (setf (gethash key (symbol-table *session*)) val))

(defun lookup-session (uuid)
  (gethash uuid *sessions*))

(defmethod initiate-session ((session session))
  (let ((*session* session))
    (setf (thread session) (current-thread))
    (let ((func (gethash (destination *session*) *extension-table*)))
      (logger :debug "INITIATING SESSION ~A TO ~A" *session* func)
      (if (or (functionp func) (fboundp func))
	  (progn
	    (logger :debug "Sending ANSWER string")
	    (set-continuation func :answer)
	    (fs-command (fs-stream *session*) :answer nil :uuid (uuid *session*)))
	  (error 'session-error 
		 :reason (format nil "Unknown extension dialed: ~A" (destination *session*)))))))

(defmethod shutdown-session ((session session) &key (hangup? t))
  (with-recursive-lock-held ((lock session))
    (logger :debug "shutdown-session got session lock on ~A" session)
    (let ((*session* session))
      (logger :debug "Destroying session ~A" session)
      (when hangup?
	(ignore-errors (fs-command (fs-stream *session*) :hangup nil :uuid (uuid *session*))))
      (when (or (functionp (end-call-handler session)) (fboundp (end-call-handler session)))
	(logger :debug "shutdown-session waiting for session lock on ~A" session)
	(handler-case
	    ;;(with-recursive-lock-held ((lock *session*))
	    (progn
	      (logger :debug "SHUTDOWN-SESSION RUNNING END-CALL-HANDLER: ~A" (end-call-handler session))
	      (funcall (end-call-handler session) session)
	      (logger :debug "shutdown-session releasing session lock on ~A" session))
	  (error (c)
	    (logger :err "shutdown-session got error on ~A: ~A" session c))))
      (reset-history session)
      (remhash (uuid session) *sessions*)
      (logger :debug "shutdown-session done for ~A" session))))

(defun create-session (&key stream sock uuid caller-id destination fs-host raw-connect-info)
  (if (streamp stream)
      (let ((session (lookup-session uuid)))
	(if (session? session)
	    session
	    (progn
	      (setf session (make-session :uuid uuid
					  :fs-stream stream
					  :fs-host fs-host
					  :sock sock
					  :caller-id caller-id
					  :destination destination
					  :raw-connect-info raw-connect-info))
	      (setf (gethash uuid *sessions*) session))))
      (error 'session-error "make-session: STREAM must be streamp")))
