;; This software is Copyright (c) Chatsubo.net, LLC, May 1, 2011.
;; Chatsubo.net, LLC grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:cl-freeswitch)

;(defconstant +buflen+ 16)
(defconstant +max-query-length+ 10000000)

(define-condition listener-error (error)
  ((reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (reason)
                 error
               (format stream "Listener error: ~A." reason)))))

(define-condition originate-error (error)
  ((reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (reason)
                 error
               (format stream "Originate error: ~A." reason)))))

(defun ip-to-string (ip)
  (format nil "~A.~A.~A.~A" (aref ip 0) (aref ip 1) (aref ip 2) (aref ip 3)))

(defun add-thread (thread)
  (with-recursive-lock-held (*thread-list-lock*)
    (push thread *thread-list*)))

(defun remove-thread (thread)
  (with-recursive-lock-held (*thread-list-lock*)
    (setf *thread-list* (remove thread *thread-list*))))

(defmethod thread-sleep ((time number))
  (loop for x from 0 to (* 10 time) do
       (when *stop-listener* (return-from thread-sleep))
       (sleep 0.1)))

(defmethod shutdown-connection ((session session))
  "Close socket and remove its event handler."
  (logger :debug "terminating ~A" session)
  (ignore-errors (usocket:socket-close (sock session)))
  (remove-thread (thread session))
  (logger :debug "~A terminated" session)
  (setf session nil))

;; FIXME: see doc string
(defun data-received-handler (session func)
  "Reads all pending characters on a socket into the session buffer. Should be replaced with 
something capable of detecting overruns."
  (let ((request nil) (stream (fs-stream session)))
    (progn
      (do ((input (read-line stream) (read-line stream)))
	  ((or (null input) (null (listen stream))))
	(push input request)))
    (when (> (length request) 0) 
      (let ((*session* session))
	(funcall func (nreverse request))))))

(defun client-loop (recv-func)
  (handler-case
      (progn
	(loop until *stop-listener*
	   do
	   (let ((sock (usocket:wait-for-input (sock *session*) :ready-only t :timeout 1)))
	     (when sock
	       (let ((status (data-received-handler *session* recv-func)))
		 (cond ((eql status :hangup)
			(logger :debug "Got hangup for ~A. Ending session." *session*)
			(shutdown-session *session*)
			(shutdown-connection *session*)
			(sb-ext:quit))
		       ((eql status :abandon)
			(logger :debug "Got abandon-call for ~A. Ending session." *session*)
			(shutdown-session *session* :hangup? nil)
			(shutdown-connection *session*)
			(sb-ext:quit)))))))
	(shutdown-session *session*)
	(shutdown-connection *session*)
	(logger :debug "Session ended: ~A" *session*))
    (end-of-file (c)
      (declare (ignore c))
      (logger :err "Client closed connection. Killing ~A" *session*)
      (shutdown-session *session* :hangup? nil)
      (shutdown-connection *session*))
    (freeswitch-client-error (c)
      (logger :err "CLIENT-LOOP GOT FS CLIENT ERROR: ~A" c)
      (shutdown-session *session* :hangup? nil)
      (shutdown-connection *session*))
    (error (c)
      (logger :err "~A got unhandled error: ~A. Killing session." *session* c)
      (shutdown-session *session*)
      (shutdown-connection *session*))))

(defun accept-handler (socket recv-func)
  (make-thread
   #'(lambda ()
       (let (*session*)
	 (logger :debug "IN ACCEPT-HANDLER FOR ~A" socket)
	 (handler-case
	     (let ((stream (usocket:socket-stream socket)))    
	       (setf *session* (fs-setup-call stream socket))
	       (force-output (fs-stream *session*))
	       (setf (thread *session*) (current-thread))
	       (initiate-session *session*))
	   (end-of-file (c)
	     (declare (ignore c))
	     (logger :err "Client closed connection. Killing ~A" *session*)
	     (when (session? *session*)
	       (shutdown-session *session* :hangup? nil)
	       (shutdown-connection *session*)))
	   (error (c)
	     (logger :err "~A got unhandled error: ~A. Killing session." *session* c)
	     (when (session? *session*)
	       (shutdown-session *session*)
	       (shutdown-connection *session*))))
	 (if (session? *session*)
	     (client-loop recv-func)
	     (progn
	       (logger :err "Unable to initiate session.")
	       (remove-thread (current-thread))))))
   :name (format nil "~A handler" socket)))

(defun start-listener (port &key (address usocket:*wildcard-host*) 
		       (recv-func #'handle-incoming-event))
  (logger :info "Starting tcp listener on port ~A" port)
  (setf *stop-listener* nil)
  (usocket:with-server-socket (listener (usocket:socket-listen address port :reuse-address t))
    (loop until *stop-listener* 
       do
       (handler-case
	   (when (usocket:wait-for-input listener :ready-only t :timeout 1)
	     (let ((client-connection (usocket:socket-accept listener)))
	       (handler-case
		   (let ((thread (accept-handler client-connection recv-func)))
		     (add-thread thread))
		 (usocket:connection-aborted-error ())
		 (usocket:socket-error (c)
		   (logger :err "Listener got error on ~A: ~A" listener c)))))
	 (error (c)
	   (logger :err "UNHANDLED ERROR OF TYPE ~A IN LISTENER: ~A" (type-of c) c)))))
  (logger :info "Shutting down tcp listener on port ~A" port))

(defun stop-listener ()
  (setf *stop-listener* t))

(defun originate (destination &key continuation session-vars (timeout 60) fs-host
		  (recv-func #'handle-incoming-event) caller-id)
  (unless fs-host (setf fs-host *fs-host*))
  (let ((thread 
	 (make-thread
	  #'(lambda ()
	      (let (*session*)
		(handler-case
		    (let* ((socket (usocket:socket-connect fs-host *fs-port*))
			   (stream (usocket:socket-stream socket)))
		      (logger :debug "IN ORIGINATE FOR ~A / ~A" socket destination)
		      (setf *session* (fs-setup-outgoing-call stream socket destination 
							      :timeout timeout
							      :caller-id caller-id))
		      (when (session? *session*)
			(setf (thread *session*) (current-thread))
			(dolist (kv session-vars)
			  (set-session-var (car kv) (cdr kv)))
			(when (and continuation (or (functionp continuation) (fboundp continuation)))
			  (funcall continuation nil :ok))))
		  (freeswitch-client-error (c)
		    (logger :err "ORIGINATE GOT FS CLIENT ERROR: ~A" c)
		    (when (session? *session*)
		      (shutdown-session *session* :hangup? nil)
		      (shutdown-connection *session*)))
		  (end-of-file (c)
		    (declare (ignore c))
		    (logger :err "Client closed connection. Killing ~A" *session*)
		    (when (session? *session*)
		      (shutdown-session *session* :hangup? nil)
		      (shutdown-connection *session*)))
		  (error (c)
		    (logger :err "~A got unhandled error: ~A. Killing session." *session* c)
		    (when (session? *session*)
		      (shutdown-session *session*)
		      (shutdown-connection *session*))))
		(if (session? *session*)
		    (client-loop recv-func)
		    (progn
		      (logger :err "Unable to originate call to ~A." destination)
		      (remove-thread (current-thread))))))
	  :name (format nil "originate-handler ~A" destination))))
    (add-thread thread)))
