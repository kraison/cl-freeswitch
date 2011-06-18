;; This software is Copyright (c) Chatsubo.net, LLC, May 1, 2011.
;; Chatsubo.net, LLC grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:cl-freeswitch)

(define-condition configuration-error (error)
  ((reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (reason)
                 error
               (format stream "Configuration error: ~A." reason)))))

(defun get-option (c s o)
  (and (py-configparser:has-option-p c s o)
       (py-configparser:get-option c s o)))

(defun load-configuration (&key file)
  (setf *configuration-file* (or (and file (probe-file file))
				 (and (sb-posix:getenv "CL_FREESWITCH_CONFIG")
				      (probe-file (sb-posix:getenv "CL_FREESWITCH_CONFIG")))
                                 (probe-file "cl-freeswitch.cfg")
                                 (probe-file "/etc/cl-freeswitch.cfg")
                                 (probe-file "/usr/local/etc/cl-freeswitch.cfg")
                                 (probe-file *configuration-file*)))
  (when (not *configuration-file*)
    (error 'configuration-error :reason "Unable to load configuration file.  Cannot continue.~%"))
  (logger :info "Using configuration file ~A~%" *configuration-file*)
  (setf *configuration* (py-configparser:make-config))
  (py-configparser:read-files *configuration* (list *configuration-file*))
  (when (py-configparser:has-section-p *configuration* "default")
    (setq 
     *id* (or (get-option *configuration* "default" "id") *id*)
     *fs-host* (or (get-option *configuration* "default" "fs-host") *fs-host*)
     *fs-port* (let ((port (get-option *configuration* "default" "fs-port")))
		 (if (and port (cl-ppcre:scan "^[0-9]+$" port))
		     (parse-integer port)
		     *fs-port*))
     *fs-auth* (or (get-option *configuration* "default" "fs-auth") *fs-auth*)
     *pstn-gateway* (or (get-option *configuration* "default" "pstn-gateway") *pstn-gateway*)
     *port* (let ((port (get-option *configuration* "default" "port")))
	      (if (and port (cl-ppcre:scan "^[0-9]+$" port))
		  (parse-integer port)
		  *port*))
     *session-history-recording* (or (get-option *configuration* "default" "record-sessions") 
				     *session-history-recording*)
     *thread-pool-size* (let ((threads (get-option *configuration* "default" "thread-pool-size")))
			  (if (and threads (cl-ppcre:scan "^[0-9]+$" threads))
			      (parse-integer threads)
			      *thread-pool-size*)))
    (when (get-option *configuration* "default" "call-queues")
      (dolist (queue-name (cl-ppcre:split "," (get-option *configuration* "default" "call-queues")))
	(format t "Creating call queue ~A @ ~A~%" queue-name
		(get-option *configuration* queue-name "extension"))
	(let ((valet-lot-range (get-option *configuration* queue-name "valet-space-range")))
	  (destructuring-bind (start end) (cl-ppcre:split "-" valet-lot-range)
	    (create-fifo-queue 
	     queue-name 
	     (get-option *configuration* queue-name "extension")
	     (get-option *configuration* queue-name "login-logout-extension")
	     :last-resort-extension (get-option *configuration* queue-name "last-resort-extension")
	     :agent-pause (or (parse-any-number 
			       (get-option *configuration* queue-name "agent-pause"))
			      10)
	     :agent-timeout (or (parse-any-number 
				 (get-option *configuration* queue-name "agent-timeout"))
				10)
	     :max-hold-time (or (parse-any-number 
				 (get-option *configuration* queue-name "max-hold-time"))
				10)
	     :greeting (get-option *configuration* queue-name "greeting-file")
	     :valet-lot-name (get-option *configuration* queue-name "valet-lot")
	     :valet-lot-start (parse-integer start)
	     :valet-lot-end (parse-integer end)))))))
  *configuration*)
