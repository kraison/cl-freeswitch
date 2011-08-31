(in-package #:cl-freeswitch)

(defun set-quit (&rest args)
  (setf *stop-listener* t))

(defun start (&key (config-file *configuration-file*))
  (load-configuration :file config-file)
  (when (and (threadp *listener-thread*) (thread-alive-p *listener-thread*))
    (error 'listener-error "Listener already running: ~A" *listener-thread*))
  (setf *listener-thread* 
	;;(make-thread #'(lambda () (start-listener *port* :address "127.0.0.1")) :name "listener-thread")))
	(make-thread #'(lambda () (start-listener *port*)) :name "listener-thread")))

(defun stop ()
  (stop-all-fifo-queues)
  (clear-all-fifo-queues)
  (stop-listener)
  (sleep 1)
  (when (and (threadp *listener-thread*) (thread-alive-p *listener-thread*))
    (format t "Shutting down ~A~%" *listener-thread*)
    (destroy-thread *listener-thread*))
  (setf *listener-thread* nil)
  (sb-ext:with-locked-hash-table (*sessions*)
    (maphash #'(lambda (id session)
                 (declare (ignore id))
                 (format t "Shutting down session ~A~%" session)
		 (handler-case
		     (trivial-timeout:with-timeout (1)
		       (shutdown-session session))
		   (error (c)
		     (format t "Unable to kill session ~A: ~A~%" session c))))
             *sessions*))
  (clrhash *sessions*)
  nil)

#|
;; A test path of execution for extension 5000
(def-operator-action start-customer-ivr #'set-customer-voice :set "tts_engine=flite")
(def-operator-action set-customer-voice #'set-customer-lang :set "tts_voice=kal")
(def-operator-action set-customer-lang #'greeting :set "default_language=en")
(def-extension "5000" #'start-customer-ivr)
(defun teardown (input status)
  (handler-case
      (progn
        (logger :debug "IN TEARDOWN FOR ~A" (get-session-var :username)))
    (error (c)
      (logger :err "teardown got error ~A" c))))
(def-operator-action hang-up-call #'teardown :hangup)
(def-operator-action end-call #'hang-up-call :playback "/home/raison/goodbye.wav")
(def-operator-action greeting #'end-call :speak "Welcome to the test I V R.")
|#
