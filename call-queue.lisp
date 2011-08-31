(in-package #:cl-freeswitch)

(defvar *fifo-queue-table* (make-hash-table :test 'equalp :synchronized t))

(defun print-fifo-caller (c s d)
  (declare (ignore d))
  (format s "#<FIFO-CALLER from ~A>" (caller-id (caller-session c))))

(defstruct (fifo-caller
	     (:conc-name nil)
	     (:print-function print-fifo-caller)
	     (:predicate fifo-caller?))
  (caller-session nil)
  (caller-connected? nil)
  (caller-valid? t)
  (caller-parking-spot nil))

(defun print-fifo-agent (a s d)
  (declare (ignore d))
  (format s "#<FIFO-AGENT ~A>" (caller-id (agent-session a))))

(defstruct (fifo-agent
	     (:conc-name nil)
	     (:print-function print-fifo-agent)
	     (:predicate fifo-agent?))
  (agent-session nil)
  (agent-extension nil)
  (agent-connected? nil)
  (agent-state :idle)
  (agent-valid? t))

(defun print-fifo-queue (q s d)
  (declare (ignore d))
  (format s "#<FIFO-QUEUE ~A @ ext ~A>" (queue-name q) (extension q)))

(defstruct (fifo-queue
	     (:conc-name nil)
	     (:print-function print-fifo-queue)
	     (:predicate fifo-queue?))
  (queue-name nil)
  (valet-lot nil)
  (parking-spots nil)
  (valet-queue (sb-queue:make-queue))
  (agent-table (make-hash-table :synchronized t :test 'equalp))
  (agent-queue (make-empty-queue))
  (caller-table (make-hash-table :synchronized t :test 'equalp))
  (caller-queue (make-empty-queue))
  (agent-pause 10)
  (agent-timeout 10)
  (max-hold-time (* 5 60))
  (greeting nil)
  (extension nil)
  (last-resort-extension nil)
  (login-logout-extension nil)
  (stop-fifo? nil)
  (fifo-queue-thread nil))

(defun show-agents ()
  (sb-ext:with-locked-hash-table (*fifo-queue-table*)
    (maphash #'(lambda (k v)
		 (format t "Agents for queue ~A:~%" k)
		 (maphash #'(lambda (name agent)
			      (format t "  ~A: ~A~%" name agent))
			  (agent-table v)))
	     *fifo-queue-table* )))

(defun show-callers ()
  (sb-ext:with-locked-hash-table (*fifo-queue-table*)
    (maphash #'(lambda (k v)
		 (format t "Callers in queue ~A:~%" k)
		 (format t "~A~%" (queue-elements (caller-queue v)))
		 (maphash #'(lambda (name caller)
			      (format t "  ~A: ~A~%" name caller))
			  (caller-table v)))
	     *fifo-queue-table* )))

(defmethod toggle-agent-indicator ((agent fifo-agent) toggle)
  (fs-sendevent (fs-stream (agent-session agent)) 
		"SWITCH_EVENT_MESSAGE_WAITING"
		(format nil "MWI-Message-Account: ~A@corp.mostgifted.com" (agent-extension agent))
		(format nil "MWI-Messages-Waiting: ~A" (if (eql toggle :on) "yes" "no"))
		"MWI-Voice-Message: 1/1 (1/1)"))

(defun stop-all-fifo-queues ()
  (sb-ext:with-locked-hash-table (*fifo-queue-table*)
    (maphash #'(lambda (name q) (setf (stop-fifo? q) t)) *fifo-queue-table*)))

(defun clear-all-fifo-queues ()
  (sb-ext:with-locked-hash-table (*fifo-queue-table*) 
    (maphash #'(lambda (name q)
		 (logger :debug "Destroying queue ~A @ ~A" name (extension q))
		 (maphash #'(lambda (name agent)
			      (declare (ignore name))
			      (ignore-errors (toggle-agent-indicator agent :off)))
			  (agent-table q))
		 (undef-extension (extension q))
		 (undef-extension (login-logout-extension q)))
	     *fifo-queue-table*)
    (clrhash *fifo-queue-table*)))

(defmethod create-fifo-caller ((q fifo-queue) &key session (valid? t))
  (let ((caller (make-fifo-caller :caller-session session :caller-valid? valid?)))
    (setf (gethash session (caller-table q)) caller)
    (enqueue-1 (caller-queue q) caller)
    caller))

(defmethod lookup-caller ((q fifo-queue) (session session))
  (gethash session (caller-table q)))

(defmethod get-next-agent ((q fifo-queue))
  (with-recursive-lock-held ((queue-lock (agent-queue q)))
    (when (not (empty-queue? (agent-queue q)))
      (let ((agent (dequeue (agent-queue q))))
	(loop until (and (fifo-agent? agent) (agent-valid? agent)) do
	     (when (empty-queue? (agent-queue q)) (return-from get-next-agent nil))
	     (setq agent (dequeue (agent-queue q))))
	agent))))

(def-operator-action hangup-fifo-call #'null :hangup)

(defmethod agent-loop ((q fifo-queue) (agent fifo-agent))
  (make-thread 
   #'(lambda ()
       (let ((*session* (agent-session agent)) (stop? nil))
	 (set-continuation #'hangup-fifo-call :bridge-finish)
	 (handler-case
	     (loop until stop? do
		  (let ((sock (usocket:wait-for-input (sock *session*) :ready-only t :timeout 1)))
		    (when sock
		      (let ((status (data-received-handler *session* #'handle-incoming-event)))
			(when (or (eql status :hangup) (eql status :abandon))
			  (logger :debug "Got ~A for agent ~A. Ending session." status *session*)
			  (setq stop? t))))))
	 (error (c)
	   (logger :err "agent-loop: ~A got error: ~A. Killing session." *session* c)))
	 (shutdown-session *session*)
	 (shutdown-connection *session*)
	 (setf (agent-connected? agent) nil
	       (agent-session agent) nil
	       (agent-state agent) :paused)
	 (thread-sleep (agent-pause q))
	 (logger :debug "Placing ~A back onto ~A" agent q)
	 (enqueue-1 q agent)
	 (setf (agent-state agent) :idle)))))

(defun get-bridge-string (extension)
  (if (cl-ppcre:scan "^[0-9]+$" extension)
      (format nil "~A@${domain_name}" extension)
      (format nil "user/~A@${domain_name}" extension)))
;      (format nil "user/${user_data(~A@${domain_name} attr number-alias)}@${domain_name}" 
;	      extension)))

(defmethod fifo-connect ((q fifo-queue) (agent fifo-agent) (caller fifo-caller))
  ;;(logger :debug "fifo-connect: connecting ~A to ~A" caller agent)
  (let ((*session* (caller-session caller)))
    (with-recursive-lock-held ((lock *session*))
      (handler-case
	  (progn
	    (do-fs-command *session* :set (format nil "effective_caller_id_number=~A" 
						  (caller-id (caller-session caller))))
	    (do-fs-command *session* :set (format nil "effective_caller_id_name=~A" 
						  (caller-id (caller-session caller))))
	    (do-fs-command *session* :set (format nil "call_timeout=~A" (agent-timeout q)))
	    (let ((response 
		   (do-fs-command *session* :bridge (get-bridge-string (agent-extension agent)))))
	      (let ((agent-uuid (fs-fetch :other-leg-unique-id response)))
		(if agent-uuid
		    (let ((agent-session 
			   (capture-call-by-uuid (fs-host (caller-session caller)) agent-uuid)))
		      (if (session? agent-session)
			  (progn
			    (when (caller-parking-spot caller)
			      (sb-queue:enqueue (caller-parking-spot caller) (valet-queue q))
			      (setf (caller-parking-spot caller) nil))
			    (logger :debug "Captured agent leg as ~A" agent-session)
			    (setf (agent-session agent) agent-session
				  (agent-connected? agent) caller 
				  (agent-state agent) :connected
				  (caller-connected? caller) agent)
			    (add-thread (agent-loop q agent)))
			  nil))
		    (progn
		      (logger :debug "Bridge failed to ~A: ~A" agent response)
		      nil)))))
	(error (c)
	  (logger :err "FIFO-CONNECT: PROBLEM WITH FS-COMMAND: ~A" c)
	  (return-from fifo-connect nil))))))

(defmethod run-fifo-queue ((q fifo-queue))
  (loop until (stop-fifo? q) 
     do
     ;;(logger :debug "run-fifo-queue looping")
     (let ((last-caller nil))
       (loop until (or (empty-queue? (caller-queue q)) (empty-queue? (agent-queue q)))
	  do
	  (let ((agent (get-next-agent q)))
	    (logger :debug "run-fifo-queue got next agent: ~A" agent)
	    (handler-case
		(if (and (fifo-agent? agent) (agent-valid? agent))
		    (let ((caller (dequeue (caller-queue q))))
		      (with-recursive-lock-held ((lock (caller-session caller)))
			(logger :debug "run-fifo-queue got next caller: ~A" caller)
			(if (not (caller-valid? caller))
			    (progn
			      (logger :debug "~A is not a valid caller" caller)
			      (enqueue-at-front-1 (agent-queue q) agent))
			    (progn
			      (when (eql last-caller caller)
				(sleep 2))
			      (setq last-caller caller)
			      (when (not (fifo-connect q agent caller))
				(logger :debug "Unable to connect to agent ~A.  Requeueing ~A." 
					agent caller)
				(enqueue-1 (agent-queue q) agent)
				(enqueue-at-front-1 (caller-queue q) caller))))))
		    (logger :debug "No agents available."))
	      (error (condition)
		(logger :debug "Error in run-fifo-queue: ~A" condition)))))
       (sleep 1))))

(defmethod login-logout-fifo-agent ((q fifo-queue) input status)
  ;;(declare (ignore input status))
  (logger :debug "in login-logout-fifo-agent for ~A ~A ~A" q status *session*)
  (let* ((extension (caller-id *session*))
	 (agent (gethash (format nil "~A" extension) (agent-table q))))
    (logger :debug "login-logout-fifo-agent got ~A / ~A" extension agent)
    (handler-case
	(if (and (fifo-agent? agent) (agent-valid? agent))
	    (progn
	      (logger :debug "Logging off ~A" agent)
	      (setf (agent-session agent) *session*)
	      (setf (agent-valid? agent) nil)
	      (remhash (format nil "~A" extension) (agent-table q))
	      (toggle-agent-indicator agent :off)
	      (set-next-step #'hangup-fifo-call :playback (prompts-dir "10000.wav")))
	    (let ((agent (make-fifo-agent :agent-extension extension
					  :agent-session *session*)))
	      (logger :debug "Logging on ~A" agent)
	      (setf (gethash (format nil "~A" extension) (agent-table q)) agent)
	      (enqueue-1 (agent-queue q) agent)
	      (toggle-agent-indicator agent :on)
	      (set-next-step #'hangup-fifo-call :playback (prompts-dir "8000.wav"))))
      (error (condition)
	(logger :err "login-logout-fifo-agent error: ~A" condition)))))

(defun abandon-fifo-call (input status)
  (declare (ignore input status))
  (logger :debug "IN ABANDON-FIFO-CALL FOR ~A" *session*)
  (set-next-step #'null :abandon-call))

(defmethod send-caller-to-last-resort ((q fifo-queue) caller)
  (logger :debug "Sending ~A to last-resort extension ~A" caller (last-resort-extension q))
  (handler-case
      (fs-command (fs-stream *session*) :session-heartbeat-off nil :uuid (uuid *session*))
    (stream-error (c)
      (logger :err "send-caller-to-last-resort: PROBLEM WITH FS-COMMAND: ~A" c)))
  (logger :debug "Deleting ~A from ~A" *session* q)
  (setf (caller-valid? caller) nil)
  (when (caller-parking-spot caller)
    (sb-queue:enqueue (caller-parking-spot caller) (valet-queue q))
    (setf (caller-parking-spot caller) nil))
  (remhash *session* (caller-table q))
  (set-next-step #'abandon-fifo-call :transfer
		 (format nil "~A XML default" (last-resort-extension q))))

(defmethod expire-fifo-session ((q fifo-queue) input status)
  (with-recursive-lock-held ((lock *session*))
    (let ((caller (lookup-caller q *session*)))
      (if (fifo-caller? caller)
	  (if (caller-connected? caller)
	      ;; FIXME: Eh? How is the caller connected to an agent but still receiving heartbeats?
	      (logger :debug "Cannot expire connected caller ~A!" caller)
	      (send-caller-to-last-resort q caller))
	  (logger :debug "expire-fifo-session: invalid caller for session ~A" *session*)))))

(defmethod skip-first-heartbeat ((q fifo-queue) input status)
  (set-continuation #'(lambda (i s) (expire-fifo-session q i s)) :session-heartbeat))

(defmethod place-caller-in-queue ((q fifo-queue) input status)
  (with-recursive-lock-held ((lock *session*))
    (logger :debug "Placing new caller in queue ~A: ~A" q *session*)
    (set-continuation #'(lambda (i s) (skip-first-heartbeat q i s)) :session-heartbeat)
    (let ((caller (create-fifo-caller q :session *session*)))
      (handler-case
	  (let ((parking-spot (sb-queue:dequeue (valet-queue q))))
	    (if parking-spot
		(progn
		  (logger :debug "Putting ~A in spot ~A" caller parking-spot)
		  (setf (caller-parking-spot caller) parking-spot)
		  (fs-command (fs-stream *session*) 
			      :valet-park (list (valet-lot q) parking-spot)
			      :uuid (uuid *session*))
		  (fs-command (fs-stream *session*) 
			      :session-heartbeat (list (max-hold-time q) (max-hold-time q))
			      :uuid (uuid *session*)))
		(progn
		  (logger :debug "~A: no parking spots.  sending caller ~A to ~A." 
			  (queue-name q) caller (last-resort-extension q))
		  (send-caller-to-last-resort q caller))))
	(stream-error (c)
	  (logger :err "place-caller-in-queue: PROBLEM WITH FS-COMMAND: ~A" c)))
      caller)))

(defmethod start-fifo-queue-session ((q fifo-queue) input status)
  (logger :debug "IN OPERATOR METHOD: start-fifo-queue-session for ~A" q)
  (if (and (greeting q) (probe-file (greeting q)))
      (progn
	(set-continuation #'(lambda (i s) (place-caller-in-queue q i s)) :playback)
	(handler-case
	    (fs-command (fs-stream *session*) :playback (list (greeting q)) :uuid (uuid *session*))
	  (stream-error (c)
	    (logger :err "start-fifo-queue-session: PROBLEM WITH FS-COMMAND: ~A" c))))
      (place-caller-in-queue q input status)))

(defun create-fifo-queue (name extension login-logout-extension &key (agent-pause 10) 
			  (agent-timeout 10) (max-hold-time (* 5 60)) greeting 
			  last-resort-extension valet-lot-name valet-lot-start valet-lot-end)
  (cond ((gethash name *fifo-queue-table*)
	 (error "A queue by the name ~A already exists." name))
	((extension-exists? extension)
	 (error "Extension ~A already exists." extension))
	((extension-exists? login-logout-extension)
	 (error "Login / logout extension ~A already exists." login-logout-extension))
	(t 
	 (let ((q (make-fifo-queue :queue-name name
				   :valet-lot valet-lot-name
				   :parking-spots nil
				   :extension extension
				   :login-logout-extension login-logout-extension
				   :agent-pause agent-pause 
				   :agent-timeout agent-timeout
				   :max-hold-time max-hold-time
				   :greeting greeting
				   :last-resort-extension last-resort-extension)))
	   (loop for x from valet-lot-start to valet-lot-end do
	     (sb-queue:enqueue x (valet-queue q)))
	   (setf (gethash name *fifo-queue-table*) q)
	   (def-extension login-logout-extension 
	       #'(lambda (i s) (login-logout-fifo-agent q i s)))
	   (def-extension extension 
	       #'(lambda (i s) (start-fifo-queue-session q i s)))
	   (setf (fifo-queue-thread q)
		 (make-thread #'(lambda () (run-fifo-queue q)) 
			      :name (format nil "~A FIFO Queue Thread" name)))
	   q))))

