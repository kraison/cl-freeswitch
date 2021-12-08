(in-package #:cl-freeswitch)

(define-condition freeswitch-client-error (error)
  ((reason :initarg :reason))
  (:report (lambda (error stream)
             (with-slots (reason)
                 error
               (format stream "FS client error: ~A." reason)))))

(defun fs-parse-new (raw)
  (let ((all-inputs nil) (current-input nil))
    (map nil
	 #'(lambda (line)
	     (if (and (scan-newline line) current-input)
		 (progn
		   (push (nreverse current-input) all-inputs)
		   (setf current-input nil))
		 (let ((pos (search ":" line)))
		   (if pos
		       (let ((key (intern (cl-ppcre:regex-replace-all 
					   "_" (string-upcase (trim-ws (subseq line 0 pos))) "-")
					  'keyword))
			     (value (subseq line (1+ pos))))
			 (push (cons key (trim-ws (hunchentoot:url-decode value))) current-input))
		       (push (cons (trim-ws line) "") current-input)))))
	 raw)
    (when current-input (push (nreverse current-input) all-inputs))
    (nreverse all-inputs)))

(defun fs-parse (raw)
  (mapcar 
   #'(lambda (input)
       (mapcar 
	#'(lambda (line)
	    (let ((pos (search ":" line)))
	      (if pos
		  (let ((key (intern (cl-ppcre:regex-replace-all 
				      "_" (string-upcase (trim-ws (subseq line 0 pos))) "-")
				     'keyword))
			(value (subseq line (1+ pos))))
		    (cons key (trim-ws (hunchentoot:url-decode value))))
		  (cons (trim-ws input) ""))))
	(my-split input :ws '(#\Newline #\Return #\Linefeed))))
   (cl-ppcre:split "(\\n\\n|\\r\\n\\r\\n)" raw)))

(defun fs-parse-line (input)
  (let ((pos (search ":" input)))
    (if pos
	(let ((key (intern (cl-ppcre:regex-replace-all 
			    "_" (string-upcase (trim-ws (subseq input 0 pos))) "-") 
			   'keyword))
	      (value (subseq input (1+ pos))))
	  (values key (trim-ws (hunchentoot:url-decode value))))
	(values (trim-ws input) ""))))

(defun fs-read-until-newline (stream)
  (let ((request nil))
    (do ((input (read-line stream) (read-line stream)))
	((or (null input) (scan-newline input)))
      (multiple-value-bind (key value) (fs-parse-line input)
	(push (cons key value) request)))
    (nreverse request)))

(defun fs-parse-clob (clob)
  (mapcar #'(lambda (line)
	      (multiple-value-bind (key value) (fs-parse-line line)
		(cons key value)))
	  (my-split (coerce clob 'string) :ws '(#\Newline #\Return #\Linefeed))))

(defmethod fs-read-chars ((stream stream) bytes)
  (let ((buf nil))
    (handler-case
	(dotimes (i bytes)
	  (push (read-char stream) buf))
      (error (condition)
	(logger :info "READ-CHARS ERRED ~A" condition)))
    (fs-parse-clob (nreverse buf))))

(defun fs-read-all (session &key convert?)
  "Reads all pending characters on a socket into the session buffer"
  (let ((buffer (buffer session))
	(sock (sock session))
	(kv-pairs nil))
    (do ((fin nil))
	(fin t)
      (setf (fill-pointer buffer) +buflen+)
      (multiple-value-bind (buf len raddr) 
	  (usocket:socket-receive sock buffer nil :element-type 'character)
	(declare (ignore raddr))
	(if (null buf)
	    (setf fin t)
	    (setf (fill-pointer buffer) len)))
      (cond ((= (length buffer) 0)
	     (setf fin t))
	    (fin 
	     (format t "Got NIL, returning~%"))
	    (t
	     (format t "  Read ~a bytes: ~a~%" (length buffer) buffer)
	     ;; FIXME: split on newline
	     (if convert?
		 (multiple-value-bind (key value) (fs-parse-line input)
		   (push (cons key value) kv-pairs))
		 (push input kv-pairs)))))
    (nreverse kv-pairs)))

(defmethod fs-read ((stream stream) &optional sock)
  (let ((request nil))
    (handler-case
	(progn
	  (setf request (fs-read-until-newline stream))
	  (let ((content-length (fs-fetch :content-length request)))
	    (when content-length
	      (if (cl-ppcre:scan "^[0-9]+$" content-length)
		  (progn
		    (setf content-length (parse-integer content-length))
		    (let ((extra (fs-read-chars stream content-length)))
		      (setf request (nconc request extra))))
		  (logger :warning "BAD CONTENT-LENGTH: ~A" content-length))))
	  request)
      #+sbcl
      (SB-INT:CLOSED-STREAM-ERROR (condition)
	(logger :err "FS-READ: got ~A.  Read ~A" condition request)
	request)
      (trivial-timeout:timeout-error (condition)
	(declare (ignore condition))
	(logger :err "FS-READ: Timeout. Read: ~A" request)
	request))))

(defun fs-fetch (sym alist)
  (cdr (assoc sym alist)))

(defmethod recognize? (expecting raw-input)
  "Looks for a set of key/value pairs in the raw freeswitch input.  Returns true if all kv pairs
are found."
  (when (every #'true
	       (mapcar #'(lambda (item)
			   (equalp (cdr (assoc (car item) raw-input)) (cdr item)))
		       expecting))
    (return-from recognize? expecting)))

(defgeneric fs-command (stream command args &key event-lock uuid &allow-other-keys))
(defgeneric fs-command-ok? (command uuid input))

(defmacro def-fs-command (command str &key uuid?)
  (let ((cmd (gensym)))
    (if uuid?
	`(defmethod fs-command ((stream stream) (command (eql ,command)) args &key event-lock uuid
				(with-lock? t))
	   (if (open-stream-p stream)
	       (let ((,cmd (apply #'format nil ,str (nconc (list uuid) args))))
		 (flet ((do-write ()
			  (format stream ,cmd)
			  (if event-lock
			      (format stream "event-lock: true~%~%")
			      (format stream "~%"))
			  (force-output stream)))
		   (logger :debug "~A" ,cmd)
		   (add-history (list :output ,cmd))
		   (if with-lock?
		       (with-recursive-lock-held ((stream-lock *session*))
			 (do-write))
		       (do-write))))
	       (error 'freeswitch-client-error 
		      :reason (format nil "Stream closed on ~A." *session*))))
	`(defmethod fs-command ((stream stream) (command (eql ,command)) args &key event-lock uuid
				(with-lock? t))
	   (declare (ignore uuid))
	   (if (open-stream-p stream)
	       (let ((,cmd (apply #'format nil ,str args)))
		 (flet ((do-write ()
			  (format stream ,cmd)
			  (if event-lock
			      (format stream "event-lock: true~%~%")
			      (format stream "~%"))
			  (force-output stream)))
		   (logger :debug "~A" ,cmd)
		   (add-history (list :output ,cmd))
		   (if with-lock?
		       (with-recursive-lock-held ((stream-lock *session*))
			 (do-write))
		       (do-write))))
	       (error 'freeswitch-client-error 
		      :reason (format nil "Stream closed on ~A." *session*)))))))

(defmacro def-recognizer (command default-return-value &rest options)
  (let ((cmd (gensym)))
    `(defmethod fs-command-ok? ((,cmd (eql ,command)) uuid input)
       (or
	,@(mapcar #'(lambda (o)
		      (let ((rv (first o)) (alist (second o)))
			`(and (recognize? ',alist input) ,rv)))
		  options)
	,default-return-value))))

(let ((disconnect1 '((:content-type . "text/disconnect-notice")
		     (:content-disposition . "disconnect")))
      (disconnect2 '((:event-name . "CHANNEL_HANGUP_COMPLETE")))
      (disconnect3 '((:event-name . "CHANNEL_HANGUP")))
      (disconnect4 '((:event-name . "CHANNEL_DESTROY"))))
  (defmethod fs-command-ok? :around (command uuid input)
    (if (and uuid (or (assoc :unique-id input) (assoc :controlled-session-uuid input)))
	(if (or (recognize? `((:unique-id . ,uuid)) input)
		(recognize? `((:controlled-session-uuid . ,uuid)) input))
	    (if (or (recognize? disconnect1 input) (recognize? disconnect2 input) 
		    (recognize? disconnect3 input) (recognize? disconnect4 input))
		:hangup
		(call-next-method))
	    :ignore) ;; Wrong UUID.
	(call-next-method))))

(def-fs-command :exit "exit~%" :uuid? nil)
(def-recognizer :exit :ignore
    (:ok ((:reply-text . "OK bye"))
    (:ok ((:content-type . "text/disconnect-notice")))))

(def-fs-command :myevents "events myevents ~A all~%" :uuid? t)
(def-recognizer :myevents :not-ok
    (:ok ((:content-type . "command/reply") (:reply-text . "OK Events Enabled")))
    (:ok ((:content-type . "command/reply") (:reply-text . "OK event listener enabled plain"))))

(def-fs-command :session-heartbeat "api uuid_session_heartbeat ~A ~A ~A~%" :uuid? t)
(def-recognizer :session-heartbeat :ignore
    (:ok ((:event-name . "SESSION_HEARTBEAT"))))

(def-fs-command :session-heartbeat-off "api uuid_session_heartbeat ~A~%" :uuid? t)
(def-recognizer :session-heartbeat-off :ignore
    (:ok ((:event-name . "SESSION_HEARTBEAT"))))

(def-fs-command :answer "sendmsg ~A~%call-command: execute~%execute-app-name: answer~%" :uuid? t)
(def-recognizer :answer :unknown
  (:ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "answer")))
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "answer")))
  (:ignore ((:event-name . "CHANNEL_ANSWER") (:variable-current-application . "answer")))
  (:ignore ((:event-name . "DTMF"))))

(def-fs-command :break "sendmsg ~A~%call-command: execute~%execute-app-name: break~%" :uuid? t)
(def-recognizer :break :unknown
  (:ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "break")))
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "break"))))

(def-fs-command :uuid-broadcast "api uuid_broadcast ~A ~A ~A~%")
(def-recognizer :uuid-broadcast :unknown
  (:ok     ((:event-name . "CHANNEL_EXECUTE_COMPLETE")
	    (:application . "uuid_broadcast")))
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "uuid_broadcast")))
  (:dtmf   ((:event-name . "DTMF"))))

(def-fs-command :uuid-bridge "api uuid_bridge ~A ~A~%")
(def-recognizer :uuid-bridge-connect-outbound :unknown
  (:ok ((:event-name . "CHANNEL_EXECUTE") (:application . "uuid_bridge")))
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_UNBRIDGE")
	    (:variable-current-application . "uuid_bridge")
	    (:variable-originate-disposition . "SUCCESS")))
  (:not-ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE")
	    (:application . "uuid_bridge")
	    (:variable-originate-disposition . "USER_NOT_REGISTERED")))
  (:dtmf ((:event-name . "DTMF"))))
;; FIXME: need to anaylze possible failure conditions a bit better.
(def-recognizer :uuid-bridge-connect-inbound :unknown
  (:ok     ((:event-name . "CHANNEL_EXECUTE") (:application . "uuid_bridge")))
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_UNBRIDGE")
	    (:variable-current-application . "uuid_bridge")
	    (:variable-originate-disposition . "SUCCESS")))
  (:not-ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE")
	    (:application . "uuid_bridge")
	    (:variable-originate-disposition . "USER_NOT_REGISTERED")))
  (:dtmf ((:event-name . "DTMF"))))
(def-recognizer :uuid-bridge-connect-inbound-or-heartbeat :unknown
  (:ok ((:event-name . "SESSION_HEARTBEAT")))
  (:ok     ((:event-name . "CHANNEL_EXECUTE") (:application . "uuid_bridge")))
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_UNBRIDGE")
	    (:variable-current-application . "uuid_bridge")
	    (:variable-originate-disposition . "SUCCESS")))
  (:not-ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE")
	    (:application . "uuid_bridge")
	    (:variable-originate-disposition . "USER_NOT_REGISTERED")))
  (:dtmf ((:event-name . "DTMF"))))

(def-fs-command :bridge 
    "sendmsg ~A~%call-command: execute~%execute-app-name: bridge~%execute-app-arg: ~A~%" :uuid? t)
(def-recognizer :bridge :unknown
  (:ok ((:event-name . "CHANNEL_BRIDGE")
	     (:variable-current-application . "bridge")
	     (:variable-originate-disposition . "SUCCESS")))
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "bridge")))
  (:ignore ((:event-name . "CHANNEL_UNBRIDGE")
	    (:variable-current-application . "bridge")
	    (:variable-originate-disposition . "SUCCESS")))
  (:not-ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE")
	    (:application . "bridge")
	    (:variable-originate-disposition . "USER_NOT_REGISTERED")))
  (:not-ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE")
	    (:application . "bridge")
	    (:variable-originate-disposition . "NETWORK_OUT_OF_ORDER")))
  (:ignore ((:event-name . "DTMF"))))
(def-recognizer :bridge-connect :unknown
  (:ok ((:event-name . "CHANNEL_BRIDGE")
	(:variable-current-application . "bridge")
	(:variable-originate-disposition . "SUCCESS")))
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "bridge")))
  (:ignore ((:event-name . "CHANNEL_UNBRIDGE")
	    (:variable-current-application . "bridge")
	    (:variable-originate-disposition . "SUCCESS")))
  (:not-ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE")
	    (:application . "bridge")
	    (:variable-originate-disposition . "USER_NOT_REGISTERED")))
  (:dtmf ((:event-name . "DTMF"))))
(def-recognizer :bridge-finish :unknown
  (:ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "bridge")))
  (:ok ((:event-name . "CHANNEL_PARK")))
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "bridge")))
  (:ignore ((:event-name . "CHANNEL_BRIDGE")
	    (:variable-current-application . "bridge")
	    (:variable-originate-disposition . "SUCCESS")))
  (:ignore ((:event-name . "CHANNEL_UNBRIDGE")
	    (:variable-current-application . "bridge")
	    (:variable-originate-disposition . "SUCCESS")))
  (:not-ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE") 
	    (:application . "bridge")
	    (:variable-originate-disposition . "USER_NOT_REGISTERED")))
  (:dtmf ((:event-name . "DTMF"))))
(def-recognizer :bridge-finish-or-heartbeat :unknown
  (:ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "bridge")))
  (:ok ((:event-name . "CHANNEL_PARK")))
  (:ok ((:event-name . "SESSION_HEARTBEAT")))
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "bridge")))
  (:ignore ((:event-name . "CHANNEL_BRIDGE")
	    (:variable-current-application . "bridge")
	    (:variable-originate-disposition . "SUCCESS")))
  (:ignore ((:event-name . "CHANNEL_UNBRIDGE")
	    (:variable-current-application . "bridge")
	    (:variable-originate-disposition . "SUCCESS")))
  (:not-ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE") 
	    (:application . "bridge")
	    (:variable-originate-disposition . "USER_NOT_REGISTERED")))
  (:dtmf ((:event-name . "DTMF"))))

(def-fs-command :gentones
    "sendmsg ~A~%call-command: execute~%execute-app-name: gentones~%execute-app-arg: ~A~%" :uuid? t)
(def-recognizer :gentones :unknown
  (:ok     ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "gentones")))
  (:ok     ((:event-name . "CHANNEL_EXECUTE_COMPLETE") 
	    (:variable-current-application . "gentones")))
  (:dtmf   ((:event-name . "DTMF"))))

(def-fs-command :playback
    "sendmsg ~A~%call-command: execute~%execute-app-name: playback~%execute-app-arg: ~A~%" :uuid? t)
(def-recognizer :playback :unknown
  (:ok     ((:event-name . "CHANNEL_EXECUTE_COMPLETE")
	    (:application-response . "FILE PLAYED")
	    (:application . "playback")))
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "playback")))
  (:dtmf   ((:event-name . "DTMF"))))

(def-fs-command :speak
    "sendmsg ~A~%call-command: execute~%execute-app-name: speak~%execute-app-arg: ~A~%" :uuid? t)
(def-recognizer :speak :unknown
  (:ok     ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "speak")))
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "speak")))
  (:dtmf   ((:event-name . "DTMF"))))

(def-fs-command :say 
    "sendmsg ~A~%call-command: execute~%execute-app-name: say~%execute-app-arg: ~A ~A ~A ~A~%"
  :uuid? t)
(def-recognizer :say :unknown
  (:ok     ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "say")))
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "say")))
  (:dtmf   ((:event-name . "DTMF"))))

(defmethod fs-command ((stream stream) (command (eql :play-and-get-digits)) args &key uuid event-lock)
  "This one violates the pattern set in the macro.  Oh well.  Once exception isn't bad.
<min> <max> <tries> <timeout> <terminators> <file> <invalid_file> <var_name> <regexp>"
  (let ((command
	 (with-output-to-string (s)
	   (format s "sendmsg ~A~%call-command: execute~%execute-app-name: play_and_get_digits~%"
		   uuid)
	   (format s "execute-app-arg: ~A ~A ~A ~A ~A ~A ~A ~A ~A~%" 
		   (nth 0 args) (nth 1 args) (nth 2 args) (nth 3 args) (nth 4 args)
		   (nth 5 args) (nth 6 args) "user_input" (if (nth 7 args) (nth 7 args) "\\d+")))))
    (with-recursive-lock-held ((stream-lock *session*))
      (format stream "~A" command)
      (if event-lock
	  (format stream "event-lock: true~%~%")
	  (format stream "~%"))
      (force-output stream))
    (logger :debug "PLAY_AND_GET_DIGITS: ~A ~A" command event-lock)
    (add-history (list :output command))))
(def-recognizer :play-and-get-digits :unknown
  (:user-failure ((:event-name . "CHANNEL_EXECUTE_COMPLETE") 
		  (:application . "play_and_get_digits")
		  (:variable-read-result . "failure")))
  (:ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE") 
	(:application . "play_and_get_digits")
	(:variable-read-result . "success")))
  (:ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE") 
	(:application . "play_and_get_digits")
	(:variable-read-result . "timeout")))
  (:hangup ((:content-type . "text/disconnect-notice")
	    (:content-disposition . "disconnect")))
  (:dtmf ((:event-name . "DTMF"))))
;  (:ignore ((:event-name . "DTMF"))))

(def-fs-command :sleep 
    "sendmsg ~A~%call-command: execute~%execute-app-name: sleep~%execute-app-arg: ~A~%" :uuid? t)
(def-recognizer :sleep :unknown
  (:ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "sleep"))))

(def-recognizer :sleep-or-bridge-finish :unknown
  (:sleep-ok  ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "sleep")))
  (:bridge-ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "bridge")))
  (:bridge-ok ((:event-name . "CHANNEL_PARK")))
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "bridge")))
  (:ignore ((:event-name . "CHANNEL_BRIDGE")
            (:variable-current-application . "bridge")
            (:variable-originate-disposition . "SUCCESS")))
  (:ignore ((:event-name . "CHANNEL_UNBRIDGE")
            (:variable-current-application . "bridge")
            (:variable-originate-disposition . "SUCCESS")))
  (:not-ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE")
            (:application . "bridge")
            (:variable-originate-disposition . "USER_NOT_REGISTERED")))
  (:dtmf ((:event-name . "DTMF"))))

(def-fs-command :send-dtmf
    "sendmsg ~A~%call-command: execute~%execute-app-name: send_dtmf~%execute-app-arg: ~A@~A~%"
  :uuid? t)
(def-recognizer :send-dtmf :unknown
  (:ok ((:|OK SUCCESS| . "")))
  (:ok ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "send_dtmf"))))

(def-fs-command :park "sendmsg ~A~%call-command: execute~%execute-app-name: park~%" :uuid? t)
(def-recognizer :park :unknown
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "park")))
  (:ok     ((:event-name . "CHANNEL_PARK"))))

(def-fs-command :hangup "sendmsg ~A~%call-command: execute~%execute-app-name: hangup~%" :uuid? t)
(def-recognizer :hangup :unknown
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "hangup")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "hangup")))
  (:ignore ((:event-name . "CHANNEL_UNPARK") (:variable-current-application . "hangup")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:variable-current-application . "hangup")))
  (:ignore ((:event-name . "CHANNEL_HANGUP") (:variable-current-application . "hangup")))
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "DTMF")))
  (:ok     ((:content-type . "text/disconnect-notice") (:content-disposition . "disconnect"))))

(def-recognizer :psychic-outgoing-finished :unknown
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "hangup")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "hangup")))
  (:ignore ((:event-name . "CHANNEL_UNPARK") (:variable-current-application . "hangup")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:variable-current-application . "hangup")))
  (:dtmf   ((:event-name . "DTMF")))
  (:ok     ((:event-name . "CHANNEL_HANGUP_COMPLETE")))
  (:ok     ((:event-name . "CHANNEL_HANGUP")))
  (:ok     ((:event-name . "CHANNEL_DESTROY")))
  (:ok     ((:content-type . "text/disconnect-notice") (:content-disposition . "disconnect"))))

(def-fs-command :set-playback-terminators
    "sendmsg ~A~%call-command: execute~%execute-app-name: set~%execute-app-arg: playback_terminators=~A~%"
  :uuid? t)
(def-recognizer :set-playback-terminators :unknown
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "set")))
  (:dtmf   ((:event-name . "DTMF")))
  (:ok     ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "set"))))

;;"sendmsg ~A~%call-command: execute~%execute-app-name: record~%execute-app-arg: ~{~A~^ ~}~%"
(def-fs-command :record 
    "sendmsg ~A~%call-command: execute~%execute-app-name: record~%execute-app-arg: ~A ~A~%"
  :uuid? t)
(def-recognizer :record :unknown
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "record")))
  (:dtmf   ((:event-name . "DTMF")))
  (:ok     ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "record"))))

(def-fs-command :uuid-record-start "api uuid_record ~A start ~A~%")
(def-recognizer :uuid-record-start :unknown
  (:ok ((:content-type . "api/response") (:content-length . "12"))))

(def-fs-command :uuid-record-stop "api uuid_record ~A stop ~A~%")
(def-recognizer :uuid-record-stop :unknown
  (:ok ((:content-type . "api/response") (:content-length . "12"))))

(def-fs-command :set 
    "sendmsg ~A~%call-command: execute~%execute-app-name: set~%execute-app-arg: ~A~%"
  :uuid? t)
(def-recognizer :set :unknown
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "set")))
  (:dtmf   ((:event-name . "DTMF")))
  (:ok     ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "set"))))

;; FIXME: need to validate recognizer patterns
(def-fs-command :transfer
    "sendmsg ~A~%call-command: execute~%execute-app-name: transfer\~%execute-app-arg: ~A~%"
  :uuid? t)
(def-recognizer :transfer :unknown
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "transfer")))
  (:dtmf   ((:event-name . "DTMF")))
  (:ok     ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "transfer"))))

;; FIXME: need to validate recognizer patterns
(def-fs-command :valet-park
    "sendmsg ~A~%call-command: execute~%execute-app-name: valet_park\~%execute-app-arg: ~A ~A~%"
  :uuid? t)
(def-recognizer :valet-park :unknown
  (:ignore ((:content-type . "command/reply") (:reply-text . "OK")))
  (:ignore ((:event-name . "CHANNEL_EXECUTE") (:application . "valet_park")))
  (:dtmf   ((:event-name . "DTMF")))
  (:ok     ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "park")))
  (:ok     ((:event-name . "CHANNEL_PARK")))
  (:ok     ((:event-name . "CHANNEL_EXECUTE_COMPLETE") (:application . "valet_park"))))

(defmethod fs-command ((stream stream) (command (eql :noop)) args &key uuid event-lock)
  (declare (ignore args stream command event-lock uuid)))

(defmethod fs-command-ok? ((command (eql :noop)) uuid input)
  :ok)

(defmethod fs-command ((stream stream) (command (eql :abandon-call)) args &key uuid event-lock)
  (declare (ignore args stream command event-lock uuid)))

(defmethod fs-command-ok? ((command (eql :abandon-call)) uuid input)
  :abandon)

(defmethod fs-command ((stream stream) command args &key uuid event-lock &allow-other-keys)
  (logger :warning "UNKNOWN COMMAND: ~A" (or command 'null)))

;(defun do-fs-command (session command &rest args)
;  (handler-case
;      (progn
;	(fs-command (fs-stream session) command args :uuid (uuid session))
;	(let ((sock (usocket:wait-for-input (sock session) :ready-only t :timeout 1)))
;	  (when sock
;	    (let ((response (fs-read-all
;	    (let ((status (data-received-handler *session* recv-func)))
;	      ))
;    (error (c)
;      (logger :err "PROBLEM WITH FS-COMMAND ~A (~A): ~A" command args c))))

(defmethod fs-sendevent ((stream stream) event &rest headers)
  (logger :debug "FS-SENDEVENT: ~A ~A" event headers)
  (format stream "sendevent ~A~%" (string-upcase event))
  (dolist (header headers)
    (format stream "~A~%" header))
  (format stream "~%")
  (force-output stream))

(defmethod fs-connect ((stream stream))
  (logger :debug "FS-CONNECT: sending connect")
  (format stream "connect~%~%")
  (force-output stream))

(defun fs-get-active-channels ()
  (let* ((sock (usocket:socket-connect *fs-host* *fs-port*))
	 (stream (usocket:socket-stream sock)))
    (unwind-protect
	 (progn
	   (fs-read stream)
	   (format stream "auth ~A~%~%" *fs-auth*)
	   (force-output stream)
	   (fs-read stream)
	   (format stream "api show channels~%~%")
	   (force-output stream)
	   (let ((channels (fs-read stream)))
	     (remove-if #'null
			(mapcar #'(lambda (l)
				    (cond ((or (eql (car l) :content-type)
					       (eql (car l) :content-length)
					       (and (stringp (car l))
						    (or (= 0 (length (car l)))
							(cl-ppcre:scan "^[0-9]+ total" (car l)))))
					   nil)
					  ((and (stringp (cdr l)) (> (length (cdr l)) 0))
					   (cdr l))))
				channels))))
      (progn
	(ignore-errors
	  (format stream "exit~%~%")
	  (force-output stream))
	(usocket:socket-close sock)))))

(defmethod fs-setup-call ((stream stream) sock)
  "Setup session info and connect the call."
  (logger :debug "Handling client ~A" (usocket:get-peer-name sock))
  (fs-connect stream)
  (let ((request (fs-read stream sock)))
    (let ((session (or (if (lookup-session (fs-fetch :unique-id request))
			   (let ((session (lookup-session (fs-fetch :unique-id request))))
			     (setf (fs-stream session) stream
				   (raw-connect-info session) request
				   (fs-host session) (ip-to-string (usocket:get-peer-name sock))
				   (sock session) sock
				   (caller-id session) (fs-fetch :caller-caller-id-number request)
				   (destination session) (fs-fetch :caller-destination-number request))
			     session)
			   (create-session :stream stream
					   :raw-connect-info request
					   :fs-host (ip-to-string (usocket:get-peer-name sock))
					   :sock sock
					   :uuid (fs-fetch :unique-id request)
					   :caller-id (fs-fetch :caller-caller-id-number request)
					   :destination (fs-fetch :caller-destination-number request))))))
      (logger :debug "SESSION IS ~A" session)
      (logger :debug "FS-SETUP-CALL: sending myevents")
      (format stream "event myevents ~A all~%~%" (uuid session))
      (force-output stream)
      (let ((response (fs-read stream sock)))
	(logger :info "FS-SETUP-CALL GOT RESPONSE: ~A" response)
	(if (eql (fs-command-ok? :myevents nil response) :ok)
	    session
	    (error 'freeswitch-client-error 
		   :reason (format nil "Unable to subscribe to events: ~A" response)))))))

(defun extract-uuid-from-outgoing (input)
  (let ((item (find "OK" input 
		    :key 'car 
		    :test #'(lambda (r i)
			      (format t "Sreaching for ~A in ~A~%" r i)
			      (when (stringp i) 
				(cl-ppcre:scan r i))))))
    (if item
	(let ((uuid (cl-ppcre:scan-to-strings "([0-9a-zA-Z]+\-){4}[0-9a-zA-Z]+" (car item))))
	  (format t "Found ~A in reponse~%" item)
	  (if uuid
	      uuid
	      (error 'freeswitch-client-error :reason (format nil "No UUID found in ~A" input))))
	(error 'freeswitch-client-error :reason (format nil "No UUID found in ~A" input)))))

(defun fs-login (fs-host)
  (let* ((sock (usocket:socket-connect fs-host *fs-port*))
	 (stream (usocket:socket-stream sock)))
    (logger :debug "Logging in to FreeSWITCH at ~A" (usocket:get-peer-name sock))
    (let ((response (fs-read stream sock)))
      (if (recognize? '((:content-type . "auth/request")) response)
	  (progn
	    (format stream "auth ~A~%~%" *fs-auth*)
	    (force-output stream)
	    (let ((response (fs-read stream sock)))
	      (if (recognize? '((:content-type . "command/reply") 
				(:reply-text . "OK accepted")) response)
		  (progn
		    (logger :debug "fs-login to ~A successful." (usocket:get-peer-name sock))
		    (values stream sock))
		  nil)))
	  nil))))

(defun capture-call-by-uuid (fs-host uuid)
  (logger :debug "capture-call-by-uuid for ~A @ ~A" uuid fs-host)
  (let ((stream nil) (sock nil) (session nil))
    (handler-case
	(multiple-value-bind (stream1 sock1) (fs-login fs-host)
	  (setq stream stream1 
		sock sock1
		session (create-session :stream stream :sock sock :uuid uuid))
	    (logger :debug "event myevents ~A ALL~%~%" uuid)
	    (format stream "event myevents ~A ALL~%~%" uuid)
	    (force-output stream)
	    (fs-read stream sock) ;; FIXME: check for errors.
	    (logger :debug "capture-call-by-uuid returning ~A" session)
	    session)
      (error (c)	
	(logger :err "Error in capture-call-by-uuid: ~A" c)
	(when (session? session)
	  (shutdown-session session))
	(when (and stream (open-stream-p stream))
	  (ignore-errors (usocket:socket-close sock)
			 (close stream)))
	nil))))
	
(defmethod fs-setup-outgoing-call ((stream stream) sock destination &key (timeout 60) 
				   (application "&park()") caller-id)
  (let ((session nil))
    (logger :debug "Logging in to FreeSWITCH at ~A" (usocket:get-peer-name sock))
    (let ((response (fs-read stream sock)))
      (if (recognize? '((:content-type . "auth/request")) response)
	  (progn
	    (format stream "auth ~A~%~%" *fs-auth*)
	    (force-output stream)
	    (let ((response (fs-read stream sock)))
	      (if (recognize? '((:content-type . "command/reply") (:reply-text . "OK accepted")) 
			      response)
		  (progn
		    (logger :debug "fs-setup-outgoing-call authentication successful. calling ~A" 
			    destination)
		    (format 
		     stream 
		     "api originate {ignore_early_media=true,originate_timeout=~A~A}~A ~A~%~%"
		     timeout 
		     (if caller-id (format nil ",origination_caller_id_number=~A" caller-id) "")
		     destination 
		     application)
		    (force-output stream)
		    (let ((response (fs-read stream sock)))
		      (if (recognize? '((:content-type . "api/response")) response)
			  (let ((uuid (extract-uuid-from-outgoing response)))
			    (logger :debug "fs-setup-outgoing-call called originate and got UUID ~A"
				    uuid)
			    (setf session (create-session :stream stream
							  :sock sock
							  :uuid uuid
							  :destination destination))
			    (logger :debug "event myevents ~A ALL~%~%" uuid)
			    (format stream "event myevents ~A ALL~%~%" uuid)
			    (force-output stream)
			    (fs-read stream sock) ;; FIXME: check for errors.
			    (logger :debug "fs-setup-outgoing-call returning ~A" session)
			    session)
			  (error 'freeswitch-client-error :reason 
				 (format nil "Problem calling originate: ~A" response)))))
		  (error 'freeswitch-client-error :reason 
			 (format nil "Outgoing call authentication not accepted: ~A" response)))))
	  (error 'freeswitch-client-error :reason
		 (format nil "Outgoing call was unsucessful. No auth requested: ~A" response))))))

(defun list-valid-session-uuids ()
  (let ((out
         (trivial-shell:shell-command
          (format nil "/usr/local/freeswitch/bin/fs_cli -P ~A -H ~A -p ~A -x 'show channels'"
                  *fs-port* *fs-host* *fs-auth*))))
    (let ((lines (cl-ppcre:split "\\n" out)) (uuids nil))
      (dolist (l lines)
        (when (cl-ppcre:scan "^[0-9abcdef]{8}\-" l)
          (let ((d (cl-ppcre:split "\," l)))
            (push (elt d 0) uuids))))
      uuids)))

(defun kill-invalid-sessions ()
  (let ((uuids (list-valid-session-uuids)))
    (apply-sessions 
     #'(lambda (s)
	 (unless (member (uuid s) uuids :test 'equalp)
	   (logger :debug "KILLING INVALID: ~A~%" s)
	   (handler-case
	       (progn
		 (when (open-stream-p (fs-stream s))
		   (format (fs-stream s) "exit~%~%"))
		 (shutdown-session s :hangup? nil)
		 (ignore-errors (shutdown-connection s)))
	     (error (c)
	       (logger :debug "Problem killing invalid session ~A: ~A" (uuid s) c))))))))
     
(defun list-invalid-sessions ()
  (let ((uuids (list-valid-session-uuids)) (sessions nil))
    (apply-sessions #'(lambda (s)
			(unless (member (uuid s) uuids :test 'equalp)
			  (push s sessions))))
    sessions))
