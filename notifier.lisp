(in-package #:cl-freeswitch)

(defun notify (message)
  (when *mail-relay*
    (make-thread
     #'(lambda ()
	 (let ((message (string-right-trim
                         '(#\Space #\Tab #\Newline #\Linefeed #\Return)
                         message)))
	   (map nil #'(lambda (email)
			(logger :debug "Notifying ~A of ~A" email message)
			(cl-smtp:send-email *mail-relay*
					    "raison@chatsubo.net"
					    email
					    "Message from the IO"
					    (format nil "~A" message)))
		*admin-emails*)))
     :name "notifier-thread")))
