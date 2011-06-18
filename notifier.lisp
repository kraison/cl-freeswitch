;; This software is Copyright (c) Chatsubo.net, LLC, May 1, 2011.
;; Chatsubo.net, LLC grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:cl-freeswitch)

(defun notify (message)
  (when *mail-relay*
    (make-thread 
     #'(lambda ()
	 (let ((message (string-right-trim '(#\Space #\Tab #\Newline #\Linefeed #\Return)
					   message)))
	   (map nil #'(lambda (email)
			(logger :debug "Notifying ~A of ~A" email message)
			(cl-smtp:send-email *mail-relay*
					    email
					    email
					    "Message from cl-freeswitch"
					    (format nil "~A" message)))
		*admin-emails*)))
     :name "notifier-thread")))
