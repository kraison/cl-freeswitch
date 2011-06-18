;; This software is Copyright (c) Chatsubo.net, LLC, May 1, 2011.
;; Chatsubo.net, LLC grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:cl-freeswitch)

(defparameter *debug* t)

(defconstant +new-connection-wait-time+ 1)
(defvar *listener-thread* nil)
(defvar *stop-listener* nil)
(defvar *thread-pool-size* 30)
(defvar *id* 1)
(defvar *port* 8084)
(defvar *fs-cli* "/usr/local/freeswitch/bin/fs_cli")
(defvar *fs-host* "127.0.0.1")
(defvar *fs-port* 8021)
(defvar *fs-auth* "cluecon")
(defvar *configuration-file* "cl-freeswitch.cfg")
(defvar *configuration* nil)
(defvar *pstn-gateway* "localhost")
(defvar *prompts-dir* "prompts")

(defvar *thread-list-lock* (make-recursive-lock))
(defvar *thread-list* nil)

(defconstant +buflen+ 64)

(defvar *extension-table* (make-hash-table :test 'equalp :synchronized t))
(defvar *sessions* (make-hash-table :test 'equalp :synchronized t))

(defvar *session-history-recording* nil)
(defvar *session* nil)
(defvar *input* nil)

(defvar *mail-relay* "localhost")
(defvar *admin-emails* (list "admin@domain.com"))
