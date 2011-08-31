;; ASDF package description for cl-freeswitch              -*- Lisp -*-

(defpackage :cl-freeswitch-system (:use :cl :asdf))
(in-package :cl-freeswitch-system)

(defsystem cl-freeswitch
  :name "Freeswitch ESL Library"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.1"
  :description "Freeswitch ESL library for Lisp"
  :long-description "Freeswitch ESL library for Lisp."
  :depends-on (:usocket
	       :uuid
	       :cl-syslog
	       :py-configparser
	       :cl-ppcre
	       :hunchentoot
	       :trivial-timeout
	       :trivial-shell
	       :bordeaux-threads
	       :cl-smtp
	       :parse-number)
  :components ((:file "cl-freeswitch-package")
	       (:file "queue" :depends-on ("cl-freeswitch-package"))
	       (:file "globals" :depends-on ("queue"))
	       (:file "utilities" :depends-on ("globals"))
	       (:file "configuration" :depends-on ("globals"))
	       (:file "session" :depends-on ("configuration"))
	       (:file "freeswitch-client" :depends-on ("session"))
	       (:file "freeswitch-api" :depends-on ("freeswitch-client"))
	       (:file "notifier" :depends-on ("freeswitch-api"))
	       (:file "event" :depends-on ("notifier"))
	       (:file "listener" :depends-on ("event"))
	       (:file "call-queue" :depends-on ("listener"))
	       (:file "license" :depends-on ("call-queue"))
	       (:file "server" :depends-on ("license"))))
