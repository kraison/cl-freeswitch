;; name failure-func choices min max tries timeout terminator menu-file
;; invalid-file &optional regex
(def-operator-menu welcome #'too-many-invalid
  (("1" #'interstitial)
   ("2" #'connect-customer-service)
   ("*" #'end-call))
  1 1 3 15000 "A" (prompts-dir "P77990.wav") (prompts-dir "P70401.wav") "[12*]")


;; name failure-func choices-fn static-choices tries timeout terminator menu-file
;; invalid-file
(def-dynamic-menu welcome
  #'too-many-invalid
  #'choices-fn
  (("0" #'connect-customer-service) ("*" #'end-call))
  3 15000 "A" (prompts-dir "P77990.wav") (prompts-dir "P70401.wav"))
