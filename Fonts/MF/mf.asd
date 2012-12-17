(cl:in-package #:common-lisp-user)

(asdf:defsystem #:mf
  :depends-on (:cl-vectors)
  :components ((:file "packages")
	       (:file "mf" :depends-on ("packages"))))
