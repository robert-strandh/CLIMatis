(in-package #:common-lisp-user)

(asdf:defsystem #:camfer
  :depends-on (:mf)
  :components ((:file "packages")
	       (:file "camfer" :depends-on ("packages"))))
