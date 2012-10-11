(in-package #:common-lisp-user)

(asdf:defsystem #:camfer
  :depends-on (:cl-vectors)
  :components ((:file "packages")
	       (:file "mf" :depends-on ("packages"))
	       (:file "camfer" :depends-on ("packages" "mf"))))
    