(cl:in-package #:common-lisp-user)

(defpackage #:clueless
  (:use #:common-lisp)
  (:shadow #:inspect)
  (:export #:inspect))

