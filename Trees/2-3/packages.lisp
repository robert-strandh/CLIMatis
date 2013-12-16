(cl:in-package #:common-lisp-user)

(defpackage #:2-3-tree
  (:use #:common-lisp)
  (:shadow #:delete #:find)
  (:export
   #:find
   #:insert
   #:delete
   ))
