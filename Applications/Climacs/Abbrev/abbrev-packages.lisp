(cl:in-package #:common-lisp-user)

(defpackage #:abbrev
  (:use #:common-lisp)
  (:export
   #:expander
   #:constituent-p
   #:trigger-p
   #:expand
   #:simple-expander
   #:capitalize-mixin
   #:simple-capitalizing-expander
   ))