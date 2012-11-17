(cl:in-package #:common-lisp-user)

(defpackage #:clim3-meter
  (:use #:common-lisp)
  (:export #:meter
	   #:reset
	   #:with-meter
	   #:report))

