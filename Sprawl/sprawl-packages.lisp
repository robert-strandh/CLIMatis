(in-package #:common-lisp-user)

(defpackage #:clim3-sprawl
  (:use #:common-lisp)
  (:export #:sprawl
	   #:min-size
	   #:size
	   #:max-size
	   #:combine-in-parallel
	   #:combine-in-series
	   #:sizes-in-series))