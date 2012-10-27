(in-package #:climacs-buffer-buffer)

(defgeneric line-count (buffer))

(defgeneric insert-line (buffer line location))

(defgeneric delete-line (buffer location))

(defgeneric hook-location (buffer hook))

(defclass buffer () ())