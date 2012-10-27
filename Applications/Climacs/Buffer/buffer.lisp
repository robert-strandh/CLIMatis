(in-package #:climacs-buffer-buffer)

(defgeneric line-count (buffer))

(defgeneric object-count (buffer))

(defgeneric preceding-object-count (buffer line))

(defgeneric insert-line (buffer line location))

(defgeneric delete-line (buffer location))

(defgeneric hook-location (buffer hook))

(defgeneric newline-size (buffer))

(defclass buffer ()
  ((%newline-size :initform 1 :initarg :newsline-size :reader newline-size)))
