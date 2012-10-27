(in-package #:common-lisp-user)

(defpackage #:climacs-buffer-line
  (:use #:common-lisp)
  (:export
   #:line
   #:buffer-hook
   #:contents
   #:object-count
   #:map-over-cursors
   #:split
   #:join
   ))

(defpackage #:climacs-buffer-cursor
  (:use #:common-lisp #:climacs-buffer-line)
  (:export
   #:cursor-error
   #:beginning-of-line
   #:end-of-line
   #:cursor-unattached
   #:line
   #:beginning-of-line-p
   #:end-of-line-p
   #:move-forward
   #:move-backward
   #:object-at-cursor
   #:insert-object
   #:insert-sequence
   #:delete-object
   #:cursor
   #:unattached-cursor
   #:attached-cursor
   #:split-line
   #:join-lines
   ))

(defpackage #:climacs-buffer-buffer
  (:use #:common-lisp)
  (:export
   #:buffer
   #:line-count
   #:insert-line
   #:delete-line
   #:hook-location))

(defpackage #:climacs-buffer-simple-line
  (:use #:common-lisp #:climacs-buffer-cursor #:climacs-buffer-line)
  (:export
   #:simple-line
   #:simple-cursor
   ))

(defpackage #:climacs-buffer-simple-buffer
  (:use #:common-lisp #:climacs-buffer-buffer)
  (:export
   #:simple-buffer))
