(in-package #:common-lisp-user)

(defpackage #:climacs-buffer-cursor
  (:use #:common-lisp)
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
   #:delete-object
   #:cursor
   #:unattached-cursor
   #:attached-cursor
   ))

(defpackage #:climacs-buffer-line
  (:use #:common-lisp)
  (:shadowing-import-from #:climacs-buffer-cursor #:line)
  (:export
   #:line
   #:buffer-hook
   #:contents
   #:object-count
   ))

(defpackage #:climacs-buffer-simple-line
  (:use #:common-lisp #:climacs-buffer-cursor #:climacs-buffer-line)
  (:export
   #:simple-line
   #:simple-cursor
   ))
