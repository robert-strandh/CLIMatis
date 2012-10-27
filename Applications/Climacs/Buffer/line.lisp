(in-package #:climacs-buffer-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BUFFER-HOOK.
;;;
;;; Return the buffer hook of the line, or NIL if the line is
;;; currently not attached to any buffer.
;;; 
;;; We require the buffer to have some kind of attachment point for
;;; each line, so that from a line we can reach the buffer itself.  We
;;; call this attachment point a BUFFER HOOK.  If the line is not
;;; attached to any buffer, then this function returns NIL.
;;;
;;; The exact nature of this object will depend on the type of the
;;; buffer.  For example, if the buffer is represented as a vector of
;;; lines, then the buffer-hook might be a CONS of the buffer and the
;;; index of the line in the vector.  Or, if the buffer is represented
;;; as a tree of lines then the buffer-hook might be a node of the
;;; tree.

(defgeneric buffer-hook (line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF BUFFER-HOOK).
;;;
;;; Set the buffer-hook of the line, indicating that it is attached to a
;;; buffer.

(defgeneric (setf buffer-hook) (new-buffer-hook line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function OBJECT-COUNT.

(defgeneric object-count (line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CONTENTS.
;;;

(defgeneric contents (line &key start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MAP-OVER-CURSORS.
;;;

(defgeneric map-over-cursors (function line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SPLIT.
;;;

(defgeneric split (line location))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function JOIN
;;;

(defgeneric join (line1 line2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LINE.

(defclass line ()
  ((%buffer-hook :initarg :buffer-hook :initform nil :accessor buffer-hook)))

