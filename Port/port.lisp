(in-package #:clim3-port)

(defclass port () ())

(defparameter *new-port* nil)

;;; FIXME: add a comment

(defmethod (setf clim3-zone:parent) :after ((new-parent port) (zone clim3-zone:zone))
  (clim3-zone:set-clients zone new-parent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MAKE-PORT.
;;;
;;; The argument to this function is a keyword symbol indicating
;;; what kind of port to make.

(defgeneric make-port (display-server))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CONNECT.
;;;
;;; Take a zone and a port and connect the zone to the port. 
;;;
;;; Signal an error if the zone is already connected. 

(defgeneric connect (zone port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function DISCONNECT.
;;;
;;; Take a zone and a port and disconnect the zone from the port.
;;;
;;; Signal an error if the zone is not connected, or if it is
;;; connected to a port other than this one.

(defgeneric disconnect (zone port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function EVENT-LOOP.

(defgeneric event-loop (port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function TEXT-ASCENT.
;;;
;;; Take a port, a text style and a string, and return the ascent of
;;; the text on that port, i.e., the largest number of pixels above
;;; the baseline of any of the characters in the string.

(defgeneric text-ascent (port text-style string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function TEXT-DESCENT.
;;;
;;; Take a port, a text style and a string, and return the descent of
;;; the text on that port, i.e., the largest number of pixels below
;;; the baseline of any of the characters in the string.

(defgeneric text-descent (port text-style string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function TEXT-WIDTH.
;;;
;;; Take a port, a text style and a string, and return the width of
;;; the text on that port, i.e., the number of horizontal pixels
;;; required to draw it.

(defgeneric text-width (port text-style string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function TEXT-STYLE-ASCENT.
;;;
;;; Take a port and a text style, and return the ascent of the text
;;; style on that port i.e., the largest number of pixels above the
;;; baseline of any of the characters in the text style.

(defgeneric text-style-ascent (port text-style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function TEXT-STYLE-DESCENT.
;;;
;;; Take a port and a text style, and return the descent of the text
;;; style on that port i.e., the largest number of pixels below the
;;; baseline of any of the characters in the text style.

(defgeneric text-style-descent (port text-style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function PORT-STANDARD-KEY-PROCESSOR.
;;;

(defgeneric port-standard-key-processor (port keycode modifier))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STANDARD-KEY-PROCESSOR.

(defun standard-key-processor (key-code modifiers)
  (port-standard-key-processor *new-port* key-code modifiers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions for traversing zones and areas

(defgeneric call-with-zone (port thunk zone))

(defgeneric call-with-area (port thunk hpos vpos width heigt))

(defgeneric call-with-position (port thunk hpos vpos))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros for traversing zones and areas
;;;

(defmacro with-zone (zone &body body)
  `(call-with-zone *new-port* (lambda () ,@body) ,zone))

(defmacro with-area ((hpos vpos width height) &body body)
  `(call-with-area *new-port* (lambda () ,@body) ,hpos  ,vpos ,width ,height))

(defmacro with-position ((hpos vpos) &body body)
  `(call-with-position *new-port* (lambda () ,@body) ,hpos  ,vpos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Key handler

(defgeneric handle-key-press (key-handler key-code modifiers))
(defgeneric handle-key-release (key-handler key-code modifiers))

(defclass key-handler () ())

(defclass null-key-handler (key-handler) ())

(defmethod handle-key-press
    ((key-handler null-key-handler) key-code modifiers)
  (declare (ignore key-code modifiers))
  nil)

(defmethod handle-key-release
    ((key-handler null-key-handler) key-code modifiers)
  (declare (ignore key-code modifiers))
  nil)

(defparameter *null-key-handler* (make-instance 'null-key-handler))

(defparameter *key-handler* *null-key-handler*)

(defclass read-keystroke-key-handler (key-handler)
  ((%receiver :initarg :receiver :reader receiver)))

(defmethod handle-key-press
    ((key-handler read-keystroke-key-handler) key-code modifiers)
  (funcall (receiver key-handler)
	   (standard-key-processor key-code modifiers)))

(defmethod handle-key-release
    ((key-handler read-keystroke-key-handler) key-code modifiers)
  (declare (ignore key-code modifiers))
  nil)

(defun read-keystroke ()
  (let ((*key-handler*
	  (make-instance 'read-keystroke-key-handler
	     :receiver (lambda (keystroke)
			 (return-from read-keystroke keystroke)))))
    (event-loop *new-port*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Button handler

(defgeneric handle-button-press (button-handler button-code modifiers))
(defgeneric handle-button-release (button-handler button-code modifiers))

(defclass button-handler () ())

(defclass null-button-handler (button-handler) ())

(defmethod handle-button-press
    ((button-handler null-button-handler) button-code modifiers)
  (declare (ignore button-code modifiers))
  nil)

(defmethod handle-button-release
    ((button-handler null-button-handler) button-code modifiers)
  (declare (ignore button-code modifiers))
  nil)

(defparameter *null-button-handler* (make-instance 'null-button-handler))

(defparameter *button-handler* *null-button-handler*)

