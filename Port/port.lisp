(in-package #:clim3-port)

(defclass clim3:port () ())

(defparameter clim3:*port* nil)

;;; FIXME: add a comment

(defmethod (setf clim3-ext:parent) :after
    ((new-parent clim3:port) (zone clim3:zone))
  (clim3-ext:set-clients zone new-parent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MAKE-PORT.
;;;
;;; The argument to this function is a keyword symbol indicating
;;; what kind of port to make.

(defgeneric clim3:make-port (display-server))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CONNECT.
;;;
;;; Take a zone and a port and connect the zone to the port. 
;;;
;;; Signal an error if the zone is already connected. 

(defgeneric clim3:connect (zone port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function DISCONNECT.
;;;
;;; Take a zone and a port and disconnect the zone from the port.
;;;
;;; Signal an error if the zone is not connected, or if it is
;;; connected to a port other than this one.

(defgeneric clim3:disconnect (zone port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function EVENT-LOOP.

(defgeneric clim3:event-loop (port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function REDISPLAY. 

(defgeneric clim3-ext:repaint (port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function TEXT-ASCENT.
;;;
;;; Take a port, a text style and a string, and return the ascent of
;;; the text on that port, i.e., the largest number of pixels above
;;; the baseline of any of the characters in the string.

(defgeneric clim3:text-ascent (port text-style string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function TEXT-DESCENT.
;;;
;;; Take a port, a text style and a string, and return the descent of
;;; the text on that port, i.e., the largest number of pixels below
;;; the baseline of any of the characters in the string.

(defgeneric clim3:text-descent (port text-style string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function TEXT-WIDTH.
;;;
;;; Take a port, a text style and a string, and return the width of
;;; the text on that port, i.e., the number of horizontal pixels
;;; required to draw it.

(defgeneric clim3:text-width (port text-style string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function TEXT-STYLE-ASCENT.
;;;
;;; Take a port and a text style, and return the ascent of the text
;;; style on that port i.e., the largest number of pixels above the
;;; baseline of any of the characters in the text style.

(defgeneric clim3:text-style-ascent (port text-style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function TEXT-STYLE-DESCENT.
;;;
;;; Take a port and a text style, and return the descent of the text
;;; style on that port i.e., the largest number of pixels below the
;;; baseline of any of the characters in the text style.

(defgeneric clim3:text-style-descent (port text-style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function STANDARD-KEY-DECODER.
;;;

(defgeneric clim3-ext:standard-key-decoder (port keycode modifier))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STANDARD-KEY-DECODER.

(defun clim3:standard-key-decoder (key-code modifiers)
  (clim3-ext:standard-key-decoder clim3:*port* key-code modifiers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions for traversing zones and areas

(defgeneric clim3-ext:call-with-zone (port thunk zone))

(defgeneric clim3-ext:call-with-area (port thunk hpos vpos width heigt))

(defgeneric clim3-ext:call-with-position (port thunk hpos vpos))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros for traversing zones and areas
;;;

(defmacro clim3:with-zone (zone &body body)
  `(clim3-ext:call-with-zone clim3:*port* (lambda () ,@body) ,zone))

(defmacro clim3:with-area ((hpos vpos width height) &body body)
  `(clim3-ext:call-with-area clim3:*port* (lambda () ,@body) ,hpos  ,vpos ,width ,height))

(defmacro clim3:with-position ((hpos vpos) &body body)
  `(clim3-ext:call-with-position clim3:*port* (lambda () ,@body) ,hpos  ,vpos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Key handler

(defgeneric clim3:handle-key-press (key-handler key-code modifiers))
(defgeneric clim3:handle-key-release (key-handler key-code modifiers))

(defclass clim3:key-handler () ())

(defclass clim3:null-key-handler (clim3:key-handler) ())

(defmethod clim3:handle-key-press
    ((key-handler clim3:null-key-handler) key-code modifiers)
  (declare (ignore key-code modifiers))
  nil)

(defmethod clim3:handle-key-release
    ((key-handler clim3:null-key-handler) key-code modifiers)
  (declare (ignore key-code modifiers))
  nil)

(defparameter clim3:*null-key-handler* (make-instance 'clim3:null-key-handler))

(defparameter clim3:*key-handler* clim3:*null-key-handler*)

(defclass read-keystroke-key-handler (clim3:key-handler)
  ((%receiver :initarg :receiver :reader receiver)))

(defmethod clim3:handle-key-press
    ((key-handler read-keystroke-key-handler) key-code modifiers)
  (funcall (receiver key-handler)
	   (clim3:standard-key-decoder key-code modifiers)))

(defmethod clim3:handle-key-release
    ((key-handler read-keystroke-key-handler) key-code modifiers)
  (declare (ignore key-code modifiers))
  nil)

(defun clim3:read-keystroke ()
  (let ((clim3:*key-handler*
	  (make-instance 'read-keystroke-key-handler
	     :receiver (lambda (keystroke)
			 (return-from clim3:read-keystroke keystroke)))))
    (clim3:event-loop clim3:*port*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function STANDARD-BUTTON-DECODER.
;;;

(defgeneric clim3-ext:standard-button-decoder (port button-code modifier))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STANDARD-BUTTON-DECODER.

(defun clim3:standard-button-decoder (button-code modifiers)
  (clim3-ext:standard-button-decoder clim3:*port* button-code modifiers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Button handler

(defgeneric clim3:handle-button-press (button-handler button-code modifiers))
(defgeneric clim3:handle-button-release (button-handler button-code modifiers))

(defclass clim3:button-handler () ())

(defclass clim3:null-button-handler (clim3:button-handler) ())

(defmethod clim3:handle-button-press
    ((button-handler clim3:null-button-handler) button-code modifiers)
  (declare (ignore button-code modifiers))
  nil)

(defmethod clim3:handle-button-release
    ((button-handler clim3:null-button-handler) button-code modifiers)
  (declare (ignore button-code modifiers))
  nil)

(defparameter clim3:*null-button-handler* (make-instance 'clim3:null-button-handler))

(defparameter clim3:*button-handler* clim3:*null-button-handler*)

