(in-package #:clim3-port)

(defclass port () ())

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

(defgeneric port-standard-key-processor (port handler-fun keycode modifier))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STANDARD-KEY-PROCESSOR.
;;;
;;; Take a function of one argument, and return a function that can be
;;; used as a handler function for a key-press or a key-release zone.
;;;
;;; The function passed as an argument is called with the standard
;;; interpretation of the key that was pressed, in the form of a list.
;;; The first element of the list is a character, and the remaining
;;; elements of the list are keywords that name modifier keys.
;;;
;;; The function returned calls the generic-function
;;; PORT-STANDARD-KEY-PROCESSOR with the port, the function given to
;;; this function as an argument, the keycode and the modifier. 

(defun standard-key-processor (handler-fun)
  (lambda (zone keycode modifiers)
    (port-standard-key-processor
     (clim3-zone:client zone) handler-fun keycode modifiers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros for traversing zones and areas
;;;

(defmacro with-zone (zone &body body)
  `(call-with-zone *port* (lambda () ,@body) ,zone))

(defmacro with-area ((hpos vpos width height) &body body)
  `(call-with-area *port* (lambda () ,@body) ,hpos  ,vpos ,width ,height))

(defmacro with-position ((hpos vpos width height) &body body)
  `(call-with-position *port* (lambda () ,@body) ,hpos  ,vpos ,width ,height))
