(in-package #:clim3-port)

(defclass port () ())

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
