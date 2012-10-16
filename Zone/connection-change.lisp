(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-CONNECT.
;;;
;;; This function is called when a zone Z becomes the child of some
;;; other zone P.  It is called with the client of P, Z, and P as
;;; arguments.
;;;
;;; The default method (on NULL) does nothing. 
;;;
;;; Client code will typically specialize at least on the type of the
;;; client.


(defgeneric notify-connect (client child parent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NOTIFY-DISCONNECT.
;;;
;;; This function is called when a zone Z is removed as the child of
;;; some other zone P.  It is called with the client of P, Z, and P as
;;; arguments.
;;;
;;; The default method (on NULL) does nothing. 
;;;
;;; Client code will typically specialize at least on the type of the
;;; client.

(defgeneric notify-disconnect (client child parent))

