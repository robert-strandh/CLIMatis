(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function WIDTH.
;;;
;;; Return the current width of the zone.  
;;; 
;;; If the zone is the root zone and it is connected to some display
;;; server, then this value reflects the width of the corresponding
;;; top-level window.

(defgeneric clim3:width (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF WIDTH).
;;;
;;; Set the current width of the zone.
;;;
;;; Applications should never call this function.  It is used only by
;;; layout zones to set the dimensions of children, and by ports to
;;; set the size of the root zone to be equal to the size of the
;;; corresponding window.
;;;
;;; Applications can influence the dimensions of a zone, provided that
;;; it is the child of a zone that does not impose the dimensions,
;;; such as a bboard zone or a scroller zone, by changing the sprawls
;;; of the zone so as to give it different natural dimensions.

;;; FIXME: change to set-width

(defgeneric (setf clim3:width) (new-width zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function HEIGHT.
;;;
;;; Return the current height of the zone.
;;;
;;; If the zone is the root zone and it is connected to some display
;;; server, then this value reflects the height of the corresponding
;;; top-level window.

(defgeneric clim3:height (zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF HEIGHT).
;;;
;;; Set the current height of the zone.
;;;
;;;
;;; Applications should never call this function.  It is used only by
;;; layout zones to set the dimensions of children, and by ports to
;;; set the size of the root zone to be equal to the size of the
;;; corresponding window.
;;;
;;; Applications can influence the dimensions of a zone, provided that
;;; it is the child of a zone that does not impose the dimensions,
;;; such as a bboard zone or a scroller zone, by changing the sprawls
;;; of the zone so as to give it different natural dimensions.

;;; FIXME: change to set-height

(defgeneric (setf clim3:height) (new-height zone))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function IMPOSE-SIZE.
;;;
;;; This function is called in order to set the size of a zone.  

(defun clim3-ext:impose-size (zone width height)
  (setf (clim3:width zone) width)
  (setf (clim3:height zone) height))

(defclass clim3-ext:size-mixin ()
  ((%width :initform 0 :accessor clim3:width)
   (%height :initform 0 :accessor clim3:height)))

