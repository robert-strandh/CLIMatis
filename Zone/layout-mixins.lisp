(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class HORIZONTALLY-VERY-ELASTIC-MIXIN.

(defclass clim3-ext:horizontally-very-elastic-mixin ()
  ()
  (:default-initargs :hsprawl (clim3-sprawl:sprawl 0 0 nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VERTICALLY-VERY-ELASTIC-MIXIN.

(defclass clim3-ext:vertically-very-elastic-mixin ()
  ()
  (:default-initargs :vsprawl (clim3-sprawl:sprawl 0 0 nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-HSPRAWL-CHANGES-HSPRAWL-MIXIN

(defclass clim3-ext:changing-child-hsprawl-changes-hsprawl-mixin () ())

(defmethod clim3-ext:notify-child-hsprawl-changed
    ((child clim3:zone) (parent clim3-ext:changing-child-hsprawl-changes-hsprawl-mixin))
  (setf (clim3:hsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-VSPRAWL-CHANGES-VSPRAWL-MIXIN

(defclass clim3-ext:changing-child-vsprawl-changes-vsprawl-mixin () ())

(defmethod clim3-ext:notify-child-vsprawl-changed
    ((child clim3:zone) (parent clim3-ext:changing-child-vsprawl-changes-vsprawl-mixin))
  (setf (clim3:vsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-HSPRAWL-CHANGES-NOTHING-MIXIN

(defclass clim3-ext:changing-child-hsprawl-changes-nothing-mixin () ())

(defmethod clim3-ext:notify-child-hsprawl-changed
    ((child clim3:zone) (parent clim3-ext:changing-child-hsprawl-changes-nothing-mixin))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-VSPRAWL-CHANGES-NOTHING-MIXIN

(defclass clim3-ext:changing-child-vsprawl-changes-nothing-mixin () ())

(defmethod clim3-ext:notify-child-vsprawl-changed
    ((child clim3:zone) (parent clim3-ext:changing-child-vsprawl-changes-nothing-mixin))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILDREN-CHANGES-HSPRAWL-MIXIN

(defclass clim3-ext:changing-children-changes-hsprawl-mixin () ())

(defmethod (setf clim3:children) :after
  (new-children (zone clim3-ext:changing-children-changes-hsprawl-mixin))
  (declare (ignore new-children))
  (setf (clim3:hsprawl zone) nil))
  
(defmethod (setf clim3:child) :after
  (new-child (zone clim3-ext:changing-children-changes-hsprawl-mixin))
  (declare (ignore new-child))
  (setf (clim3:hsprawl zone) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILDREN-CHANGES-VSPRAWL-MIXIN

(defclass clim3-ext:changing-children-changes-vsprawl-mixin () ())

(defmethod (setf clim3:children) :after
  (new-children (zone clim3-ext:changing-children-changes-vsprawl-mixin))
  (declare (ignore new-children))
  (setf (clim3:vsprawl zone) nil))
  
(defmethod (setf clim3:child) :after
  (new-child (zone clim3-ext:changing-children-changes-vsprawl-mixin))
  (declare (ignore new-child))
  (setf (clim3:vsprawl zone) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILDREN-CHANGES-BOTH-SPRAWLS-MIXIN

(defclass clim3-ext:changing-children-changes-both-sprawls-mixin () ())

(defmethod (setf clim3:children) :after
  (new-children (zone clim3-ext:changing-children-changes-both-sprawls-mixin))
  (declare (ignore new-children))
  (setf (clim3:hsprawl zone) nil)
  (setf (clim3:vsprawl zone) nil))
  
(defmethod (setf clim3:child) :after
  (new-child (zone clim3-ext:changing-children-changes-both-sprawls-mixin))
  (declare (ignore new-child))
  (setf (clim3:hsprawl zone) nil)
  (setf (clim3:vsprawl zone) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILDREN-CHANGES-NOTHING-MIXIN

(defclass clim3-ext:changing-children-changes-nothing-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-POSITION-NOT-ALLOWED-MIXIN

(defclass clim3-ext:changing-child-position-not-allowed-mixin () ())

(defmethod clim3-ext:notify-child-position-changed
    ((child clim3:zone)
     (parent clim3-ext:changing-child-position-not-allowed-mixin))
  (error "attempt to change the position of a child"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-POSITION-CHANGES-NOTHING-MIXIN

(defclass clim3-ext:changing-child-position-changes-nothing-mixin () ())

(defmethod clim3-ext:notify-child-position-changed
    ((child clim3:zone)
     (parent clim3-ext:changing-child-position-changes-nothing-mixin))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-POSITION-CHANGES-HSPRAWL-MIXIN

(defclass clim3-ext:changing-child-position-changes-hsprawl-mixin () ())

(defmethod clim3-ext:notify-child-position-changed
    ((child clim3:zone)
     (parent clim3-ext:changing-child-position-changes-hsprawl-mixin))
  (setf (clim3:hsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-POSITION-CHANGES-VSPRAWL-MIXIN

(defclass clim3-ext:changing-child-position-changes-vsprawl-mixin () ())

(defmethod clim3-ext:notify-child-position-changed
    ((child clim3:zone)
     (parent clim3-ext:changing-child-position-changes-vsprawl-mixin))
  (setf (clim3:vsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-POSITION-CHANGES-BOTH-SPRAWLS-MIXIN

(defclass clim3-ext:changing-child-position-changes-both-sprawls-mixin () ())

(defmethod clim3-ext:notify-child-position-changed
    ((child clim3:zone)
     (parent clim3-ext:changing-child-position-changes-both-sprawls-mixin))
  (setf (clim3:hsprawl parent) nil)
  (setf (clim3:vsprawl parent) nil))

