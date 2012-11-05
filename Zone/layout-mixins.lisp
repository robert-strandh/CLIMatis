(in-package #:clim3-zone)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-HSPRAWL-CHANGES-HSPRAWL-MIXIN

(defclass changing-child-hsprawl-changes-hsprawl-mixin () ())

(defmethod notify-child-hsprawl-changed
    ((child zone) (parent changing-child-hsprawl-changes-hsprawl-mixin))
  (setf (hsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-VSPRAWL-CHANGES-VSPRAWL-MIXIN

(defclass changing-child-vsprawl-changes-vsprawl-mixin () ())

(defmethod notify-child-vsprawl-changed
    ((child zone) (parent changing-child-vsprawl-changes-vsprawl-mixin))
  (setf (vsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-HSPRAWL-CHANGES-NOTHING-MIXIN

(defclass changing-child-hsprawl-changes-nothing-mixin () ())

(defmethod notify-child-hsprawl-changed
    ((child zone) (parent changing-child-hsprawl-changes-nothing-mixin))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-VSPRAWL-CHANGES-NOTHING-MIXIN

(defclass changing-child-vsprawl-changes-nothing-mixin () ())

(defmethod notify-child-vsprawl-changed
    ((child zone) (parent changing-child-vsprawl-changes-nothing-mixin))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILDREN-CHANGES-HSPRAWL-MIXIN

(defclass changing-children-changes-hsprawl-mixin () ())

(defmethod (setf children) :after
  (new-children (zone changing-children-changes-hsprawl-mixin))
  (declare (ignore new-children))
  (setf (hsprawl zone) nil))
  
(defmethod (setf child) :after
  (new-child (zone changing-children-changes-hsprawl-mixin))
  (declare (ignore new-child))
  (setf (hsprawl zone) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILDREN-CHANGES-VSPRAWL-MIXIN

(defclass changing-children-changes-vsprawl-mixin () ())

(defmethod (setf children) :after
  (new-children (zone changing-children-changes-vsprawl-mixin))
  (declare (ignore new-children))
  (setf (vsprawl zone) nil))
  
(defmethod (setf child) :after
  (new-child (zone changing-children-changes-vsprawl-mixin))
  (declare (ignore new-child))
  (setf (vsprawl zone) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILDREN-CHANGES-BOTH-SPRAWLS-MIXIN

(defclass changing-children-changes-both-sprawls-mixin () ())

(defmethod (setf children) :after
  (new-children (zone changing-children-changes-vsprawl-mixin))
  (declare (ignore new-children))
  (setf (hsprawl zone) nil)
  (setf (vsprawl zone) nil))
  
(defmethod (setf child) :after
  (new-child (zone changing-children-changes-vsprawl-mixin))
  (declare (ignore new-child))
  (setf (hsprawl zone) nil)
  (setf (vsprawl zone) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILDREN-CHANGES-NOTHING-MIXIN

(defclass changing-children-changes-nothing-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-POSITION-NOT-ALLOWED-MIXIN

(defclass changing-child-position-not-allowed-mixin () ())

(defmethod notify-child-position-changed
    ((child zone) (parent changing-child-position-not-allowed-mixin))
  (error "attempt to change the position of a child"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-POSITION-CHANGES-HSPRAWL-MIXIN

(defclass changing-child-position-changes-hsprawl-mixin () ())

(defmethod notify-child-position-changed
    ((child zone) (parent changing-child-position-changes-hsprawl-mixin))
  (setf (hsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-POSITION-CHANGES-VSPRAWL-MIXIN

(defclass changing-child-position-changes-vsprawl-mixin () ())

(defmethod notify-child-position-changed
    ((child zone) (parent changing-child-position-changes-vsprawl-mixin))
  (setf (vsprawl parent) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHANGING-CHILD-POSITION-CHANGES-BOTH-SPRAWLS-MIXIN

(defclass changing-child-position-changes-both-sprawls-mixin () ())

(defmethod notify-child-position-changed
    ((child zone) (parent changing-child-position-changes-both-sprawls-mixin))
  (setf (hsprawl parent) nil)
  (setf (vsprawl parent) nil))

