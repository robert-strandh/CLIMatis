(cl:in-package #:clim3-application)

(defvar clim3:*application*)

(defclass clim3:application () ())

(defclass clim3:view () ())

(defgeneric clim3:current-view (application))

(defgeneric clim3:key-processor (application view input-context))

(defgeneric clim3:submit-keystroke (key-processor keystroke))

