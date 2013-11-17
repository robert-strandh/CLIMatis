(in-package :clim3-color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Color.  
;;;
;;; The values of the fields are real numbers between 0 and 1.

(defclass clim3:color ()
  ((%red :initarg :red :reader clim3:red)
   (%green :initarg :green :reader clim3:green)
   (%blue :initarg :blue :reader clim3:blue)))

(defmethod initialize-instance :after ((color clim3:color)
				       &key
					 (red nil red-p)
					 (green nil green-p)
					 (blue nil blue-p))
  (when (null red-p) (error "Red color not supplied."))
  (check-type red (real 0 1))
  (when (null green-p) (error "Green color not supplied."))
  (check-type green (real 0 1))
  (when (null blue-p) (error "Blue color not supplied."))
  (check-type blue (real 0 1)))

(defun clim3:make-color (red green blue)
  (make-instance 'clim3:color :red red :green green :blue blue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Database of named colors. 
;;;
;;; We keep a database of commonly-used colors. 

(defparameter *colors* (make-hash-table :test #'equal))

(defun clim3:find-color (name)
  (check-type name string)
  ;; Do not return second value of gethash. 
  (let ((value (gethash name *colors*)))
    value))

(defun clim3:name-color (color name)
  (check-type color clim3:color)
  (check-type name string)
  (setf (gethash name *colors*) color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pixel.

(defclass clim3:pixel ()
  ((%color :initarg :color :reader clim3:color)
   (%opacity :initarg :opacity :reader clim3:opacity)))

(defmethod initialize-instance :after ((pixel clim3:pixel)
				       &key
					 (color nil color-p)
					 (opacity nil opacity-p))
  (when (null color-p) (error "No color supplied."))
  (check-type color clim3:color)
  (when (null opacity-p) (error "No opcacity supplied."))
  (check-type opacity (real 0 1)))

(defun make-pixel (color opacity)
  (make-instance 'clim3:pixel :color color :opacity opacity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Composing.

(defgeneric compose-over (x y))

(defmethod compose-over ((x clim3:color) (y clim3:color))
  x)

(defmethod compose-over ((x clim3:color) (y clim3:pixel))
  x)

(defmethod compose-over ((x clim3:pixel) (y clim3:color))
  (let ((a1 (clim3:opacity x))
	(a2 (- 1 (clim3:opacity x))))
    (clim3:make-color
     (+ (* (clim3:red x)   a1) (* (clim3:red y)   a2))
     (+ (* (clim3:green x) a1) (* (clim3:green y) a2))
     (+ (* (clim3:blue x)  a1) (* (clim3:blue y)  a2)))))

(defmethod compose-over ((x clim3:pixel) (y clim3:pixel))
  (let ((a1 (clim3:opacity x))
	(a2 (* (clim3:opacity y) (- 1 (clim3:opacity x)))))
    (make-pixel (clim3:make-color
		 (+ (* (clim3:red x)   a1) (* (clim3:red y)   a2))
		 (+ (* (clim3:green x) a1) (* (clim3:green y) a2))
		 (+ (* (clim3:blue x)  a1) (* (clim3:blue y)  a2)))
		(- 1 (* (- 1 (clim3:opacity x)) (- 1 (clim3:opacity y)))))))


    

