(in-package :clim3-color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Color.  
;;;
;;; The values of the fields are real numbers between 0 and 1.

(defclass color ()
  ((%red :initarg :red :reader red)
   (%green :initarg :green :reader green)
   (%blue :initarg :blue :reader blue)))

(defmethod initialize-instance :after ((color color)
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

(defun make-color (red green blue)
  (make-instance 'color :red red :green green :blue blue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Database of named colors. 
;;;
;;; We keep a database of commonly-used colors. 

(defparameter *colors* (make-hash-table :test #'equal))

(defun find-color (name)
  (check-type name string)
  ;; Do not return second value of gethash. 
  (let ((value (gethash name *colors*)))
    value))

(defun name-color (color name)
  (check-type color color)
  (check-type name string)
  (setf (gethash name *colors*) color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pixel.

(defclass pixel ()
  ((%color :initarg :color :reader color)
   (%opacity :initarg :opacity :reader opacity)))

(defmethod initialize-instance :after ((pixel pixel)
				       &key
					 (color nil color-p)
					 (opacity nil opacity-p))
  (when (null color-p) (error "No color supplied."))
  (check-type color color)
  (when (null opacity-p) (error "No opcacity supplied."))
  (check-type opacity (real 0 1)))

(defun make-pixel (color opacity)
  (make-instance 'pixel :color color :opacity opacity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Composing.

(defgeneric compose-over (x y))

(defmethod compose-over ((x color) (y color))
  x)

(defmethod compose-over ((x color) (y pixel))
  x)

(defmethod compose-over ((x pixel) (y color))
  (let ((a1 (opacity x))
	(a2 (- 1 (opacity x))))
    (make-color (+ (* (red x)   a1) (* (red y)   a2))
		(+ (* (green x) a1) (* (green y) a2))
		(+ (* (blue x)  a1) (* (blue y)  a2)))))

(defmethod compose-over ((x pixel) (y pixel))
  (let ((a1 (opacity x))
	(a2 (* (opacity y) (- 1 (opacity x)))))
    (make-pixel (make-color (+ (* (red x)   a1) (* (red y)   a2))
			    (+ (* (green x) a1) (* (green y) a2))
			    (+ (* (blue x)  a1) (* (blue y)  a2)))
		(- 1 (* (- 1 (opacity x)) (- 1 (opacity y)))))))


    

