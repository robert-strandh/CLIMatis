(cl:in-package #:abbrev)

(defclass expander () ())

(defgeneric constituent-p (expander char))

(defgeneric trigger-p (expander char))

(defgeneric expand (expander string))

(defclass simple-expander (expander)
  ((%abbrevs :initarg :abbrevs :accessor abbrevs)))

(defmethod constituent-p ((expander simple-expander) char)
  (alphanumericp char))

(defmethod trigger-p ((expander simple-expander) char)
  (not (alphanumericp char)))

(defmethod expand ((expander simple-expander) string)
  (cdr (find string (abbrevs expander) :test #'equal :key #'car)))

(defclass capitalize-mixin () ())

(defmethod expand :around
    ((expander capitalize-mixin) string)
  (cond ((every #'upper-case-p string)
	 (let ((expansion (call-next-method expander
					    (string-downcase string))))
	   (unless (null expansion)
	     (string-upcase expansion))))
	((upper-case-p (char string 0))
	 (let ((expansion (call-next-method expander
					    (string-downcase string))))
	   (unless (null expansion)
	     (string-capitalize expansion))))
	(t
	 (call-next-method))))

(defclass simple-capitalizing-expander
    (simple-expander capitalize-mixin)
  ())
