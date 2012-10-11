(in-package #:clim3-input-method)

(defgeneric manage-event (input-method event))

(defgeneric gesture-available-p (input-method))

(defgeneric next-gesture (input-method))

(defclass input-method ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modifier mask.  Do this better.  

(defconstant +shift-modifier-mask+ 1)
(defconstant +control-modifier-mask+ 4)
(defconstant +meta-modifier-mask+ 8)

(defun choose-interpretation (input-method modifier-state interpretations)
  (cond ((= modifier-state 0)
	 (code-char (aref interpretations 0)))
	((= modifier-state +shift-modifier-mask+)
	 (code-char (aref interpretations 1)))
	((and (= modifier-state +control-modifier-mask+)
	      (<= (char-code #\a) (aref interpretations 0) (char-code #\z)))
	 (code-char (- (aref interpretations 0) (char-code #\a))))
	((and (= modifier-state
		 (logior +shift-modifier-mask+ +control-modifier-mask+))
	      (<= (char-code #\@) (aref interpretations 1) (char-code #\_)))
	 (code-char (- (aref interpretations 1) (char-code #\@))))))

(defun enqueue-gesture (input-method gesture)
  (setf (gesture-queue input-method)
	(append (gesture-queue input-method)
		(list gesture))))

(defgeneric accumulate-gesture (input-method gesture))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; An input method that immediately 
;;; pushed a character on the gesture queue

(defclass immediate-accumulate-mixin () ())
  
(defmethod accumulate-gesture ((input-method immediate-accumulate-mixin)
			       gesture)
  (enqueue-gesture input-method gesture))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; An input method that converts
;;; key-press events to characters.

(defclass character-input-method (input-method
				  immediate-accumulate-mixin)
  ((%gesture-queue :initform '() :accessor gesture-queue)))

(defmethod gesture-available-p ((input-method character-input-method))
  (not (null (gesture-queue input-method))))

(defmethod next-gesture ((input-method character-input-method))
  (pop (gesture-queue input-method)))

(defclass modifier-tracking-input-method-mixin ()
  ((%modifier-state :initform 0 :accessor modifier-state)))

(defmethod manage-event ((input-method character-input-method)
			 (event key-press-event))
  (cond ((member (aref (interpretations event) 0)
	      '(#xffe1 #xffe2 #xffe3 #xffe4
		#xffe7 #xffe8 #xffe9 #xffea))
	 ;; These are the modifier keys. 
	 nil)
	((member (aref (interpretations event) 0)
		 '(#xff0a #xff0d))
	 (accumulate-gesture input-method #\Newline))
	(t
	 (accumulate-gesture
	  input-method
	  (choose-interpretation input-method
				 (modifier-state event)
				 (interpretations event))))))

(defmethod manage-event ((input-method character-input-method)
			 (event key-release-event))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ...

(defclass immediate-character-input-method (character-input-method
					    immediate-accumulate-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; An input method that accumulates
;;; characters and converts a sequence
;;; of such characters into a different
;;; sequence of characters.

(defclass multi-keystroke-input-method-mixin ()
  ((%combinations :initarg :combinations :initform '() :reader combinations)
   (%prefixes :initform '() :accessor prefixes)))

(defmethod accumulate-gesture
    ((input-method multi-keystroke-input-method-mixin)
     gesture)
  (setf (prefixes input-method)
	(append (prefixes input-method) (list gesture)))
  (let ((entry (find (prefixes input-method) (combinations input-method)
		     :test #'equal :key #'car)))
    (if (not (null entry))
	(progn (loop for char in (cdr entry)
		     do (enqueue-gesture input-method char))
	       (setf (prefixes input-method) '()))
	(unless (member-if (lambda (entry)
			     (and (< (length (prefixes input-method))
				     (length (car entry)))
				  (= (length (prefixes input-method))
				     (mismatch (car entry) (prefixes input-method)))))
			   (combinations input-method))
	  (loop for char in (prefixes input-method)
		do (enqueue-gesture input-method char))
	  (setf (prefixes input-method) '())))))

(defclass latin-1-prefix-input-method (character-input-method
				       multi-keystroke-input-method-mixin)
  ()
  (:default-initargs
      :combinations
      '(
	((#\' #\a) . (#\á))
	((#\' #\o) . (#\ó))
	((#\' #\u) . (#\ú))
	((#\' #\e) . (#\é))
	((#\' #\i) . (#\í))
	((#\' #\y) . (#\ý))
	((#\` #\a) . (#\à))
	((#\` #\o) . (#\ò))
	((#\` #\u) . (#\ù))
	((#\` #\e) . (#\è))
	((#\` #\i) . (#\ì))
	((#\^ #\a) . (#\â))
	((#\^ #\o) . (#\ô))
	((#\^ #\u) . (#\û))
	((#\^ #\e) . (#\ê))
	((#\^ #\i) . (#\î))
	((#\~ #\a) . (#\ã))
	((#\~ #\o) . (#\õ))
	((#\~ #\u) . (#\µ))
	((#\~ #\n) . (#\ñ))
	((#\~ #\c) . (#\ç))
	((#\" #\a) . (#\ä))
	((#\" #\o) . (#\ö))
	((#\" #\u) . (#\ü))
	((#\" #\e) . (#\ë))
	((#\" #\i) . (#\ï))
	((#\" #\y) . (#\ÿ))
	((#\/ #\a) . (#\å))
	((#\/ #\o) . (#\ø))	
	((#\/ #\/) . (#\°))
	)))
