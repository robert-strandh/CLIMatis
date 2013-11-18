(defpackage #:test-editor
  (:use #:common-lisp))

(in-package #:test-editor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; View

(defclass editor-view ()
  ((%cursor :initform (cons 0 0) :accessor cursor)
   (%buffer :initform (vector (vector)) :accessor buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commands

(defun editor-error (format-string &rest arguments)
  (apply #'format *error-output* format-string arguments)
  (error "get me out of here"))

(defun beginning-of-line-p (view)
  (zerop (cdr (cursor view))))

(defun end-of-line-p (view)
  (with-accessors ((buffer buffer) (cursor cursor)) view
    (= (length (aref buffer (car cursor))) (cdr cursor))))

(defun first-line-p (view)
  (zerop (car (cursor view))))

(defun last-line-p (view)
  (= (car (cursor view)) (1- (length (buffer view)))))

(defun beginning-of-buffer-p (view)
  (and (first-line-p view) (beginning-of-line-p view)))

(defun end-of-buffer-p (view)
  (and (last-line-p view) (end-of-line-p view)))

(defun move-to-beginning-of-line (view)
  (setf (cdr (cursor view)) 0))

(defun move-to-end-of-line (view)
  (with-accessors ((buffer buffer) (cursor cursor)) view
    (setf (cdr cursor) (length (aref buffer (car cursor))))))

(defun cursor-forward (view)
  (with-accessors ((cursor cursor)) view
    (if (end-of-line-p view)
	(if (last-line-p view)
	    (editor-error "At the end of the buffer.")
	    (progn (incf (car cursor))
		   (move-to-beginning-of-line view)))
	(incf (cdr cursor)))))

(defun cursor-backward (view)
  (with-accessors ((buffer buffer) (cursor cursor)) view
    (if (beginning-of-line-p view)
	(if (first-line-p view)
	    (editor-error "At the beginning of the buffer.")
	    (progn (decf (car cursor))
		   (move-to-end-of-line view)))
	(decf (cdr cursor)))))

(defun insert-object (view object)
  (with-accessors ((buffer buffer) (cursor cursor)) view
    (setf (aref buffer (car cursor))
	  (concatenate 'vector
		       (subseq (aref buffer (car cursor)) 0 (cdr cursor))
		       (list object)
		       (subseq (aref buffer (car cursor)) (cdr cursor))))
    (incf (cdr cursor))))

(defun delete-object-or-merge-line (view)
  (with-accessors ((buffer buffer) (cursor cursor)) view
    (if (end-of-line-p view)
	(if (last-line-p view)
	    (editor-error "At the end of the buffer.")
	    (setf buffer
		  (concatenate 'vector
			       (subseq buffer 0 (car cursor))
			       (list (concatenate 'vector
						  (aref buffer (car cursor))
						  (aref buffer (1+ (car cursor)))))
			       (subseq buffer (+ (car cursor) 2)))))
	(setf (aref buffer (car cursor))
	      (concatenate 'vector
			   (subseq (aref buffer (car cursor)) 0 (cdr cursor))
			   (subseq (aref buffer (car cursor)) (1+ (cdr cursor))))))))

(defun erase-object-or-merge-line (view)
  (cursor-backward view)
  (delete-object-or-merge-line view))

(defun split-line (view)
  (with-accessors ((buffer buffer) (cursor cursor)) view
    (let ((line (aref buffer (car cursor))))
      (setf buffer
	    (concatenate 'vector
			 (subseq buffer 0 (car cursor))
			 (list (subseq line 0 (cdr cursor))
			       (subseq line (cdr cursor)))
			 (subseq buffer (1+ (car cursor))))))
    (incf (car cursor))
    (move-to-beginning-of-line view)))

(defun merge-line (view)
  (with-accessors ((buffer buffer) (cursor cursor)) view
    (if (last-line-p view)
	(editor-error "On last line.")
	(setf buffer
	      (concatenate 'vector
			   (subseq buffer 0 (car cursor))
			   (list (concatenate 'vector
					      (aref buffer (car cursor))
					      (aref buffer (1+ (car cursor)))))
			   (subseq buffer (+ (car cursor) 2)))))))

(defun kill-to-end-of-line (view)
  (with-accessors ((buffer buffer) (cursor cursor)) view
    (if (end-of-line-p view)
	(if (last-line-p view)
	    (editor-error "At the end of the buffer.")
	    (merge-line view))
	(setf (aref buffer (car cursor))
	      (subseq (aref buffer (car cursor)) 0 (cdr cursor))))))

(defun exit (view)
  (declare (ignore view))
  (throw 'end nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Keystroke processor

(defgeneric process-keystroke (keystroke-processor keystroke))

(defclass keystroke-processor () ())

(defclass emacs-like-keystroke-processor ()
  ((%view :initarg :view :reader view)
   (%command-table :initarg :command-table :reader command-table)
   (%numeric-args :initform nil :accessor numeric-args)
   (%keystrokes-so-far :initform '() :accessor keystrokes-so-far))) 

(defun reset-keystroke-processor (keystroke-processor)
  (setf (numeric-args keystroke-processor) nil)
  (setf (keystrokes-so-far keystroke-processor) '()))
  

(defun abort-and-reset (keystroke-processor)
  (editor-error "Quit.")
  (reset-keystroke-processor keystroke-processor))

(defparameter *fundametal-table*
  '((((#\x :control) (#\c :control)) . exit)
    (((#\f :control))                . cursor-forward)
    (((#\b :control))                . cursor-backward)
    (((#\d :control))                . delete-object-or-merge-line)
    (((#\h :control))                . erase-object-or-merge-line)
    (((#\a :control))                . move-to-beginning-of-line)
    (((#\e :control))                . move-to-end-of-line)
    (((#\k :control))                . kill-to-end-of-line)
    (((#\Return))                    . split-line)))

(defun prefix-p (partial-sentence sentence)
  (and (<= (length partial-sentence) (length sentence))
       (every #'equal partial-sentence sentence)))

(defmethod process-keystroke
    ((keystroke-processor emacs-like-keystroke-processor) keystroke)
  (if (equal keystroke '(#\g :control))
      (abort-and-reset keystroke-processor)
      (with-accessors ((view view)
		       (command-table command-table)
		       (keystrokes-so-far keystrokes-so-far))
	  keystroke-processor
	(setf keystrokes-so-far (append keystrokes-so-far (list keystroke)))
	(let ((entries (remove-if-not (lambda (entry)
					(prefix-p keystrokes-so-far (car entry)))
				      command-table)))
	  (cond  ((null entries)
		  (if (and (null (cdr keystrokes-so-far))
			   (null (cdr keystroke)))
		      ;; We have a single character.  
		      (progn (reset-keystroke-processor keystroke-processor)
			     (insert-object view (car keystroke)))
		      (progn (editor-error "No command for keystokes.")
			     (reset-keystroke-processor keystroke-processor))))
		 ((equal keystrokes-so-far (caar entries))
		  ;; We found a perfect match.
		  (reset-keystroke-processor keystroke-processor)
		  (handler-case (funcall (cdar entries) view)
		    (error () nil)))
		 (t
		  nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GUI		       

(defun string-to-words-and-spaces (string start)
  (if (= start (length string))
      '()
      (if (eql (aref string start) #\Space)
	  (let ((first-non-blank (position #\Space string :test-not #'eql :start start)))
	    (when (null first-non-blank)
	      (setf first-non-blank (length string)))
	    (cons (- first-non-blank start)
		  (string-to-words-and-spaces string first-non-blank)))
	  (let ((first-blank (position #\Space string :start start)))
	    (when (null first-blank)
	      (setf first-blank (length string)))
	    (cons (subseq string start first-blank)
		  (string-to-words-and-spaces string first-blank))))))

(defun line-from-string (string)
  (clim3:hbox*
   (clim3:hbox
    (mapcar (lambda (item)
	      (if (numberp item)
		  (clim3:hframe* 7 7 7)
		  (clim3-text:text
		   (coerce item 'string)
		   (clim3-text-style:text-style :camfer :sans :roman 12)
		   (clim3:make-color 0.0 0.0 0.0))))
	    (string-to-words-and-spaces string 0)))
   (clim3:hframe* 0 0 nil)))

(defun edit-zone (height lines)
  (clim3:vframe*
   height height height
   (clim3:pile*
    (clim3:bboard* lines)
    (clim3:opaque (clim3:make-color 0.95 0.95 0.95)))))

(defun info-zone (height)
  (clim3:vframe*
   height height height
   (clim3:pile*
    (clim3:hbox*
     (clim3:hframe* 30 30 30)
     (clim3-text:text
      "Climacs"
      (clim3-text-style:text-style :camfer :sans :roman 12)
      (clim3:make-color 0.0 0.0 0.0))
     (clim3:hframe* 0 0 nil))
    (clim3:opaque (clim3:make-color 0.8 0.8 0.8)))))
   
(defun minibuffer-zone (height)
  (clim3:vframe*
   height height height 
   (clim3:pile*
    ;; This one should really be a scroller.
    (clim3:bboard*
      ;; The zone containing the text
     (clim3:hbox* (clim3:hframe* 0 0 nil)))
    (clim3:opaque (clim3:make-color 0.95 0.95 0.95)))))


(defun stuff-buffer-text (buffer vbox)
  (setf (clim3:children vbox)
	(loop for line across buffer
	      ;; Put the words and spaces of a line into a hbox.
	      collect (line-from-string line)
	      ;; Make some space between lines
	      collect (clim3:vframe* 5 5 nil))))

(defparameter *fun* nil)

(defun find-cursor (view)
  (with-accessors ((buffer buffer) (cursor cursor)) view
    (let* ((prefix (subseq (aref buffer (car cursor)) 0 (cdr cursor)))
	   (words-and-spaces (string-to-words-and-spaces prefix 0)))
      (cond ((= (cdr cursor) (length (aref buffer (car cursor))))
	     (values (* 2 (car cursor)) nil nil))
	    ((vectorp (car (last words-and-spaces)))
	     (values (* 2 (car cursor))
		     (1- (length words-and-spaces))
		     (coerce (car (last words-and-spaces)) 'string)))
	    (t
	     (values (* 2 (car cursor))
		     (length words-and-spaces)
		     nil))))))

(defun editor-zones (width height)
  (let* ((lines (clim3:vbox*))
	 (view (make-instance 'editor-view))
	 (keystroke-processor
	   (make-instance 'emacs-like-keystroke-processor
			  :view view
			  :command-table *fundametal-table*)))
    (clim3:hframe*
     width width width
     (clim3:vframe*
      height height height
      (clim3:pile* 
       (clim3-input:key
	(clim3:standard-key-processor
	 (setf *fun*
	       (lambda (key)
		 (process-keystroke keystroke-processor key)
		 (stuff-buffer-text (buffer view) lines))))
	(lambda (&rest rest) (declare (ignore rest)) nil))
       (clim3:vbox*
	(edit-zone (- height 60) lines)
	(info-zone 30)
	(minibuffer-zone 30)))))))

(defun editor (width height)
  (let ((port (clim3:make-port :clx-framebuffer))
	(root (editor-zones width height)))
    (clim3:connect root port)
    (catch 'end 
      (clim3:event-loop port))
    (clim3:disconnect root port)))

