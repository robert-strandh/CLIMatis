(defparameter *test-buffer-contents*
  '("Sven Duvas fader var sergeant,"
    "avdankad, arm och gra."
    "Var med ar 88 ren,"
    "och var ren gammal da."
    "Nu bodde pa sin torva han,"
    "och fick sitt brod av den."
    "Han hade kring sig tio barn,"
    "och yngst av dem sin Sven."))

(defparameter *test-buffer-contents2*
  '("Sven Duvas fader var sergeant"))

(defun string-to-words-and-spaces (string start)
  (if (= start (length string))
      '()
      (if (eql (char string start) #\Space)
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
  (clim3-layout:hbox*
   (clim3-layout:hbox
    (mapcar (lambda (item)
	      (if (numberp item)
		  (clim3-layout:hframe* 7 7 7)
		  (clim3-text:text item nil (clim3-color:make-color 0.0 0.0 0.0))))
	    (string-to-words-and-spaces string 0)))
   (clim3-layout:hframe* 0 0 nil)))

(defun edit-zone (height)
  (let ((parent-of-lines
	  (clim3-layout:bboard* 
	   (clim3-layout:vbox
	    (loop for string in *test-buffer-contents*
		  ;; Put the words and spaces of a line into a hbox.
		  collect (line-from-string string)
		  ;; Put something very elastic after each line of text.
		  collect (clim3-layout:hframe* 0 0 nil))))))
    ;; Return both the root of the hierarchy, and the parent of the
    ;; lines, so that we can replace its children as we edit.
    (values (clim3-layout:vframe*
	     height height height
	     (clim3-layout:pile*
	      parent-of-lines
	      (clim3-graphics:opaque (clim3-color:make-color 0.95 0.95 0.95))))
	    parent-of-lines)))

(defun info-zone (height)
  (clim3-layout:vframe*
   height height height
   (clim3-layout:pile*
    (clim3-layout:hbox*
     (clim3-layout:hframe* 30 30 30)
     (clim3-text:text "Climacs" nil (clim3-color:make-color 0.0 0.0 0.0))
     (clim3-layout:hframe* 0 0 nil))
    (clim3-graphics:opaque (clim3-color:make-color 0.8 0.8 0.8)))))
   
(defun minibuffer-zone (height)
  (clim3-layout:vframe*
   height height height 
   (clim3-layout:pile*
    ;; This one should really be a scroller.
    (clim3-layout:bboard*
      ;; The zone containing the text
     (clim3-layout:hbox* (clim3-layout:hframe* 0 0 nil)))
    (clim3-graphics:opaque (clim3-color:make-color 0.95 0.95 0.95)))))


(defun editor (width height)
  (let ((parent-of-lines nil))
    (clim3-layout:hframe*
     width width width
     (clim3-layout:vframe*
      height height height
      (clim3-layout:pile* 
       (clim3-input:key (clim3-port:standard-key-processor #'print)
			(lambda (&rest rest) (declare (ignore rest)) nil))
       (clim3-layout:vbox*
	(multiple-value-bind (root p-of-l)
	    (edit-zone (- height 60))
	  (setf parent-of-lines p-of-l)
	  root)
	(info-zone 30)
	(minibuffer-zone 30)))))))