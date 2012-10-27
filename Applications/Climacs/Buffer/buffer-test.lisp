(in-package #:climacs-buffer-test)

(defun test ()
  (let ((line (make-instance 'climacs-buffer-simple-line:simple-line)))
    (assert (zerop (climacs-buffer-line:object-count line)))))