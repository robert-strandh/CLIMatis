(in-package #:climacs-buffer-cursor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition CURSOR-ERROR.
;;;
;;; This is the base class of all cursor conditions. 

(define-condition cursor-error (error)
  ((%cursor :initarg cursor :reader cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition BEGINNING-OF-LINE.
;;;
;;; This condition is signaled when an attempt is made to position the
;;; cursor before the beginning of the line.

(define-condition beginning-of-line (cursor-error) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition BEGINNING-OF-LINE.
;;;
;;; This condition is signaled when an attempt is made to position the
;;; cursor beyond the end of the line.

(define-condition end-of-line (cursor-error) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Condition CURSOR-UNATTACHED.
;;;
;;; This condition is signaled when an operation is attempted that
;;; requires the cursor to be attached to a line, but the cursor given
;;; to the operation is unattached.

(define-condition cursor-unattached (cursor-error) ())
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function LOCATION.
;;; 
;;; Return the location of the cursor within the line as a
;;; non-negative integer between 0 and L (inclusive), where L is the
;;; object count of the line.
;;;
;;; Each specialized subclass of CURSOR must have a method on this
;;; generic function.

(defgeneric location (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF LOCATION).
;;;
;;; Set the location of the cursor.  The new location must be a
;;; non-negative integer between 0 and L (inclusive), where L is the
;;; object count of the line.  
;;;
;;; Each specialized subclass of CURSOR must have a method on this
;;; generic function.

(defgeneric (setf location) (new-location cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function LINE.
;;;
;;; Given a cursor, return the line to which the cursor is attached,
;;; or NIL if the cursor is currently not attached to a line. 

(defgeneric line (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF LINE).
;;;
;;; Attach the cursor to a line.

(defgeneric (setf line) (new-line cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BEGINNING-OF-LINE-P.
;;;
;;; Returns true if the cusor is located at the beginning of the line,
;;; and false otherwise.  If the cursor is unattached, the condition
;;; CURSOR-UNATTACHED is signaled.
;;;
;;; A default method, specialized for CURSOR is supplied (see below).
;;; It checks whether the location of the cursor is 0.  Subclasses of
;;; CURSOR therefore do not have to supply a method on this generic
;;; function, but they can supply one if there is a more efficient
;;; method for accomplishing the task on that particular subclass. 

(defgeneric beginning-of-line-p (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function END-OF-LINE-P.
;;;
;;; Returns true if the cusor is located at the end of the line, and
;;; false otherwise.  If the cursor is unattached, the condition
;;; CURSOR-UNATTACHED is signaled.
;;;
;;; A default method, specialized for CURSOR is supplied (see below).
;;; It checks whether the location of the cursor is equal to the
;;; object count of the line to which the cursor belongs.  Subclasses
;;; of CURSOR therefore do not have to supply a method on this generic
;;; function, but they can supply one if there is a more efficient
;;; method for accomplishing the task on that particular subclass.


(defgeneric end-of-line-p (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MOVE-FORWARD.
;;;
;;; Move the cursor forward by the count given.  The default count
;;; is 1.
;;;
;;; If the count given is not a non-negative integer, then a
;;; condition of type TYPE-ERROR is signaled.
;;;
;;; If the cursor is currently unattached, a condition of type
;;; CURSOR-UNATTACHED is signaled.
;;;
;;; If the count is such that the cursor would move beyond the end of
;;; the line, then a condition of type END-OF-LINE is signaled.
;;;
;;; A default method, specialized for CURSOR is supplied (see below).
;;; It uses LOCATION and (SETF LOCATION) to modify the location of the
;;; cursor.
;;;
;;; A :BEFORE method, specialized for CURSOR is supplied (see below).
;;; It checks that the resulting location of the cursor is less than
;;; or equal to the object count of the line, and if not, signals a
;;; condition of type END-OF-LINE.

(defgeneric move-forward (cursor &optional count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MOVE-BACKWARD.
;;;
;;; Move the cursor backward by the count given.  The default count
;;; is 1.
;;;
;;; If the count given is not a non-negative integer, then a
;;; condition of type TYPE-ERROR is signaled.
;;;
;;; If the cursor is currently unattached, a condition of type
;;; CURSOR-UNATTACHED is signaled.
;;;
;;; If the count is such that the cursor would move to a location
;;; before the beginning of the line, then a condition of type
;;; BEGINNING-OF-LINE is signaled.
;;;
;;; A default method, specialized for CURSOR is supplied (see below).
;;; It uses LOCATION and (SETF LOCATION) to modify the location of the
;;; cursor.
;;;
;;; A :BEFORE method, specialized for CURSOR is supplied (see below).
;;; It checks that the resulting location of the cursor is
;;; non-negative, and if not, signals a condition of type
;;; BEGINNING-OF-LINE.

(defgeneric move-backward (cursor &optional count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function OBJECT-AT-CURSOR.

(defgeneric object-at-cursor (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function INSERT-OBJECT.
;;;

(defgeneric insert-object (cursor object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function INSERT-SEQUENCE.
;;;

(defgeneric insert-sequence (cursor sequence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function DELETE-OBJECT.
;;;

(defgeneric delete-object (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function LOCATION.
;;;
;;; Return the location of the cursor within the line as a
;;; non-negative integer between 0 and L, where L is the number of
;;; objects of the line.  A value of 0 means the beginning of the
;;; line, and a value of L means the end of the line.
;;;
;;; Some types of lines do not have a direct representation of the
;;; location of the cursor corresponding to what this function
;;; returns, and for such lines it could be inefficient to compute the
;;; position.  For that reason, it is recommended to use the function
;;; MOVE-FORWARD and MOVE-BACKWARD whenever possible, as opposed to
;;; manipulating the location of the cursor directly.


(defgeneric location (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF LOCATION).

(defgeneric (setf location) (new-location cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Protocol Class CURSOR.
;;;
;;; This is the base class of all cursors.  This class should not be
;;; instantiated directly.

(defclass cursor () ())
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class UNATTACHED-CURSOR.
;;;
;;; Cursors that are unattached are instances of this class. 

(defclass unattached-cursor (cursor) ())
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function LINE.
;;;
;;; Given cursor that is attached to some line, return the line to
;;; which the cursor is attached.

(defgeneric line (attached-cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF LINE).
;;;

(defgeneric (setf line) (new-line cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ATTACHED-CURSOR.
;;;
;;; This is the base class for all attached cursors.  Each line type
;;; will define its own subclass(es) of this class. 

(defclass attached-cursor (cursor)
  ((%line :initarg :line :initform nil :reader line :writer set-line)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default methods.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on (SETF LINE) specialized for a NULL line.
;;;
;;; Client code detatches a cursor from a line by calling (SETF LINE)
;;; with a NULL line.  The primary method changes the class of the
;;; cursor to UNATTACHED-CURSOR.  
;;;
;;; Client code must define a :BEFORE method on this function that
;;; removes the cursor from the collection of cursors of its line.
;;; Such a :BEFORE method must be specialized for a NULL line and for
;;; the particular cursor type that client code defines. 

(defmethod (setf line) ((new-line null) (cursor attached-cursor))
  (change-class cursor 'unattached-cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on (SETF LINE) specialized for a UNATTACHED-CURSOR.
;;;
;;; Client code attaches a cursor to a line by calling (SETF LINE)
;;; with an unattached cursor and a subclass of LINE. 
;;;
;;; Client code must define a :BEFORE method on this function that
;;; changes the class of the cursor to the special cursor type defined
;;; by client code, and that inserts the cursor appropriately into the
;;; collection of cursors of that line.

(defmethod (setf line) (new-line (cursor unattached-cursor))
  (set-line new-line cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Error checking :BEFORE method on MOVE-FORWARD.

(defmethod move-forward :before ((cursor cursor) &optional (count 1))
  (when (> (+ (location cursor) count) (object-count (line cursor)))
    (error 'end-of-line)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Error checking :BEFORE method on MOVE-FORWARD.

(defmethod move-backward :before ((cursor cursor) &optional (count 1))
  (when (< (- (location cursor) count) 0)
    (error 'beginning-of-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Error checking :BEFORE method on OBJECT-AT-CURSOR.

(defmethod object-at-cursor :before ((cursor attached-cursor))
  (when (end-of-line-p cursor)
    (error 'end-of-line :cursor cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Error checking method on OBJECT-AT-CURSOR for UNATTACHED-CURSOR.

(defmethod object-at-cursor ((cursor unattached-cursor))
  (error 'cursor-unattached :cursor cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Error checking method on INSERT-OBJECT for UNATTACHED-CURSOR.

(defmethod insert-object ((cursor unattached-cursor) object)
  (declare (ignore object))
  (error 'cursor-unattached :cursor cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Error checking :BEFORE method on DELETE-OBJECT.

(defmethod delete-object :before ((cursor attached-cursor))
  (when (end-of-line-p cursor)
    (error 'end-of-line :cursor cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Error checking method on DELETE-OBJECT for UNATTACHED-CURSOR.

(defmethod delete-object ((cursor unattached-cursor))
  (error 'cursor-unattached :cursor cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default method on BEGINNING-OF-LINE-P.

(defmethod begining-of-line-p ((cursor cursor))
  (zerop (location cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default method on END-OF-LINE-P.

(defmethod end-of-line-p ((cursor cursor))
  (= (location cursor) (object-count (line cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default method on BEGINNING-OF-LINE.

(defmethod beginning-of-line ((cursor cursor))
  (setf (location cursor) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default method on END-OF-LINE.

(defmethod end-of-line ((cursor cursor))
  (setf (location cursor) (object-count (line cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default method on MOVE-FORWARD.

(defmethod move-forward ((cursor cursor) &optional (count 1))
  (incf (location cursor) count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default method on MOVE-BACKWARD.

(defmethod move-backward ((cursor cursor) &optional (count 1))
  (decf (location cursor) count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default method on INSERT-SEQUENCE.

(defmethod insert-sequence ((cursor cursor) (sequence sequence))
  (map nil (lambda (object) (insert-object cursor object)) sequence))


