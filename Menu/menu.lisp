(in-package #:clim3-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Menus and menu items

(defclass menu ()
  (;; When the menu is created, it has no zone.
   ;; A zone is created when the menu is realized
   (%zone :initform nil :accessor zone)
   ;; A list of menu items.  
   (%items :initarg :items :initform '() :accessor items)))

(defclass menu-item ()
  (;; This slot is required so that we can go up the chain
   ;; to the top menu, and remove its zone once an item has
   ;; been selected
   (%parent :initarg :parent :accessor parent)
   ;; When the menu item is realized, this zone is created
   ;; and added to the zone of the parent.
   (%zone :initform nil :accessor zone)))

;;; This menu item just creates a little zone that 
;;; divides up the items of the menu into groups.
(defclass divider (menu-item)
  ())

;;; The base class for menu items that show some text.
(defclass (text-item) (menu-item)
  (;; The text that the menu item should have.
   ;; Perhaps this needs to be a bit more general, so that
   ;; other things that text could be used.  We should also
   ;; allow for little images. 
   (%text :initarg :text :accessor text)))

;;; A menu item that when selected invokes a command.
;;; The command invocation can be partial, so that missing
;;; arguments are supplied through calls to ACCEPT. 
(defclass command-invoker (text-item)
  ((%command-invocation :intarg :command-invocation :reader command-invocation)))

;;; A menu item that when selected creates a submenu.
(defclass submenu-creator (text-item)
  ((%menu :initarg :menu :accessor menu)))

;;; A menu item that toggles some variable.
(defclass toggle-item (text-item)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 

