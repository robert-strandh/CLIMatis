(cl:in-package #:common-lisp-user)

(defpackage #:2-3-tree
  (:use #:common-lisp)
  (:shadow #:delete #:find)
  (:export
   #:find
   #:insert
   #:delete
   #:root
   #:left
   #:middle
   #:right
   #:item
   #:size
   #:tree
   #:leaf
   #:2-node
   #:3-node
   #:leaf-class
   #:2-node-class
   #:3-node-class
   ))
