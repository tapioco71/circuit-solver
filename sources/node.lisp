;;;; -*- mode: common-lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; node.lisp

(in-package :circuit-solver)

;; Classes.

;;;
;;; node element class
;;;

(defclass node-class (element-class)
  ((class
    :documentation "node CLASS: reference, voltage-current, active-reactive-power or undefined."
    :initarg :class
    :initform ""
    :accessor node-class-class
    :accessor element-class-class)
   (state
    :documentation "node STATE: undiscovered, discovered and processed."
    :initarg :state
    :initform nil
    :accessor node-class-state
    :accessor element-class-state)
   (number
    :documentation "node NUMBER: 0 = reference, n = other nodes."
    :initarg :number
    :initform -1
    :accessor node-class-number
    :accessor element-class-number)))

;; Functions.

(defun make-node (&rest parameters &key
                                     (id nil id-p)
                                     (name (symbol-name (gensym "node-")) name-p)
                                     (class "" class-p)
                                     (state nil state-p)
                                     (number nil number-p))
  (declare (ignorable parameters id name class state number))
  (let ((object (make-instance 'node-class
                               :id id
                               :name name
                               :class class
                               :state state
                               :number number)))
    object))

;; Methods.

(defmethod sexpify ((object node-class))
  "Create a sexp for node element: :NAME name [ :ID id ] :CLASS element-class [ :STATE state ] [ :NUMBER number ]."
  (let ((return-value (call-next-method object)))
    (unless (undefined-class-p object)
      (setq return-value (append return-value (list :class (node-class-class object)))))
    (unless (eql (node-class-state object) nil)
      (setq return-value (append return-value (list :state (node-class-state object)))))
    (unless (eql (node-class-number object) -1)
      (setq return-value (append return-value (list :number (node-class-number object)))))
    return-value))

(defmethod undefined-class-p ((object node-class))
  (or (string-equal (node-class-class object)
                    "undefined")
      (string-equal (node-class-class object)
                    "")))

(defmethod reference-class-node-p ((object node-class))
  (or (string-equal (node-class-class object)
                    "reference")
      (string-equal (node-class-class object)
                    "gnd")
      (string-equal (node-class-class object)
                    "0")))

(defmethod element-with-node ((object node-class) node-name)
  (when (string-equal node-name
                      (element-class-name object))
    object))

(defmethod check-element-with-selectors ((object node-class) selectors)
  (let ((return-value (call-next-method object selectors)))
    (if (listp selectors)
	(dolist (selector selectors)
	  (setq return-value (or return-value
				  (funcall selector object))))
	(setq return-value (funcall selectors object)))
    return-value))

;; End node.lisp
