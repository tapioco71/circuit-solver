;;;; -*- mode: common-lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; coupling.lisp

(in-package :circuit-solver)

;; Classes.

;;;
;;; inductances coupling element class
;;;

(defclass coupling-class (element-class)
  ((class
    :documentation "coupling CLASS: motional or transformational."
    :initarg :class
    :initform "undefined"
    :accessor coupling-class-class
    :accessor element-class-class)
   (elements-list
    :documentation "list of the inductances taking part in the coupling."
    :initarg :elements-list
    :initform ()
    :accessor elements-list
    :accessor coupling-class-elements-list
    :accessor element-class-elements-list)
   (model
    :documentation "model definition for k parameter."
    :initarg :model
    :initform nil
    :accessor model
    :accessor coupling-class-model
    :accessor element-class-model)
   (value
    :documentation "k parameter constant value."
    :initarg :value
    :initform 0d0
    :accessor value
    :accessor coupling-class-value
    :accessor element-class-value)))

;; Functions.

(defun make-coupling (&rest parameters &key
                                         (id nil id-p)
                                         (name (symbol-name (gensym "coupling-")) name-p)
                                         (class "" class-p)
                                         (elements-list nil elements-list-p)
                                         (model nil model-p)
                                         (value nil value-p))
  (declare (ignorable parameters id name class elements-list model value))
  (let ((object (make-instance 'coupling-class
                               :id id
                               :name name
                               :class class
                               :elements-list elements-list
                               :model model
                               :value value)))
    object))

;; Methods.

(defmethod sexpify ((object coupling-class))
  "Create a sexp for coupling element: :NAME name [ :ID id ] :ELEMENTS-LIST elements-list { :MODEL model | :VALUE value }."
  (let ((return-value (call-next-method object)))
    (when (coupling-class-elements-list object)
      (setq return-value (append return-value (list :elements-list (mapcar #'sexpify
                                                                           (coupling-class-elements-list object))))))
    (when (has-model-p object)
      (setq return-value (append return-value (list :model (sexpify (coupling-class-model object))))))
    (when (has-value-p object)
      (setq return-value (append return-value (list :value (coupling-class-value object)))))
    return-value))

(defmethod has-model-p ((object coupling-class))
  (typep object 'model-class))

(defmethod has-value-p ((object coupling-class))
  (null (coupling-class-value object)))

(defmethod rename-netlist-element ((object coupling-class) name &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((return-value (call-next-method object
                                        name
                                        :debug-mode debug-mode
                                        :output output)))
    (dolist (coupling-element (coupling-class-elements-list return-value))
      (setq coupling-element (rename-netlist-element coupling-element
                                                     name
                                                     :debug-mode debug-mode
                                                     :output output)))
    (when (coupling-class-model return-value)
      (setf (coupling-class-model return-value) (rename-netlist-element (coupling-class-model return-value)
                                                                        name
                                                                        :debug-mode debug-mode
                                                                        :output output)))
    return-value))

(defmethod element-with-node ((object coupling-class) node-name)
  (let ((return-value nil))
    (dolist (coupling-element (coupling-class-elements-list object))
      (when (position node-name
                      (passive-class-nodes-list object)
                      :test 'string-equal)
	(push coupling-element return-value)))
    return-value))

(defmethod check-element-with-selectors ((object coupling-class) selectors)
  (let ((return-value (call-next-method object selectors)))
    (if (listp selectors)
	(dolist (selector selectors)
	  (setq return-value (or return-value
				  (funcall selector object))))
	(setq return-value (funcall selectors object)))
    return-value))

;; End coupling.lisp
