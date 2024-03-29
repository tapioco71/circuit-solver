;;;; -*- mode: common-lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; model.lisp

(in-package :circuit-solver)

;; Classes.

;;;
;;; model class definition
;;;

(defclass model-class (element-class)
  ((class
    :documentation "model CLASS: function, differential, lisp-function, lisp-differential or undefined."
    :initarg :class
    :initform "undefined"
    :accessor model-class-class
    :accessor element-class-class)
   (parameters-list
    :documentation "PARAMETERS-LIST of model input parameters."
    :initarg :parameters-list
    :initform nil
    :accessor model-class-parameters-list
    :accessor element-class-parameters-list)
   (function-name
    :documentation "hardcoded lisp function name."
    :initarg :function-name
    :initform ""
    :accessor model-class-function-name
    :accessor element-class-function-name)
   (external-function-name
    :documentation "external lisp function for model."
    :initarg :function
    :initform nil
    :accessor model-class-external-function-name
    :accessor element-class-external-function-name)
   (probes-list
    :documentation "list of probes to sample circuital quantities."
    :initarg :probes-list
    :initform nil
    :accessor model-class-probes-list
    :accessor element-class-probes-list)
   (states-list
    :documentation "model state vector."
    :initarg :states-list
    :initform nil
    :accessor model-class-states-list
    :accessor element-class-states-list)
   (value
    :documentation "model value."
    :initarg :value
    :initform 0d0
    :accessor model-class-value
    :accessor element-class-value)))

;; Functions.

(defun make-model (&rest parameters &key
                                      (id nil id-p)
                                      (name (symbol-name (gensym "model-")) name-p)
                                      (class "" class-p)
                                      (parameters-list nil parameters-list-p)
                                      (function-name "" function-name-p)
                                      (external-function-name "" external-function-name-p)
                                      (probes-list nil probes-list-p)
                                      (states-list nil states-list-p)
                                      (value nil value-p))
  (declare (ignorable parameters id name class parameters-list function-name external-function-name probes-list states-list value))
  (let ((object (make-instance 'model-class
                               :id id
                               :name name
                               :class class
                               :parameters-list parameters-list
                               :function-name function-name
                               :external-function-name external-function-name
                               :probes-list probes-list
                               :states-list states-list
                               :value value)))
    object))

;; Methods.


(defmethod sexpify ((object model-class))
  (let ((return-value (call-next-method object)))
    (unless (undefined-class-p object)
      (setq return-value (append return-value (list :class (model-class-class object)))))
    (when (model-class-function-name object)
      (setq return-value (append return-value (list :function-name (model-class-function-name object)))))
    (when (model-class-external-function-name object)
      (setq return-value (append return-value (list :external-function-name (model-class-external-function-name object)))))
    (when (model-class-parameters-list object)
      (setq return-value (append return-value (list :parameters-list (model-class-parameters-list object)))))
    (when (model-class-states-list object)
      (setq return-value (append return-value (list :states-list (model-class-states-list object)))))
    (when (model-class-probes-list object)
      (setq return-value (append return-value (mapcar #'sexpify (model-class-probes-list object)))))
    (when (has-value-p object)
      (setq return-value (append return-value (list :value (model-class-value object)))))
    return-value))

(defmethod undefined-class-p ((object model-class))
  (or (string-equal (model-class-class object)
                    "undefined")
      (string-equal (model-class-class object)
                    "")))

(defmethod simple-function-p ((object model-class))
  (string-equal (model-class-class object)
                "function"))

(defmethod differential-function-p ((object model-class))
  (string-equal (model-class-class object)
                "differential"))

(defmethod has-value-p ((object model-class))
  (null (model-class-value object)))

(defmethod rename-netlist-element ((object model-class) name &rest parameters &key
                                                                                (debug-mode nil)
                                                                                (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((return-value (call-next-method object
                                        name
                                        :debug-mode debug-mode
                                        :output output)))
    (when (element-class-probes-list return-value)
      (setf (element-class-probes-list return-value) (mapcar #'(lambda (x)
							         (rename-netlist-element x
                                                                                         name
                                                                                         :debug-mode debug-mode
                                                                                         :output output))
                                                             (element-class-probes-list return-value))))
    return-value))

(defmethod check-element-with-selectors ((object model-class) selectors)
  (let ((return-value (call-next-method object selectors)))
    (if (listp selectors)
	(dolist (selector selectors)
	  (setq return-value (or return-value
				  (funcall selector object))))
	(setq return-value (funcall selectors object)))
    return-value))

;; End model.lisp
