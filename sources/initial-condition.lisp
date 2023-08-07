;;;; -*- mode: common-lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; initial-condition.lisp

(in-package :circuit-solver)

;; Classes.

;;;
;;; initial condition specifier
;;;

(defclass initial-condition-class (element-class)
  ((target-name
    :documentation "name for element initial conditions: branch (current), node (voltage) or model (quantity)."
    :initarg :target-name
    :initform nil
    :accessor initial-condition-class-target-name
    :accessor element-class-target-name)
   (value
    :documentation "value for initial condition."
    :initarg :value
    :initform nil
    :accessor initial-condition-class-value
    :accessor element-class-value)))

;; Functions.

(defun make-initial-condition (&rest parameters &key
                                                  (id nil id-p)
                                                  (name (symbol-name (gensym "initial-condition-")) name-p)
                                                  (target-name nil target-name-p)
                                                  (value nil value-p))
  (declare (ignorable parameters id name target-name value))
  (let ((object (make-instance 'initial-condition-class
                               :id id
                               :name name
                               :target-name target-name
                               :value value)))
    object))

;; Methods.

(defmethod sexpify ((object initial-condition-class))
  (let ((return-value (call-next-method object)))
    (when (initial-condition-class-target-name object)
      (setq return-value (append return-value (list :target-name (initial-condition-class-target-name object)))))
    (when (initial-condition-class-value object)
      (setq return-value (append return-value (list :value (initial-condition-class-value object)))))
    return-value))

;; End initial-condition.lisp
