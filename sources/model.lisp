;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; model.lisp
;;;;
;;;; Copyright (c) 2020 Angelo Rossi
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

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

;;
;; Methods.
;;

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

(defmethod rename-netlist-element ((object model-class) name &rest parameters &key (debug-mode nil) (output *standard-output*))
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
