;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; sources.lisp
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
;;; current/voltage source class definition
;;;

(defclass source-class (element-class)
  ((class
    :documentation "source CLASS: current, voltage or undefined."
    :initarg :class
    :initform "undefined"
    :accessor source-class-class
    :accessor element-class-class)
   (nodes-list
    :documentation "source connection nodes to the circuit."
    :initarg :nodes-list
    :initform ()
    :accessor source-class-nodes-list
    :accessor element-class-nodes-list)
   (model
    :documentation "source model."
    :initarg :model
    :initform nil
    :accessor source-class-model
    :accessor element-class-model)
   (value
    :documentation "source constant value e.g. 10 A or 1000 V."
    :initarg :value
    :initform nil
    :accessor source-class-value
    :accessor element-class-value)))

;; Functions.

(defun make-source (&rest parameters &key
                                       (id nil id-p)
                                       (name (symbol-name (gensym "source-")) name-p)
                                       (class "" class-p)
                                       (nodes-list nil nodes-list-p)
                                       (model nil model-p)
                                       (value 0d0 value-p))
  (declare (ignorable id name parameters class nodes-list model value))
  (let ((object (make-instance 'source-class
                               :id id
                               :name name
                               :class class
                               :nodes-list nodes-list
                               :model model
                               :value value)))
    object))

;; Methods.

(defmethod sexpify ((object source-class))
  "Create a sexp for source element: :NAME name [ :ID id ] :CLASS element-class :NODES-LIST nodes-list { :MODEL model | :VALUE value }."
  (let ((return-value (call-next-method object)))
    (unless (undefined-class-p object)
      (setq return-value (append return-value (list :class (source-class-class object)))))
    (when (source-class-nodes-list object)
      (setq return-value (append return-value (list :nodes-list (source-class-nodes-list object)))))
    (when (has-model-p object)
      (setq return-value (append return-value (list :model (sexpify (source-class-model object))))))
    (when (has-value-p object)
      (setq return-value (append return-value (list :value (source-class-value object)))))
    return-value))

(defmethod undefined-class-p ((object source-class))
  (or (string-equal (source-class-class object)
                    "undefined")
      (string-equal (source-class-class object)
                    "")))

(defmethod voltage-source-class-p ((object source-class))
  (string-equal (source-class-class object)
                "voltage-source"))

(defmethod current-source-class-p ((object source-class))
  (string-equal (source-class-class object)
                "current-source"))

(defmethod has-model-p ((object source-class))
  (typep (source-class-model object)
         'model-class))

(defmethod has-value-p ((object source-class))
  (null (source-class-value object)))

(defmethod rename-netlist-element ((object source-class) name &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((return-value (call-next-method object
                                        name
                                        :debug-mode debug-mode
                                        :output output)))
    (when (source-class-model return-value)
      (setf (source-class-model return-value) (rename-netlist-element (source-class-model return-value)
                                                                      name
                                                                      :debug-mode debug-mode
                                                                      :output output)))
    (when (source-class-nodes-list return-value)
      (setf (source-class-nodes-list return-value) (mapcar #'(lambda (x)
							       (concatenate 'string name
                                                                            ":"
                                                                            x))
                                                           (source-class-nodes-list return-value))))
    return-value))

(defmethod element-with-node ((object source-class) node-name)
  (when (position node-name
                  (source-class-nodes-list object)
                  :test 'string-equal)
    object))

(defmethod check-element-with-selectors ((object source-class) selectors)
  (let ((return-value (call-next-method object selectors)))
    (if (listp selectors)
	(dolist (selector selectors)
	  (setq return-value (or return-value
				  (funcall selector object))))
	(setq return-value (funcall selectors object)))
    return-value))

;; End sources.lisp
