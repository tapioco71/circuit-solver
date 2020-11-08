;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; passive.lisp
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
;;; passive circuital element
;;;

(defclass passive-class (element-class)
  ((class
    :documentation "passive CLASS: resistance, conductance, inductance, capacitance or undefined."
    :initarg :class
    :initform "undefined"
    :accessor passive-class-class
    :accessor element-class-class)
   (nodes-list
    :documentation "NODES-LIST are the pin connections for the element: (\"N1\" \"N2\")"
    :initarg :nodes-list
    :initform ()
    :accessor passive-class-nodes-list
    :accessor element-class-nodes-list)
   (model
    :documentation "passive MODEL."
    :initarg :model
    :initform nil
    :accessor passive-class-model
    :accessor element-class-model)
   (value
    :documentation "passive constant VALUE e.g. R1 = 10 Ohm: VALUE = 10 Ohm."
    :initarg :value
    :initform 0d0
    :accessor passive-class-value
    :accessor element-class-value)))

;; Methods.

(defmethod sexpify ((object passive-class))
  "Create a sexp for passive element: :NAME name [ :ID id ] :CLASS element-class :NODES-LIST nodes-list { :MODEL model | :VALUE value }."
  (let ((return-value (call-next-method object)))
    (unless (undefined-class-p object)
      (setq return-value (append return-value (list :class (passive-class-class object)))))
    (when (passive-class-nodes-list object)
      (setq return-value (append return-value (list :nodes-list (passive-class-nodes-list object)))))
    (when (has-model-p object)
      (setq return-value (append return-value (list :model (sexpify (passive-class-model object))))))
    (when (has-value-p object)
      (setq return-value (append return-value (list :value (passive-class-value object)))))
    return-value))

(defmethod undefined-class-p ((object passive-class))
  (or (string-equal (passive-class-class object)
                    "undefined")
      (string-equal (passive-class-class object)
                    "")))

(defmethod has-model-p ((object passive-class))
  (typep object 'model-class))

(defmethod has-value-p ((object passive-class))
  (null (passive-class-value object)))

(defmethod rename-netlist-element ((object passive-class) name &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((return-value (call-next-method object
                                        name
                                        :debug-mode debug-mode
                                        :output output)))
    (when (passive-class-model return-value)
      (setf (passive-class-model return-value) (rename-netlist-element (passive-class-model return-value)
                                                                       name
                                                                       :debug-mode debug-mode)))
    (when (passive-class-nodes-list return-value)
      (setf (passive-class-nodes-list return-value) (mapcar #'(lambda (x)
							        (concatenate 'string name
                                                                             ":"
                                                                             x))
                                                            (passive-class-nodes-list return-value))))
    return-value))

;; Methods.

(defmethod element-with-node ((object passive-class) node-name)
  (when (position node-name
                  (passive-class-nodes-list object)
                  :test 'string-equal)
    object))

;; End passive.lisp
