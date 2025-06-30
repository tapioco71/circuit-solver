;;;; -*- mode: lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; initial-condition.lisp
;;;;
;;;; Copyright (c) 2020-2025 Angelo Rossi
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
(defmethod print-object ((object initial-condition-class) s)
  (print-unreadable-object (object s :type t)
    (format s
            ":id ~s name ~s :target-name ~s :value ~s"
            (element-class-id object)
            (element-class-name object)
            (initial-condition-class-target-name object)
            (initial-condition-class-value object))))

(defmethod sexpify ((object initial-condition-class))
  (let ((return-value (call-next-method object)))
    (when (initial-condition-class-target-name object)
      (setq return-value (append return-value (list :target-name (initial-condition-class-target-name object)))))
    (when (initial-condition-class-value object)
      (setq return-value (append return-value (list :value (initial-condition-class-value object)))))
    return-value))

;;;; end of initial-condition.lisp file.
