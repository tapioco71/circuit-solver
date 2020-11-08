;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; node.lisp
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

;;
;; Methods.
;;

(defmethod undefined-class-p ((object node-class))
  (or (string-equal (string-downcase (node-class-class object))
                    "undefined")
      (string-equal (node-class-class object) "")))

(defmethod reference-class-node-p ((object node-class))
  (or (string-equal (string-downcase (node-class-class object))
                    "reference")
      (string-equal (string-downcase (node-class-class object))
                    "gnd")
      (string-equal (string-downcase (node-class-class object))
                    "0")))

(defmethod element-with-node ((object node-class) node-name)
  (when (string-equal node-name (element-class-name object))
    object))

;; End node.lisp
