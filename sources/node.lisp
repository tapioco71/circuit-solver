;;;; -*- mode: lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; node.lisp
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
(defmethod print-object ((object node-class) s)
  (print-unreadable-object (object s :type t)
    (format s
            ":id ~s name ~s :class ~s :state ~s :number ~s"
            (node-class-id object)
            (node-class-name object)
            (node-class-class object)
            (node-class-state object)
            (node-class-number object))))

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

;;;; end of node.lisp file.
