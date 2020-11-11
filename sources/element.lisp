;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; element.lisp
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
;;; basic netlist element class definition:
;;;
;;; - a numerical ID;
;;; - a characters string for NAME.
;;;

(defclass element-class ()
  ((id
    :initarg :id
    :initform -1
    :accessor element-class-id)
   (name
    :initarg :name
    :initform ""
    :accessor element-class-name)))

;; Methods.

(defmethod sexpify ((object element-class))
  "Create a sexp for element: :NAME name :ID id"
  (let ((return-value (list (type-of object))))
    (when (element-class-name object)
      (setq return-value (append return-value (list :name (element-class-name object)))))
    (unless (eql (element-class-id object) -1)
      (setq return-value (append return-value (list :id (element-class-id object)))))
    return-value))

(defmethod rename-element ((object element-class) radix)
  (setf (element-class-name object) (concatenate 'string
                                                 radix
                                                 ":"
                                                 (element-class-name object)))
  object)

(defmethod rename-netlist-element ((object element-class) name &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((return-value object))
    (when debug-mode
      (format output
              "~%Renaming ~a type ~a to "
              (element-class-name return-value)
              (type-of return-value))
      (finish-output output))
    (setf (element-class-name return-value) (merge-names name
                                                         (element-class-name return-value)))
    (when debug-mode
      (format output
              "~a."
              (element-class-name return-value))
      (finish-output output))
    return-value))

(defmethod check-element-with-selectors ((object element-class) selectors)
  (let ((return-value nil))
    (if (listp selectors)
	(dolist (selector selectors)
	  (setq return-value (or return-value
				 (funcall selector object))))
	(setq return-value (funcall selectors object)))
    return-value))

;; End element.lisp
