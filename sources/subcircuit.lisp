;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; subcircuit.lisp
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
;;; subcircuit class definition
;;;

(defclass subcircuit-class (element-class)
  ((file-pathname
    :documentation "FILE-NAME for subcircuit definition."
    :initarg :file-pathname
    :initform ""
    :accessor subcircuit-class-file-pathname
    :accessor element-class-file-pathname)
   (nodes-list
    :documentation "list of connection nodes."
    :initarg :nodes-list
    :initform nil
    :accessor subcircuit-class-nodes-list
    :accessor element-class-nodes-list)))

(defmethod rename-netlist-element ((object subcircuit-class) name &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((return-value (call-next-method object name
                                        :debug-mode debug-mode
                                        :output output)))
    (setf (subcircuit-class-nodes-list return-value) (mapcar #'(lambda (x)
							         (concatenate 'string name ":" x))
                                                             (subcircuit-class-nodes-list return-value)))
    return-value))

(defmethod element-with-node ((object subcircuit-class) node-name)
  (when (position node-name (subcircuit-class-nodes-list object) :test 'string-equal)
    object))

;; End subcircuit.lisp
