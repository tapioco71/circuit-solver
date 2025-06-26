;;;; -*- mode: lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; problem.lisp
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
;;; general problem class
;;;

(defclass problem-class ()
  ((id
    :documentation "ID value of problem."
    :initarg :id
    :initform -1
    :accessor problem-class-id)
   (name
    :documentation "Problem name."
    :initarg :name
    :initform ""
    :accessor problem-class-name)
   (date
    :documentation "Problem date creation."
    :initarg :date
    :initform (get-decoded-time)
    :accessor problem-class-date)
   (netlist-file-pathname
    :documentation "Main netlist file name."
    :initarg :netlist-file-pathname
    :initform ""
    :accessor problem-class-netlist-file-pathname)
   (log-file-pathname
    :documentation "Log file name for simulation errors/info."
    :initarg :log-file-pathname
    :initform ""
    :accessor problem-class-log-file-pathname)
   (netlist
    :documentation "Main netlist."
    :initarg :netlist
    :initform ()
    :accessor problem-class-netlist)
   (simulation
    :documentation "Simulation data."
    :initarg :simulation
    :initform nil
    :accessor simulation)
   (p-matrix
    :documentation "P matrix."
    :initarg :p-matrix
    :initform nil
    :accessor p-matrix)
   (r-matrix
    :documentation "R matrix."
    :initarg :r-matrix
    :initform nil
    :accessor r-matrix)
   (g-matrix
    :documentation "G matrix."
    :initarg :g-matrix
    :initform nil
    :accessor g-matrix)
   (si-matrix
    :documentation "Si matrix."
    :initarg :si-matrix
    :initform nil
    :accessor si-matrix)
   (sv-matrix
    :documentation "Sv matrix."
    :initarg :sv-matrix
    :initform nil
    :accessor sv-matrix)
   (l-matrix
    :documentation "L matrix."
    :initarg :l-matrix
    :initform nil
    :accessor l-matrix)
   (c-matrix
    :documentation "C matrix."
    :initarg :c-matrix
    :initform nil
    :accessor c-matrix)
   (ki-matrix
    :documentation "Ki matrix."
    :initarg :ki-matrix
    :initform nil
    :accessor ki-matrix)
   (kv-matrix
    :documentation "Kv matrix."
    :initarg :kv-matrix
    :initform nil
    :accessor kv-matrix)))

;; Functions.

(defun make-problem (&rest parameters &key
                                        (id (symbol-name (gensym "P-")) id-p)
                                        (name "" name-p)
                                        (date (get-universal-time) date-p)
                                        (netlist-file-pathname nil netlist-file-pathname-p)
                                        (log-file-pathname nil log-file-pathname-p)
                                        (netlist nil netlist-p)
                                        (simulation nil simulation-p)
                                        (p-matrix nil p-matrix-p)
                                        (r-matrix nil r-matrix-p)
                                        (g-matrix nil g-matrix-p)
                                        (si-matrix nil si-matrix-p)
                                        (sv-matrix nil sv-matrix-p)
                                        (l-matrix nil l-matrix-p)
                                        (c-matrix nil c-matrix-p)
                                        (ki-matrix nil ki-matrix-p)
                                        (kv-matrix nil kv-matrix-p))
  (declare (ignorable parameters
                      id
                      name
                      date
                      netlist-file-pathname
                      log-file-pathname
                      netlist
                      simulation
                      p-matrix
                      r-matrix
                      g-matrix
                      si-matrix
                      sv-matrix
                      l-matrix
                      c-matrix
                      ki-matrix
                      kv-matrix))
  (let ((object (make-instance 'problem-class
                               :id id
                               :name name
                               :date date
                               :netlist-file-pathname netlist-file-pathname
                               :log-file-pathname log-file-pathname
                               :netlist netlist
                               :simulation simulation
                               :p-matrix p-matrix
                               :r-matrix r-matrix
                               :g-matrix g-matrix
                               :si-matrix si-matrix
                               :sv-matrix sv-matrix
                               :l-matrix l-matrix
                               :c-matrix c-matrix
                               :ki-matrix ki-matrix
                               :kv-matrix kv-matrix)))
    object))

;; Methods.

;; Macros.

(defmacro generate-thread-function ((p debug) &body body)
  `#'(lambda ()
       (let ((return-value nil)
	     (thread-name (symbol-name (gensym "thread-"))))
         (lparallel:force ,p)
	 (when ,debug
	   (format *standard-output*
		   "Thread ~a started.~%"
		   thread-name)
	   (finish-output *standard-output*))
	 ,@body
	 (when ,debug
	   (format *standard-output*
		   "Thread ~a finished.~%"
		   thread-name)
	   (finish-output *standard-output*))
	 (setq return-value t)
	 return-value)))

;;;; end of problem.lisp file.
