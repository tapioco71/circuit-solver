;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; problem.lisp
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
   (simulation-type
    :documentation "simulation type: time, frequency or undefined."
    :initarg :simulation-type
    :initform "undefined"
    :accessor problem-class-simulation-type)
   (x-start
    :documentation "Start value for time or frequency."
    :initarg :x-start
    :initform nil
    :accessor problem-class-x-start)
   (x-end
    :documentation "End value for time or frequency."
    :initarg :x-end
    :initform nil
    :accessor problem-class-x-end)
   (x-value
    :documentation "Value for time or frequency during simulation."
    :initarg :x-value
    :initform nil
    :accessor problem-class-x-value)
   (x-steps
    :documentation "Number of steps for time or frequency."
    :initarg :x-steps
    :initform 1000
    :accessor problem-class-x-steps)))

;; End problem.lisp
