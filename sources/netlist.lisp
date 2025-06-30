;;;; -*- mode: lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; netlist.lisp
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
;;; netlist as element:
;;;
;;; - a characters string to hold FILE-NAME for the netlist;
;;; - an ELEMENTS-LIST holding subcircuits, bipoles, nodes, probes and so on.
;;;

(defclass netlist-class (element-class)
  ((file-pathname
    :initarg :file-pathname
    :initform #p""
    :accessor file-pathname
    :accessor netlist-class-file-pathname
    :accessor element-class-file-pathname)
   (author
    :initarg :author
    :initform ""
    :accessor author
    :accessor netlist-class-author
    :accessor element-class-author)
   (date
    :initarg :date
    :initform ""
    :accessor date
    :accessor netlist-class-date
    :accessor element-class-date)
   (elements-list
    :initarg :elements-list
    :initform nil
    :accessor elements-list
    :accessor netlist-class-elements-list
    :accessor element-class-elements-list)))

;; Functions.

(defun make-netlist (&rest parameters &key
                                        (id nil id-p)
                                        (name (symbol-name (gensym "netlist-")) name-p)
                                        (file-pathname nil file-pathname-p)
                                        (author nil author-p)
                                        (date nil date-p)
                                        (elements-list nil elements-list-p))
  (declare (ignorable parameters id name file-pathname author date elements-list))
  (let ((object (make-instance 'netlist-class
                               :id id
                               :name name
                               :file-pathname file-pathname
                               :author author
                               :date date
                               :elements-list elements-list)))
    object))

;; Methods.
(defmethod print-object ((object netlist-class) s)
  (print-unreadable-object (object s :type t)
    (format s
            ":id ~s name ~s :file-pathname ~s :author ~s :date ~s :elements-list ~s"
            (element-class-id object)
            (element-class-name object)
            (netlist-class-file-pathname object)
            (netlist-class-author object)
            (netlist-class-date object)
            (netlist-class-elements-list object))))

(defmethod sexpify ((object netlist-class))
  "Create a sexp for netlist element: :NAME name [ :ID id ] :ELEMENTS-LIST elements-list."
  (let ((return-value (call-next-method object)))
    (unless (string-equal (netlist-class-author object)
                          "")
      (setq return-value (append return-value (list :author (netlist-class-author object)))))
    (unless (string-equal (netlist-class-date object)
                          "")
      (setq return-value (append return-value (list :date (netlist-class-date object)))))
    (setq return-value (append return-value (list :elements-list (mapcar #'sexpify (netlist-class-elements-list object)))))
    return-value))

(defmethod rename-netlist-element ((object netlist-class) name &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((return-value object))
    (when debug-mode
      (format output
              "~%~%Renaming netlist ~a to "
              (element-class-name object))
      (finish-output output))
    (setf (element-class-name return-value) (concatenate 'string name
                                                         ":"
                                                         (element-class-name return-value)))
    (when debug-mode
      (format output
              "~a."
              (element-class-name return-value))
      (finish-output output))
    (dolist (element (netlist-class-elements-list return-value))
      (rename-netlist-element element
                              name
                              :debug-mode debug-mode
                              :output output))
    return-value))

;;;; end of netlist.lisp file.
