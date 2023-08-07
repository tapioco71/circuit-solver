;;;; -*- mode: common-lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; netlist.lisp

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

;; End netlist.lisp
