;;;; -*- mode: lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; probe.lisp
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
;;; probe class definition
;;;

(defclass probe-class (element-class)
  ((class
    :documentation "probe CLASS: voltage, current or undefined probe type."
    :initarg :class
    :initform "undefined"
    :accessor probe-class-class
    :accessor element-class-class)
   (elements-list
    :documentation "if probe = current then currents flowing in the elements in the list are take in account."
    :initarg :elements-list
    :initform nil
    :accessor probe-class-elements-list
    :accessor element-class-elements-list)
   (nodes-list
    :documentation " If probe = voltage voltage across two nodes will be taken in account."
    :initarg :nodes-list
    :initform nil
    :accessor probe-class-nodes-list
    :accessor element-class-nodes-list)))

;; Functions.

(defun make-probe (&rest parameters &key
                                      (id nil id-p)
                                      (name (symbol-name (gensym "probe-")) name-p)
                                      (class nil class-p)
                                      (elements-list nil elements-list-p)
                                      (nodes-list nil nodes-list-p))
  (declare (ignorable parameters id name class elements-list nodes-list))
  (let ((object (make-instance 'probe-class
                               :id id
                               :name name
                               :class class
                               :elements-list elements-list
                               :nodes-list nodes-list)))
    object))

;; Methods.
(defmethod print-object ((object probe-class) s)
  (print-unreadable-object (object s :type t)
    (format s
            ":id ~s name ~s :class ~s :elements-list ~s :nodes-list ~s"
            (element-class-id object)
            (element-class-name object)
            (probe-class-class object)
            (probe-class-elements-list object)
            (probe-class-nodes-list object))))

(defmethod sexpify ((object probe-class))
  (let ((return-value (call-next-method object)))
    (unless (undefined-class-p object)
      (setq return-value (append return-value (list :class (probe-class-class object)))))
    (when (probe-class-elements-list object)
      (setq return-value (append return-value (list :elements-list (probe-class-elements-list object)))))
    (when (probe-class-nodes-list object)
      (setq return-value (append return-value (list :nodes-list (probe-class-nodes-list object)))))
    return-value))

(defmethod undefined-class-p ((object probe-class))
  (or (string-equal (probe-class-class object)
                    "undefined")
      (string-equal (probe-class-class object)
                    "")))

(defmethod voltage-probe-class-p ((object probe-class))
  (string-equal (probe-class-class object)
                "voltage-probe"))

(defmethod current-probe-class-p ((object probe-class))
  (string-equal (probe-class-class object)
                "current-probe"))

(defmethod rename-netlist-element ((object probe-class) name &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (handler-case
      (let ((return-value (call-next-method object
                                            name
                                            :debug-mode debug-mode
                                            :output output)))
	(cond
	  ((voltage-probe-class-p return-value)
	   (setf (probe-class-nodes-list return-value) (mapcar #'(lambda (x)
								   (merge-names name x))
                                                               (probe-class-nodes-list return-value))))
	  ((current-probe-class-p return-value)
	   (setf (probe-class-elements-list return-value) (mapcar #'(lambda (x)
								      (merge-names name x))
                                                                  (probe-class-elements-list return-value))))
	  (t
	   (error 'undefined-probe-type-error
                  :probe-type (element-class return-value)
                  :probe-name (element-class-name return-value))))
	return-value)

    (undefined-probe-type-error (condition)
      (format *error-output*
              "~%Undefined probe type ~a for ~a."
              (probe-type condition)
              (probe-name condition))
      (finish-output *error-output*)
      nil)))

(defmethod element-with-node ((object probe-class) node-name)
  (when (position node-name
                  (probe-class-nodes-list object)
                  :test 'string-equal)
    object))

(defmethod check-element-with-selectors ((object probe-class) selectors)
  (let ((return-value (call-next-method object selectors)))
    (if (listp selectors)
	(dolist (selector selectors)
	  (setq return-value (or return-value
				  (funcall selector object))))
	(setq return-value (funcall selectors object)))
    return-value))

;;;; end of probe.lisp file.
