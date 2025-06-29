;;;; -*- mode: lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; subcircuit.lisp
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
;;; subcircuit class definition
;;;

(defclass subcircuit-class (element-class)
  ((file-pathname
    :documentation "FILE-PATHNAME for subcircuit definition."
    :initarg :file-pathname
    :initform ""
    :accessor file-pathname
    :accessor subcircuit-class-file-pathname
    :accessor element-class-file-pathname)
   (nodes-list
    :documentation "list of connection nodes."
    :initarg :nodes-list
    :initform nil
    :accessor nodes-list
    :accessor subcircuit-class-nodes-list
    :accessor element-class-nodes-list)))

;; Functions
(defun make-subcircuit (&rest parameters &key
                                           (id nil id-p)
                                           (name (symbol-name (gensym "subcircuit-")) name-p)
                                           (file-pathname #p"" file-pathname-p)
                                           (nodes-list nil nodes-list-p))
  (declare (ignorable parameters
                      id
                      name
                      file-pathname
                      nodes-list))
  (make-instance (find-class 'subcircuit-class)
                 :id id
                 :name name
                 :file-pathname file-pathname
                 :nodes-list nodes-list))

;; Methods.
(defmethod print-object ((object subcircuit-class) s)
  (print-unreadable-object (object s :type t)
    (format s
            ":id ~s name ~s :class ~s :file-pathname ~s :nodes-list ~s"
            (subcircuit-class-id object)
            (subcircuit-class-name object)
            (subcircuit-class-file-pathname object)
            (subcircuit-class-nodes-list object))))

;;
;; create a sexp for subcircuit element: :NAME name [ :ID id ] :FILE-PATHNAME file-name :NODES-LIST nodes-list
;;

(defmethod sexpify ((object subcircuit-class))
  "Create a sexp for subcircuit element: :NAME name [ :ID id ] :FILE-PATHNAME file-name :NODES-LIST nodes-list."
  (let ((return-value (call-next-method object)))
    (when (pathnamep (subcircuit-class-file-pathname object))
      (setq return-value (append return-value  (list :file-pathname (subcircuit-class-file-pathname object)))))
    (when (subcircuit-class-nodes-list object)
      (setq return-value (append return-value (list :nodes-list (subcircuit-class-nodes-list object)))))
    return-value))

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
  (when (position node-name
                  (subcircuit-class-nodes-list object)
                  :test 'string-equal)
    object))

;;;; end of subcircuit.lisp file.
