;;;; -*- mode: common-lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; subcircuit.lisp

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

;; Methods.

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

;; End subcircuit.lisp
