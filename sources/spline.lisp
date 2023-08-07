;;;; -*- mode: common-lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; spline.lisp

(in-package :circuit-solver)

;; Classes.

;;;
;;; spline class
;;;

(defclass spline-data-class (element-class)
  ((data-vectors
    :documentation "data itself."
    :initarg :data-vectors
    :initform nil
    :accessor spline-class-data-vectors)))

;; Functions.

(defun make-spline-data (&rest parameters &key
                                            (id nil id-p)
                                            (name (symbol-name (gensym "spline-data-")) name-p)
                                            (data-vectors nil data-vectors-p))
  (declare (ignorable parameters id name data-vectors))
  (let ((object (make-instance 'spline-data-class
                               :id id
                               :name name
                               :data-vectors data-vectors)))
    object))

;; End spline.lisp
