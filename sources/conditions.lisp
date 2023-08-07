;;;; -*- mode: common-lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; conditions.lisp

(in-package :circuit-solver)

;; Conditions.

;;;
;;; 05/12/2014 - 23:15:00
;;;
;;; exception handling
;;;

(define-condition invalid-source-element (error)
  ((source-name
    :initarg :source-name
    :reader source-name)))

(define-condition invalid-passive-element-in-coupling (error)
  ((coupling-name :initarg :coupling-name :reader coupling-name)))

(define-condition mismatched-number-of-coupling-values-vs-coupling-inductances-error (error)
  ((coupling-name :initarg :coupling-name :reader coupling-name)))

(define-condition  wrong-number-of-elements-for-coupling-error (error)
  ((element-name :initarg :element-name :reader element-name)))

(define-condition wrong-subcircuit-nodes-list-error (error)
  ((subcircuit-name :initarg :subcircuit-name :reader subcircuit-name)
   (actual-nodes-count :initarg :actual-nodes-count :reader actual-nodes-count)
   (needed-nodes-count :initarg :needed-nodes-count :reader needed-nodes-count)))

(define-condition unknown-object-error (error)
  ((object :initarg :object :reader object :initform nil)))

(define-condition unknown-function-error (error)
  ((function-name :initarg :function-name :reader function-name :initform nil)))

(define-condition undefined-model-class-error (error)
  ((model-name :initarg :model-name :reader model-name :initform nil)
   (model-class-name :initarg :model-class-name :reader model-class-name :initform nil)))

(define-condition initial-condition-error (error)
  ((initial-condition-name :initarg :initial-condition-name :reader initial-condition-name :initform nil)
   (element-name :initarg :element-name :reader element-name :initform nil)
   (coupling-name :initarg :coupling-name :reader coupling-name :initform nil)
   (node-name :initarg :node-name :reader node-name :initform nil)))

(define-condition unknow-element-for-initial-condition-error (error)
  ((element-name :initarg :element-name :reader element-name :initform nil)
   (initial-condition-name :initarg :initial-condition-name :reader initial-condition-name :initform nil)))

(define-condition unknow-object-error (error)
  ((object :initarg :object :reader object :initform nil)))

(define-condition file-writing-error (error)
  ((file-pathname :initarg :file-pathname :reader file-pathname)))

(define-condition file-not-opened-error (error)
  ())

(define-condition probe-not-found-error (error)
  ((probe-name :initarg :probe-name :reader probe-name :initform nil)
   (node-name :initarg :node-name :reader node-name :initform nil)
   (element-name :initarg :element-name :reader element-name :initform nil)))

(define-condition file-not-found-error (error)
  ((file-pathname :initarg :file-pathname :reader file-pathname)))

(define-condition repeated-node-for-element-error (error)
  ((node-name :initarg :node-name :reader node-name)
   (element-name :initarg :element-name :reader element-name)))

(define-condition no-such-node-for-element-error (error)
  ((node-name :initarg :node-name :reader node-name)
   (element-name :initarg :element-name :reader element-name)))

(define-condition mismatched-coupling-element (error)
  ((element-name :initarg :element-name :reader element-name)))

(define-condition no-node-for-probe-error (error)
  ((node-name :initarg :node-name :reader node-name)
   (probe-name :initarg :probe-name :reader probe-name)))

(define-condition no-element-for-probe-error (error)
  ((element-name :initarg :element-name :reader element-name)
   (probe-name :initarg :probe-name :reader probe-name)))

(define-condition undefined-probe-type-error (error)
  ((probe-name :initarg :probe-name :reader probe-name)
   (probe-type :initarg :probe-type :reader probe-type)))

(define-condition simulation-time-interval-error (error)
  ((t0 :initarg :t0 :reader t0)
   (t1 :initarg :t1 :reader t1)))

(define-condition value-or-model-entry-error (error)
  ((text :initarg :text :reader text)))

(define-condition parser-error (error)
  ((file-pathname
    :initarg :file-pathname
    :accessor file-pathname
    :initform nil
    :documentation "Name of the file where error is.")))

(define-condition solver-error (error)
  ((numerical
    :initarg :message
    :accessor numerical-error
    :initform nil
    :documentation "Numerical problem solving problem.")
   (time-step
    :initarg
    :message
    :accessor time-step-error
    :initform nil
    :documentation "Time step at which there is an error.")))

;; End conditions.lisp
