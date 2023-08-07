;;;; -*- mode: common-lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; utilities.lisp

(in-package :circuit-solver)

;; Functions.

(defun reduce-to-echelon (a &rest parameters &key (start '(0 0) start-p) (dimensions '(0 0) dimensions-p))
  (declare (ignorable parameters
                      start
                      dimensions))
  (check-type a grid:foreign-array)
  (when start-p
    (check-type start list)
    (assert (= (length start) 2))
    (loop
      for x in start
      do
         (check-type x (integer 0))))
  (if dimensions-p
      (progn
        (check-type dimensions list)
        (assert (= (length dimensions) 2))
        (loop
          for x in dimensions
          do
             (check-type x (integer 0))))
      (setq dimensions (grid:dimensions a)))
  (let ((as (grid:subgrid a dimensions start)))
    (when as
      (loop
        named main-loop
        for c from 0 below (grid:dim1 as)
        unless (zerop (grid:norm (grid:row as c)
                                 :infinity))
          do
             (loop
               with b = nil
               for r from 0 below (grid:dim0 as)
               unless (zerop (grid:gref as r c))
                 do
                    (setq b (grid:row as 0))
                    (setf (grid:row as 0) (grid:row as c)
                          (grid:row as c) b)
                    (setf (grid:row as 0) (gsl:elt* (grid:row as 0)
                                                    (/ 1d0 (grid:gref as 0 c))))
                    (loop
                      with alpha = 0d0
                      for p from (1+ r) below (grid:dim0 as)
                      do
                         (setq alpha (grid:gref a p c))
                         (setf (grid:row as p) (gsl:elt+ (gsl:elt* (- alpha) (grid:row as r))
                                                         (grid:row as p))))
                    (when (and (> (grid:dim0 as) 0)
                               (> (grid:dim1 as) 0))
                      (reduce-to-echelon as
                                         :start '(1 1)
                                         :dimensions (list (1- (grid:dim0 as))
                                                           (1- (grid:dim1 as))))))))))
