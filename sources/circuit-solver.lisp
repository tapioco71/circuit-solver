;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; circuit-solver.lisp
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

;;; "circuit_solver" goes here. Hacks and glory await!

;;;
;;; define some constants
;;;

;;;
;;; 23/07/2011 - 19:56:37
;;;
;;; Changing defparameter to defconstant
;;;

(defconstant major 0)
(defconstant minor 3)
(defconstant build 0)
(defconstant revision 0)
(defconstant year 2014)

;;;
;;; special variables
;;;

(defparameter *time* 0d0)
(defparameter *minimum-steps-number* 10)
(defparameter *steps-number* *minimum-steps-number*)
(defparameter *old-step* nil)
(defparameter *t0* 0d0)
(defparameter *t1* 0d0)
(defparameter *h* 0d0)

;;;
;;; data structures:
;;; classes for circuital elements and problem description
;;;
;;; - element is the base class bearing an numerical id and a string name
;;; - netlist is the set of circuital elements and netlists
;;; - node is the object for describing circuital elements junction
;;;

;;
;; serialization: create an object from a sexp
;;

;; (defun objectify (classes sexp)
;;   (cond
;;     ((atom sexp)
;;      sexp)
;;     ((member (first sexp) classes)
;;      (apply (function make-instance)
;; 	    (mapcar (lambda (subsexp)
;; 		      (objectify classes subsexp)) sexp)))
;;     (t
;;      (mapcar
;;       (lambda (subsexp)
;; 	(objectify classes subsexp)) sexp))))

(defun create-object-from-sexp (class sexp)
  (apply (function make-instance)
	 (concatenate 'list (list class) (mapcar (lambda (subsexp)
						   (objectify subsexp))
                                                 (rest sexp)))))

(defun objectify (sexp)
  (handler-case
      (cond
	((atom sexp)
	 sexp)
	((eql (first sexp) 'netlist)
	 (create-object-from-sexp 'netlist-class sexp))
	((eql (first sexp) 'passive)
	 (create-object-from-sexp 'passive-class sexp))
	((eql (first sexp) 'coupling)
	 (create-object-from-sexp 'coupling-class sexp))
	((eql (first sexp) 'source)
	 (create-object-from-sexp 'source-class sexp))
	((eql (first sexp) 'node)
	 (create-object-from-sexp 'node-class sexp))
	((eql (first sexp) 'probe)
	 (create-object-from-sexp 'probe-class sexp))
	((eql (first sexp) 'subcircuit)
	 (create-object-from-sexp 'subcircuit-class sexp))
	((eql (first sexp) 'model)
	 (create-object-from-sexp 'model-class sexp))
	((eql (first sexp) 'initial-condition)
	 (create-object-from-sexp 'initial-condition-class sexp))
	(t
	 (mapcar (lambda (subsexp)
		   (objectify subsexp))
                 sexp)))
    (unknown-object-error (condition)
      (format *error-output*
              "~%Unknown object found: ~s~%"
              (object condition))
      (finish-output *error-output*)
      nil)))

;;
;; Predicates.
;;

(defun resistance-class-p (object)
  (when (typep object 'passive-class)
    (string-equal (passive-class-class object)
                  "resistance")))

(defun inductance-class-p (object)
  (when (typep object 'passive-class)
    (string-equal (passive-class-class object)
                  "inductance")))

(defun capacitance-class-p (object)
  (when (typep object 'passive-class)
    (string-equal (passive-class-class object)
                  "capacitance")))

(defun conductance-class-p (object)
  (when (typep object 'passive-class)
    (string-equal (passive-class-class object)
                  "conductance")))

(defun coupling-class-p (object)
  (typep object 'coupling-class))

;;
;; selection and exclusion criterion functions
;;

(defun where (&rest parameters &key class-type id name number class)
  (declare (ignorable parameters class-type id name number class))
  #'(lambda (object)
      (and (if class-type
	       (typep object class-type)
	       t)
	   (if id
	       (eql (element-class-id object) id)
	       t)
	   (if name
	       (string-equal (element-class-name object) name)
	       t)
	   (if class
	       (string-equal (element-class-class object) class)
	       t)
	   (if number
	       (eql (element-class-number object) number)
	       t))))

;;;
;;; select elements in a list that satisfy WHERE clause. List of WHERE clauses could be used to perfect the search. Found elements are merged into a list.
;;;

(defun select (selectors netlist &rest parameters &key (lock nil))
  (declare (ignorable parameters lock))
  (let ((selection nil)
        (temp-netlist nil))
    (if lock
        (bt:with-recursive-lock-held (lock)
          (setq temp-netlist (copy-seq netlist)))
        (setq temp-netlist netlist))
    (dolist (element temp-netlist)
      (let ((where-return-value nil))
	(if (listp selectors)
	    (dolist (selector selectors)
	      (setq where-return-value (or (funcall selector element)
                                           where-return-value)))
	    (setq where-return-value (funcall selectors element)))
	(when (eql where-return-value t)
	  (setq selection (append selection (list element))))))
    selection))

;;;
;;; create a list where all elements do not satisfy the WHERE clause
;;;

(defun exclude (selectors netlist &rest parameters &key (lock nil))
  (declare (ignorable parameters lock))
  (let ((selection nil)
        (temp-netlist nil))
    (if lock
        (bt:with-recursive-lock-held (lock)
          (setq temp-netlist (copy-seq netlist)))
        (setq temp-netlist netlist))
    (dolist (element temp-netlist)
      (let ((where-return-value nil))
	(if (listp selectors)
	    (dolist (selector selectors)
	      (setq where-return-value (or (funcall selector element) where-return-value)))
	    (setq where-return-value (funcall selectors element)))
	(unless (eql where-return-value t)
	  (setq selection (append selection (list element))))))
    selection))

;;;
;;; find an element in a list that satisfy the WHERE clause or a list of WHERE clauses.
;;;

(defun find-element (selectors netlist &rest parameters &key (lock nil))
  (declare (ignorable parameters lock))
  (let ((element-position 0)
        (temp-netlist nil))
    (if lock
        (bt:with-recursive-lock-held (lock)
          (setq temp-netlist (copy-seq netlist)))
        (setq temp-netlist netlist))
    (dolist (element temp-netlist)
      (when (check-element-with-selectors element selectors)
	(return (values element element-position)))
      (incf element-position))))

;;;
;;; count function
;;;

(defun count-elements (selectors netlist &rest parameters &key (lock nil))
  (declare (ignorable parameters lock))
  (let ((temp-netlist nil))
    (if lock
        (bt:with-recursive-lock-held (lock)
          (setq temp-netlist (copy-seq netlist)))
        (setq temp-netlist netlist))
    (length (select selectors temp-netlist))))

;;;
;;; update element members
;;;

(defmethod update ((object element-class) &rest parameters &key id name)
  (declare (ignorable parameters id name))
  (when id
    (setf (element-class-id object) id))
  (when name
    (setf (element-class-name object) name))
  object)

(defmethod update ((object netlist-class) &rest parameters &key id name elements-list)
  (declare (ignorable parameters id name elements-list))
  (let ((return-value (call-next-method object
                                        :id id
                                        :name name)))
    (when elements-list
      (setf (netlist-class-elements-list return-value) elements-list))
    return-value))

(defmethod update ((object node-class) &rest parameters &key id name class number)
  (declare (ignorable parameters id name class number))
  (let ((return-value (call-next-method object
                                        :id id
                                        :name name)))
    (when class
      (setf (node-class-class object) class))
    (when number
      (setf (node-class-number object) number))
    return-value))

(defmethod update ((object passive-class) &rest parameters &key id name class nodes-list model value)
  (declare (ignorable parameters id name class nodes-list model value))
  (handler-case
      (let ((return-value (call-next-method object
                                            :id id
                                            :name name)))
	(when class
	  (setf (passive-class-class return-value) class))
	(when nodes-list
	  (setf (passive-class-nodes-list return-value) nodes-list))
	(when model
	  (setf (passive-class-model return-value) model))
	(when value
	  (setf (passive-class-value return-value) value))
	return-value)
    (value-or-model-entry-error (condition)
      (format *error-output*
              "Only value or model shall be selected for ~a.~%"
              (text condition))
      (finish-output *error-output*)
      nil)))

(defmethod update ((object coupling-class) &rest parameters &key id name elements-list model value)
  (declare (ignorable parameters id name elements-list model value))
  (handler-case
      (let ((return-value (call-next-method object
                                            :id id
                                            :name name)))
	(when elements-list
	  (setf (coupling-class-elements-list return-value) elements-list))
	(when model
	  (setf (coupling-class-model return-value) model))
	(when value
	  (setf (coupling-class-value object) value))
	return-value)
    (value-or-model-entry-error (condition)
      (format *error-output*
              "Only value or model shall be selected for ~a.~%"
              (text condition))
      (finish-output *error-output*)
      nil)))

(defmethod update ((object source-class) &rest parameters &key id name class nodes-list model value)
  (declare (ignorable parameters id name class nodes-list model value))
  (let ((return-value (call-next-method object
                                        :id id
                                        :name name)))
    (when class
      (setf (source-class-class return-value) class))
    (if nodes-list
	(setf (source-class-nodes-list return-value) nodes-list))
    (when model
      (setf (source-class-model return-value) model))
    (when value
      (setf (source-calss-value return-value) value))
    return-value))

(defmethod update ((object subcircuit-class) &rest parameters &key id name file-pathname nodes-list)
  (declare (ignorable parameters id name file-pathname nodes-list))
  (let ((return-value (call-next-method objexct
                                        :id id
                                        :name name)))
    (when file-pathname
      (setf (subcircuit-class-file-pathname return-value) file-pathname))
    (when nodes-list
      (setf (subcircuit-class-nodes-list return-value) nodes-list))
    return-value))

(defmethod update ((object model-class) &rest parameters &key id name class file-pathname parameters-list function-name value)
  (declare (ignorable parameters id name class file-pathname parameters-lisr function-name value))
  (let ((return-value (call-next-method object
                                        :id id
                                        :name name)))
    (when class
      (setf (model-class-class return-value) class))
    (when file-pathname
      (setf (model-class-file-pathname return-value) file-pathname))
    (when parameters-list
      (setf (model-class-parameters-list return-value) parameters-list))
    (when function-name
      (setf (model-class-function-name return-name) function-name))
    (when value
      (setf (model-class-value return-value) value))
    return-value))

(defmethod update ((object probe-class) &rest parameters &key id name class elements-list nodes-list)
  (declare (ignorable parameters id name class elements-list nodes-list))
  (let ((return-value (call-next-method object
                                        :id id
                                        :name name)))
    (when class
      (setf (probe-class-class return-value) class))
    (when elements-list
      (setf (probe-class-elements-list return-value) elements-list))
    (when nodes-list
      (setf (probe-class-nodes-list return-value) nodes-list))
    return-value))

(defmethod update ((object initial-condition-class) &rest parameters &key id name target-name value)
  (declare (ignorable parameters id name target-name value))
  (let ((return-value (call-next-method object
                                        :id id
                                        :name name)))
    (when target-name
      (setf (initial-condition-class-target-name return-value) target-name))
    (when value
      (setf (initial-condition-class-value return-value) value))
    return-value))

;;;
;;; add a node to a netlist
;;;

(defun add-node (netlist node)
  (let ((elements-list (exclude (where :class-type 'node-class) (netlist-class-elements-list netlist)))
	(nodes-list (select (where :class-type 'node-class) (netlist-class-elements-list netlist)))
	(return-value netlist))
    (setq nodes-list (append nodes-list (list node)))
    (setf (netlist-class-elements-list return-value) (append elements-list nodes-list))
    return-value))

;;;
;;; merge element nodes to connect to the target netlist
;;;

(defmethod merge-element ((object probe-class) nodes-pairs &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((source-element object))
    (when debug-mode
      (format output
              "~%Merging ~a -> "
              (sexpify source-element))
      (finish-output output))
    (dolist (nodes-pair nodes-pairs)
      (when (voltage-probe-class-p source-element)
	(setf (probe-class-nodes-list source-element) (substitute-if (second nodes-pair) #'(lambda (x)
											     (equalp x (first nodes-pair)))
                                                                     (probe-class-nodes-list source-element)))))
    (when debug-mode
      (format output
              "~a."
              (sexpify source-element))
      (finish-output output))
    source-element))

(defmethod merge-element ((object model-class) nodes-pairs &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((source-element object))
    (when debug-mode
      (format output
              "~%Merging ~a -> "
              (sexpify source-element))
      (finish-output output))
    (dolist (source-element-probe (model-class-probes-list source-element))
      (setq source-element-probe (merge-element source-element-probe
                                                nodes-pairs
                                                :debug-mode debug-mode
                                                :output output)))
    (when debug-mode
      (format output
              "~a"
              (sexpify source-element))
      (finish-output output))
    source-element))

(defmethod merge-element ((object passive-class) nodes-pairs &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((source-element object))
    (when debug-mode
      (format output
              "~%Merging ~a -> "
              (sexpify source-element))
      (finish-output output))
    (dolist (nodes-pair nodes-pairs)
      (setf (passive-class-nodes-list source-element) (substitute-if (second nodes-pair) #'(lambda (x)
										             (equalp x (first nodes-pair)))
                                                                     (passive-class-nodes-list source-element)))
      (when (passive-class-model source-element)
	(setf (passive-class-model source-element) (merge-element (passive-class-model source-element)
                                                                  nodes-pairs
                                                                  :debug-mode debug-mode
                                                                  :output output))))
    (when debug-mode
      (format output
              "~a."
              (sexpify source-element))
      (finish-output output))
    source-element))

(defmethod merge-element ((object coupling-class) nodes-pairs &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((source-element object))
    (when debug-mode
      (format output
              "~%Merging ~a -> "
              (sexpify source-element))
      (finish-output output))
    (dolist (coupling-element (coupling-class-elements-list source-element))
      (setq coupling-element (merge-element coupling-element
                                            nodes-pairs
                                            :debug-mode debug-mode
                                            :output output)))
    (when (coupling-class-model source-element)
      (setf (coupling-class-model source-element) (merge-element (coupling-class-model source-element)
                                                                 nodes-pairs
                                                                 :debug-mode debug-mode
                                                                 :output output)))
    (when debug-mode
      (format output
              "~a"
              (sexpify source-element))
      (finish-output output))
    source-element))

(defmethod merge-element ((object subcircuit-class) nodes-pairs &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((source-element object))
    (when debug-mode
      (format output
              "~%Merging ~a -> "
              (sexpify source-element))
      (finish-output output))
    (dolist (nodes-pair nodes-pairs)
      (setf (subcircuit-class-nodes-list source-element) (substitute-if (second nodes-pair) #'(lambda (x)
										                (equalp x (first nodes-pair)))
                                                                        (subcircuit-class-nodes-list source-element))))
    (when debug-mode
      (format output
              "~a."
              (sexpify source-element))
      (finish-output output))
    source-element))

;;;
;;; connect netlist2 to netlist1 (conversely netlist1 to netlist2) resulting a netlist
;;; user must provide a node to node matrix e.g.:
;;;               (("netlist1:N1" "netlist2:N1") ("netlist1:N2" "netlist2:N10"))
;;;

(defun connect (source-netlist target-netlist nodes-pairs &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((return-value target-netlist)
	(source-elements-list (exclude (where :class-type 'node-class)
                                       (netlist-class-elements-list source-netlist)))
	(target-elements-list (exclude (where :class-type 'node-class)
                                       (netlist-class-elements-list target-netlist)))
	(source-nodes-list (select (where :class-type 'node-class)
                                   (netlist-class-elements-list source-netlist)))
	(target-nodes-list (select (where :class-type 'node-class)
                                   (netlist-class-elements-list target-netlist))))
    (when debug-mode
      (format output
              "~%~%Connecting ~a to ~a with connections ~a."
              (element-class-name source-netlist)
              (element-class-name target-netlist)
              nodes-pairs)
      (finish-output output))
    (dolist (nodes-pair nodes-pairs)
      (setq source-nodes-list (remove-if #'(lambda (x)
					     (equalp (first nodes-pair) (element-class-name x)))
                                         source-nodes-list)))
    (when debug-mode
      (format output
              "~%~%Nodes to merge:~%~a"
              (mapcar #'sexpify source-nodes-list))
      (finish-output output))
    (setq target-nodes-list (append target-nodes-list source-nodes-list))
    (dolist (source-element source-elements-list)
      (setq source-element (merge-element source-element nodes-pairs
                                          :debug-mode debug-mode
                                          :output output))
      (setq target-elements-list (append target-elements-list
                                         (list source-element))))
    (setf (netlist-class-elements-list return-value) target-elements-list)
    (setf (netlist-class-elements-list return-value) (append (netlist-class-elements-list return-value)
                                                             target-nodes-list))
    return-value))

;;;
;;; read a netlist from a file
;;;

(defun read-netlist (file-pathname &rest parameters &key (debug-mode nil) (output *standard-output*))
  "Read a complete netlist from an existing file."
  (declare (ignorable parameters debug-mode output))
  (handler-case
      (let ((*package* (find-package :circuit-solver)))
	(when debug-mode
	  (format output
                  "~a~%"
                  file-pathname)
          (finish-output output))
	(with-open-file (input-file-stream file-pathname :direction :input :if-does-not-exist nil)
	  (if input-file-stream
	      (objectify (read input-file-stream))
	      (error 'file-not-found-error
                     :file-pathname file-pathname))))
    (file-not-found-error (condition)
      (format *error-output*
              "~%file ~a does not exist.~%"
              (file-pathname condition))
      (finish-output *error-output*)
      nil)))

;;;
;;; write a netlist in a file
;;;

(defun write-netlist (file-pathname netlist)
  "Write a complete netlist in a file."
  (with-open-file (out file-pathname :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print (mapcar #'sexpify netlist-class)
	     out))))

;;;
;;; find node position in nodes list
;;;

(defun find-node-position (name nodes-list &rest parameters &key (lock nil))
  (declare (ignorable parameters lock))
  (if lock
      (bt:with-recursive-lock-held (lock)
        (position name nodes-list
	          :key (function element-class-name)
	          :test (function string-equal)))
      (position name nodes-list
	        :key (function element-class-name)
	        :test (function string-equal))))

(defun merge-names (parent child)
  (concatenate 'string parent ":" child))

;;;
;;; rename netlist
;;;

(defun find-node-occurrences (netlist node-name)
  (let ((return-value nil)
	(elements-list (netlist-class-elements-list netlist)))
    (dolist (element elements-list)
      (dolist (subelement (element-with-node element node-name))
	(push subelement return-value)))
    return-value))

;;;
;;; include all subcircuit in a netlist
;;;

(defun include-subcircuits (netlist &rest parameters &key (verbose nil) (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters verbose debug-mode output))
  (handler-case
      (let ((return-value netlist)
	    (subcircuit-calls-list nil))
	(when verbose
	  (format output
                  "~%Including subcircuits.")
          (finish-output output))
	(loop do
	  (setq subcircuit-calls-list (select (where :class-type 'subcircuit-class)
					      (netlist-class-elements-list return-value)))
	  (when subcircuit-calls-list
	    (when debug-mode
	      (format output
                      "~%~%Subcircuit calls: ~a~%"
                      (mapcar #'sexpify subcircuit-calls-list))
              (finish-output output))
	    (let ((subcircuits-list (mapcar #'(lambda (x)
						(read-netlist (subcircuit-class-file-pathname x)
                                                              :debug-mode debug-mode
                                                              :output output))
                                            subcircuit-calls-list))
		  (subcircuit nil)
		  (i 0))
	      (dolist (subcircuit-call subcircuit-calls-list)
		(setq subcircuit (nth i subcircuits-list))
		(when debug-mode
		  (format output
                          "~%~%Original subcircuit netlist:~%~a"
                          (sexpify subcircuit))
                  (finish-output output))
		(setq subcircuit (rename-netlist-element subcircuit
                                                         (element-class-name subcircuit-call)
                                                         :debug-mode debug-mode
                                                         :output output))
		(let ((subcircuit-nodes-list (select (where :class-type 'node-class)
						     (netlist-class-elements-list subcircuit))))
		  (when (> (length (subcircuit-class-nodes-list subcircuit-call))
                           (length subcircuit-nodes-list))
		    (error 'wrong-subcircuit-nodes-list-error
                           :subcircuit-name (element-class-name subcircuit)
			   :actual-nodes-count (length subcircuit-nodes-list)
			   :needed-nodes-count (length (subcircuit-class-nodes-list subcircuit-call))))
		  (let ((connection-nodes-list nil))
		    (loop
                      for i from 0 below (length (subcircuit-class-nodes-list subcircuit-call))
                      do
			 (setq connection-nodes-list (append connection-nodes-list (list (list (element-class-name (nth i subcircuit-nodes-list))
											       (nth i (subcircuit-class-nodes-list subcircuit-call)))))))
		    (when debug-mode
		      (format output
                              "~%~%Connections nodes pairs ~a"
                              connection-nodes-list)
                      (finish-output output))
		    (setq return-value (connect subcircuit
                                                return-value
                                                connection-nodes-list
                                                :debug-mode debug-mode
                                                :output output))
		    (setf (netlist-class-elements-list return-value) (exclude (where :name (element-class-name subcircuit-call)
                                                                                     :class-type 'subcircuit-class)
									      (netlist-class-elements-list return-value)))
		    (when debug-mode
		      (format output
                              "~%~%Resulting netlist:~%~s~%"
                              (sexpify return-value))
                      (finish-output output))))
		(incf i))))
	      until (eql subcircuit-calls-list nil))
	(when verbose
	  (format output
                  " Done!")
          (finish-output output))
	return-value)
    (wrong-subcircuit-nodes-list-error (condition)
      (format *error-output*
              "~%Subcircuit ~a has got ~a instead of ~a in the include command."
              (subcircuit-name condition)
              (actual-nodes-count condition)
              (needed-nodes-count condition))
      (finish-output *error-output*))))

(defun check-netlist (netlist &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (handler-case
      (let* ((elements-list (exclude (where :class-type 'node-class)
                                     (netlist-class-elements-list netlist)))
	     (nodes-list (select (where :class-type 'node-class)
                                 (netlist-class-elements-list netlist)))
	     (error-found 0))
	(when (< (length nodes-list) 2)
	  (format output
                  "Less than two nodes for netlist ~a.~%"
                  (element-class-name netlist))
          (finish-output output)
	  (setq error-found 1))
	(let ((reference-nodes (select (list (where :class "reference")
					     (where :class "gnd")
					     (where :class "0"))
                                       nodes-list)))
	  (cond
	    ((eql (length reference-nodes) 0)
	     (format output
                     "~%No reference node in netlist ~a."
                     (element-class-name netlist))
             (finish-output output)
	     (setq error-found 2))
	    ((> (length reference-nodes) 1)
	     (format output
                     "~%Too many reference nodes in netlist ~a: ~a."
                     (element-class-name netlist)
                     (mapcar #'sexpify reference-nodes))
             (finish-output output)
	     (setq error-found 3))))
	(dolist (element elements-list)
	  (when (> (length (select (where :name (element-class-name element)) elements-list)) 1)
	    (format output
                    "~%Object ~a defined more than once."
                    (element-class-name element))
            (finish-output output)
	    (setq error-found 4))
	  (typecase element
	    (coupling-class
	     (when (< (length (coupling-class-elements-list element)) 2)
	       (error 'wrong-number-of-elements-for-coupling-error
                      :element-name (element-class-name element))
	       (setq error-found 5))
	     (unless (eql (/ (* (length (coupling-class-elements-list element))
				(1- (length (coupling-class-elements-list element))))
                             2)
			  (grid:dim0 (coupling-class-value element)))
	       (error 'mismatched-number-of-coupling-values-vs-coupling-inductances-error
                      :coupling-name (element-class-name element)))
	     (unless (check-objects (coupling-class-elements-list element)
                                    (list (where :class "inductance")
					  (where :class "capacitance")))
	       (error 'invalid-passive-element-in-coupling
                      :coupling-name (element-class-name element))))))
	error-found)
    (wrong-number-of-elements-for-coupling-error (condition)
      (format *error-output*
              "~%Less than coupling elements in ~a.~%"
              (element-name condition))
      (finish-output *error-output*)
      nil)
    (mismatched-number-of-coupling-values-vs-coupling-inductances-error (condition)
      (format *error-output*
              "~%Coupling ~a mismatched number of coupling values vs coupling inductances."
              (coupling-name condition))
      (finish-output *error-output*)
      nil)
    (invalid-passive-element-in-coupling (condition)
      (format *error-output*
              "~%Invalid passive element in coupling ~a~%"
              (coupling-name condition))
      (finish-output *error-output*)
      nil)))

;;;
;;; create K or Y vector
;;;

(defun create-k-y-vector (netlist &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((elements-list (select (list (where :class-type 'source-class)
				     (where :class-type 'passive-class))
                               netlist))
	(couplings-list (select (where :class-type 'coupling-class) netlist))
	(nodes-list (exclude (list (where :class "reference")
				   (where :class "REFERENCE")
				   (where :class "gnd")
				   (where :class "GND")
				   (where :class "0"))
                             (select (where :class-type 'node-class)
                                     netlist)))
	(k-y-vector nil)
	(k-y-rows 0))
    (when debug-mode
      (format output
              "~%Creating K or Y vector: ")
      (finish-output output))
    (setq k-y-rows (length nodes-list))
    (incf k-y-rows (length elements-list))
    (dolist (coupling couplings-list)
      (incf k-y-rows (length (coupling-class-elements-list coupling))))
    (when (> k-y-rows 0)
      (setq k-y-vector (grid:make-foreign-array 'double-float
                                                :dimensions k-y-rows
                                                :initial-element 0d0)))
    (when debug-mode
      (if (> k-y-rows 0)
	  (format output "(~a)." k-y-rows)
	  (format output "ø."))
      (finish-output output))
    k-y-vector))

;;;
;;; create the p-matrix once for all
;;;

(defun create-p-matrix (netlist &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((nodes-list (select (where :class-type 'node-class)
                            netlist))
	(couplings-list (select (where :class-type 'coupling-class)
                                netlist))
	(p-matrix nil)
	(p-rows 0)
	(p-cols 0))
    (when debug-mode
      (format output
              "~%Creating P matrix: ")
      (finish-output output))
    (setq p-rows (1- (length nodes-list)))
    (setq p-cols (length (select (list (where :class-type 'source-class)
				       (where :class-type 'passive-class))
                                 netlist)))
    (dolist (coupling couplings-list)
      (incf p-cols (length (coupling-class-elements-list coupling))))
    (when (and (> p-cols 0) (> p-rows 0))
      (setq p-matrix (grid:make-foreign-array 'double-float
                                              :dimensions (list p-rows p-cols)
                                              :initial-element 0d0)))
    (when debug-mode
      (if (and (> p-cols 0) (> p-rows 0))
          (format output "P(~a x ~a)." p-rows p-cols)
	  (format output "P = ø."))
      (finish-output output))
    p-matrix))

;;;
;;; create g-point matrix
;;;

(defun create-g-c-matrix (netlist &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let* ((nodes-list (select (where :class-type 'node-class)
                             netlist))
	 (elements-list (select (where :class-type 'passive-class)
                                netlist))
	 (couplings-list (select (where :class-type 'coupling-class)
                                 netlist))
	 (g-c-matrix nil)
	 (g-c-rows 0)
	 (g-c-cols 0))
    (when debug-mode
      (format output
              "~%Creating G or C matrix: ")
      (finish-output output))
    (setq g-c-cols (1- (length nodes-list)))
    (setq g-c-rows (length elements-list))
    (dolist (coupling couplings-list)
      (incf g-c-rows (length (coupling-class-elements-list coupling))))
    (when (and (> g-c-rows 0) (> g-c-cols 0))
      (setq g-c-matrix (grid:make-foreign-array 'double-float
                                                :dimensions (list g-c-rows g-c-cols)
                                                :initial-element 0d0)))
    (when debug-mode
      (if (and (> g-c-rows 0)
               (> g-c-cols 0))
	  (format output
                  "(~a, ~a)."
                  g-c-rows
                  g-c-cols)
	  (format output
                  "ø."))
      (finish-output output))
    g-c-matrix))

;;;
;;; create l-matrix for synchronous machine abduction
;;;

(defun create-r-l-matrix (netlist &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let* ((elements-list (select (where :class-type 'passive-class) netlist))
	 (couplings-list (select (where :class-type 'coupling-class) netlist))
	 (r-l-matrix nil)
	 (r-l-rows 0)
	 (r-l-cols 0))
    (when debug-mode
      (format output
              "~%Creating R or L matrix: ")
      (finish-output output))
    (setq r-l-cols (length (select (list (where :class-type 'passive-class)
					 (where :class-type 'source-class))
                                   netlist)))
    (setq r-l-rows (length elements-list))
    (dolist (coupling couplings-list)
      (incf r-l-rows (length (coupling-class-elements-list coupling)))
      (incf r-l-cols (length (coupling-class-elements-list coupling))))
    (when (and (> r-l-rows 0)
               (> r-l-cols 0))
      (setq r-l-matrix (grid:make-foreign-array 'double-float
                                                :dimensions (list r-l-rows r-l-cols)
                                                :initial-element 0d0)))
    (when debug-mode
      (if (and (> r-l-rows 0)
               (> r-l-cols 0))
	  (format output
                  "(~a, ~a)."
                  r-l-rows
                  r-l-cols)
	  (format output
                  "ø."))
      (finish-output output))
    r-l-matrix))

;;;
;;; create a beautiful Si matrix
;;;

(defun create-si-matrix (netlist &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let* ((elements-list (select (list (where :class-type 'passive-class)
				      (where :class-type 'source-class))
                                netlist))
	 (current-sources-list (select (list (where :class-type 'source-class
                                                    :class "current-source"))
                                       netlist))
	 (couplings-list (select (where :class-type 'coupling-class)
                                 netlist))
	 (si-matrix nil)
	 (si-rows 0)
	 (si-cols 0))
    (when debug-mode
      (format output "~%Creating Si matrix: ")
      (finish-output output))
    (setq si-rows (length current-sources-list))
    (setq si-cols (length elements-list))
    (dolist (coupling couplings-list)
      (incf si-cols (length (coupling-class-elements-list coupling))))
    (when (and (> si-rows 0) (> si-cols 0))
      (setq si-matrix (grid:make-foreign-array 'double-float :dimensions (list si-rows si-cols) :initial-element 0d0)))
    (when debug-mode
      (if (and (> si-rows 0) (> si-cols 0))
	  (format output "Si(~a, ~a)." si-rows si-cols)
	  (format output "Si = ø."))
      (finish-output output))
    si-matrix))

;;;
;;; create the Sv matrix for your joy
;;;

(defun create-sv-matrix (netlist &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((nodes-list (select (where :class-type 'node-class)
                            netlist))
	(voltage-sources-list (select (list (where :class-type 'source-class
                                                   :class "voltage-source"))
                                      netlist))
	(sv-matrix nil)
	(sv-rows 0)
	(sv-cols 0))
    (when debug-mode
      (format output "~%Creating Sv matrix: ")
      (finish-output output))
    (setq sv-rows (length voltage-sources-list))
    (setq sv-cols (1- (length nodes-list)))
    (when (and (> sv-rows 0) (> sv-cols 0))
      (setq sv-matrix (grid:make-foreign-array 'double-float
                                               :dimensions (list sv-rows sv-cols)
                                               :initial-element 0d0)))
    (when debug-mode
      (if (and (> sv-rows 0) (> sv-cols 0))
	  (format output "Sv(~a, ~a)." sv-rows sv-cols)
	  (format output "Sv = ø."))
      (finish-output output))
    sv-matrix))

;;
;; create know factor matrix (current sources part).
;;

(defun create-ki-vector (netlist &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((current-sources-list (select (list (where :class-type 'source-class
                                                   :class "current-source"))
                                      netlist))
	(ki-vector nil)
	(ki-rows 0))
    (when debug-mode
      (format output "~%Creating Ki matrix: ")
      (finish-output output))
    (setq ki-rows (length current-sources-list))
    (when (> ki-rows 0)
      (setq ki-vector (grid:make-foreign-array 'double-float :dimensions ki-rows :initial-element 0d0)))
    (when debug-mode
      (if (> ki-rows 0)
	  (format output "Ki(~a)." ki-rows)
	  (format output "Ki = ø."))
      (finish-output output))
    ki-vector))

;;
;; create the kv part of "sto cazzo".
;;

(defun create-kv-vector (netlist &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((voltage-sources-list (select (list (where :class-type 'source-class
                                                   :class "voltage-source"))
                                      netlist))
	(kv-vector nil)
	(kv-rows 0))
    (when debug-mode
      (format output "~%Creating Kv matrix: ")
      (finish-output output))
    (setq kv-rows (length voltage-sources-list))
    (when (> kv-rows 0)
      (setq kv-vector (grid:make-foreign-array 'double-float
                                               :dimensions kv-rows
                                               :initial-element 0d0)))
    (when debug-mode
      (if (> kv-rows 0)
	  (format output "Kv(~a)." kv-rows)
	  (format output "Kv = ø."))
      (finish-output output))
    kv-vector))

;;
;; update functions stuff
;;

(defun update-p-matrix (p-matrix netlist &rest parameters &key
                                                            (lock nil)
                                                            (debug-mode nil)
                                                            (output *standard-output*))
  (declare (ignorable parameters lock debug-mode output))
  (let ((nodes-list (select (where :class-type 'node-class)
                            netlist
                            :lock lock))
	(elements-list (select (list (where :class-type 'source-class)
				     (where :class-type 'passive-class)
				     (where :class-type 'coupling-class))
                               netlist
                               :lock lock))
	(i 0)
	(j 0))
    (when debug-mode
      (format output
              "~%~%---- update-p-matrix ---- ~%~%")
      (format output
              "Updating P[~a x ~a].~%"
              (grid:dim0 p-matrix)
              (grid:dim1 p-matrix))
      (finish-output output))
    (dolist (node (exclude (list (where :class "reference")
				 (where :class "gnd")
				 (where :class "0"))
                           nodes-list
                           :lock lock))
      (setq j 0)
      (let ((node-name (element-class-name node)))
	(dolist (element elements-list)
	  (typecase element
	    (source-class
	     (let ((element-node-names-list (source-class-nodes-list element)))
	       (when (string-equal node-name (first element-node-names-list))
		 (setf (grid:gref p-matrix i j) -1d0)
		 (when debug-mode
		   (format output
                           "P(~a, ~a) = ~a.~%"
                           i
                           j
                           (grid:gref p-matrix i j))
                   (finish-output output)))
	       (when (string-equal node-name (second element-node-names-list))
		 (setf (grid:gref p-matrix i j) +1d0)))
	     (incf j))
	    (passive-class
	     (let ((element-node-names-list (passive-class-nodes-list element)))
	       (when (string-equal node-name (first element-node-names-list))
		 (setf (grid:gref p-matrix i j) -1d0))
	       (when (string-equal node-name (second element-node-names-list))
		 (setf (grid:gref p-matrix i j) +1d0)))
	     (incf j))
	    (coupling-class
	     (let ((inductances-list (coupling-class-elements-list element)))
	       (dolist (inductance inductances-list)
		 (let ((element-node-names-list (passive-class-nodes-list inductance)))
		   (when (string-equal node-name (first element-node-names-list))
		     (setf (grid:gref p-matrix i j) -1d0)
		     (when debug-mode
		       (format output
                               "P(~a, ~a) = ~a.~%"
                               i
                               j
                               (grid:gref p-matrix i j))
                       (finish-output output)))
		   (when (string-equal node-name (second element-node-names-list))
		     (setf (grid:gref p-matrix i j) +1d0)))
		 (incf j)))))))
      (incf i))
    (when debug-mode
      (format output
              "P =~%~a~%"
              p-matrix)
      (finish-output output))
    p-matrix))

;;;
;;; update R matrix
;;;

(defun update-r-matrix (r-matrix netlist &rest parameters &key
                                                            (lock nil)
                                                            (debug-mode nil)
                                                            (output *standard-output*))
  (declare (ignorable parameters lock debug-mode output))
  (let ((elements-list (select (list (where :class-type 'passive-class)
				     (where :class-type 'coupling-class)
				     (where :class-type 'source-class))
                               netlist
                               :lock lock))
	(i 0)
	(j 0))
    (when debug-mode
      (format output
              "~%~%---- update-r-matrix ----~%~%")
      (format output
              "Updating R[~a x ~a].~%"
              (grid:dim0 r-matrix)
              (grid:dim1 r-matrix))
      (finish-output output))
    (dolist (element elements-list)
      (typecase element
	(coupling-class
	 (setq j (+ j
                    (length (coupling-class-elements-list element))))
	 (setq i (+ i
                    (length (coupling-class-elements-list element)))))
	(passive-class
	 (cond
	   ((resistance-class-p element)
	    (setf (grid:gref r-matrix i j) (- (passive-class-value element))))
	   ((or (conductance-class-p element)
		(capacitance-class-p element))
	    (setf (grid:gref r-matrix i j) -1d0)))
	 (incf i)
	 (incf j))
	(source-class
	 (incf j))))
    (when debug-mode
      (format output
              "R =~%~a~%"
              r-matrix)
      (finish-output output))
    r-matrix))

(defmethod submatrix-update ((element passive-class) i matrix &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (handler-case
      (let ((element-node-names-list (passive-class-nodes-list element))
	    (j 0)
	    (k 0))
	(cond
	  ((or (resistance-class-p element)
	       (inductance-class-p element))
	   (setq k 0)
	   (dolist (element-node-name element-node-names-list)
	     (let ((found-node (find-element (where :name element-node-name) nodes-list)))
	       (unless found-node
		 (error 'no-such-node-for-element-error
                        :node-name element-node-name
                        :element-name (element-class-name element)))
	       (setq j (find-node-position element-node-name (exclude (list (where :class "reference")
									    (where :class "gnd")
									    (where :class "0"))
                                                                      nodes-list)))
	       (unless (reference-class-node-p found-node)
		 (setf (grid:gref g-matrix i j) (expt -1d0 k)))
	       (incf k))))
	  ((conductance-class-p element)
	   (setq k 0)
	   (dolist (element-node-name element-node-names-list)
	     (let ((found-node (find-element (where :name element-node-name) nodes-list)))
	       (unless found-node
		 (error 'no-such-node-for-element-error
                        :node-name element-node-name
                        :element-name (element-class-name element)))
	       (setq j (find-node-position element-node-name (exclude (list (where :class "reference")
									    (where :class "gnd")
									    (where :class "0"))
                                                                      nodes-list)))
	       (unless (reference-class-node-p found-node)
		 (setf (grid:gref g-matrix i j) (* (expt -1d0 k) (passive-class-value element))))
	       (incf k)))))
	matrix)
    (no-such-node-for-element-error (condition)
      (format *error-output*
              "No such node ~a for element ~a.~%"
              (node-name condition)
              (element-name condition))
      (finish-output *error-output*)
      nil)))

(defmethod submatrix-update ((element coupling-class) i matrix &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (handler-case
      (let ((element-node-names-list (passive-class-nodes-list element))
	    (j 0)
	    (k 0))
	(dolist (coupling-element (coupling-class-elements-list element))
	  (typecase coupling-element
	    (passive-class
	     (let ((element-node-names-list (passive-class-nodes-list coupling-element)))
	       (unless (and (inductance-class-p coupling-element)
			    (capacitance-class-p coupling-element))
		 (error 'mismatched-coupling-element
                        :element-name (element-class-name element)))
	       (setq k 0)
	       (dolist (element-node-name element-node-names-list)
		 (let ((found-node (find-element (where :name element-node-name) nodes-list)))
		   (unless found-node
		     (error 'no-such-node-for-element
                            :node-name element-node-name
                            :element-name (element-class-name coupling-element)))
		   (setq j (find-node-position element-node-name (exclude (list (where :class "reference")
										(where :class "gnd")
										(where :class "0"))
                                                                          nodes-list)))
		   (unless (reference-class-node-p found-node)
		     (setf (grid:gref g-matrix i j) (expt -1d0 k)))
		   (incf k)))))
	    (t
	     (error 'mismatched-coupling-element
                    :element-name (element-class-name element)))))
	matrix)
    (no-such-node-for-element-error (condition)
      (format *error-output*
              "No such node ~a for element ~a.~%"
              (node-name condition)
              (element-name condition))
      (finish-output *error-output*)
      nil)
    (mismatched-coupling-element (condition)
      (format *error-output*
              "Coupling ~a must contains only inductances or capacitances.~%"
              (element-name condition))
      (finish-output *error-output*)
      nil)))

(defun apply-selectors (object selectors)
  (cond
    ((null selectors)
     nil)
    ((listp selectors)
     (or (apply-selectors object (first selectors))
	 (apply-selectors object (rest selectors))))
    (t
     (funcall selectors object))))

(defun check-objects (objects selectors)
  (cond
    ((null objects)
     t)
    ((listp objects)
     (and (check-objects (rest objects) selectors)
	  (apply-selectors (first objects) selectors)))
    (t
     (apply-selectors (first objects) selectors))))

;;
;; update G matrix
;;

(defun update-g-matrix (g-matrix netlist &rest parameters &key
                                                            (lock nil)
                                                            (debug-mode nil)
                                                            (output *standard-output*))
  (declare (ignorable parameters lock debug-mode output))
  (handler-case
      (let ((elements-list (select (list (where :class-type 'source-class)
					 (where :class-type 'passive-class)
					 (where :class-type 'coupling-class))
                                   netlist
                                   :lock lock))
	    (nodes-list (select (where :class-type 'node-class)
                                netlist
                                :lock lock))
	    (i 0)
	    (j 0)
	    (k 0))
	(when debug-mode
	  (format output
                  "~%~%---- update-g-matrix ----~%~%")
	  (format output
                  "Updating G[~a x ~a].~%"
                  (grid:dim0 g-matrix)
                  (grid:dim1 g-matrix))
          (finish-output output))
	(dolist (element elements-list)
	  (typecase element
	    (passive-class
	     (let ((element-node-names-list (passive-class-nodes-list element)))
	       (cond
		 ((or (resistance-class-p element)
		      (inductance-class-p element))
		  (setq k 0)
		  (dolist (element-node-name element-node-names-list)
		    (let ((found-node (find-element (where :name element-node-name)
                                                    nodes-list
                                                    :lock lock)))
		      (unless found-node
			(error 'no-such-node-for-element-error :node-name element-node-name :element-name (element-class-name element)))
		      (setq j (find-node-position element-node-name (exclude (list (where :class "reference")
										   (where :class "gnd")
										   (where :class "0"))
                                                                             nodes-list
                                                                             :lock lock)))
		      (unless (reference-class-node-p found-node)
			(setf (grid:gref g-matrix i j) (expt -1d0 k)))
		      (incf k))))
		 ((conductance-class-p element)
		  (setq k 0)
		  (dolist (element-node-name element-node-names-list)
		    (let ((found-node (find-element (where :name element-node-name)
                                                    nodes-list
                                                    :lock lock)))
		      (unless found-node
			(error 'no-such-node-for-element-error
                               :node-name element-node-name
                               :element-name (element-class-name element)))
		      (setq j (find-node-position element-node-name (exclude (list (where :class "reference")
										   (where :class "gnd")
										   (where :class "0"))
                                                                             nodes-list
                                                                             :lock lock)))
		      (unless (reference-class-node-p found-node)
			(setf (grid:gref g-matrix i j) (* (expt -1d0 k) (passive-class-value element))))
		      (incf k))))))
	     (incf i))
	    (coupling-class
	     (dolist (coupling-element (coupling-class-elements-list element))
	       (typecase coupling-element
		 (passive-class
		  (let ((element-node-names-list (passive-class-nodes-list coupling-element)))
		    (unless (or (inductance-class-p coupling-element)
				(capacitance-class-p coupling-element))
		      (error 'mismatched-coupling-element
                             :element-name (element-class-name element)))
		    (setq k 0)
		    (dolist (element-node-name element-node-names-list)
		      (let ((found-node (find-element (where :name element-node-name)
                                                      nodes-list
                                                      :lock lock)))
			(unless found-node
			  (error 'no-such-node-for-element
                                 :node-name element-node-name
                                 :element-name (element-class-name coupling-element)))
			(setq j (find-node-position element-node-name (exclude (list (where :class "reference")
										     (where :class "gnd")
										     (where :class "0"))
                                                                               nodes-list
                                                                               :lock lock)))
			(unless (reference-class-node-p found-node)
			  (setf (grid:gref g-matrix i j) (expt -1d0 k)))
			(incf k)))))
		 (t
		  (error 'mismatched-coupling-element
                         :element-name (element-class-name element))))
	       (incf i)))))
	(when debug-mode
	  (format output
                  "G =~%~a~%"
                  g-matrix)
          (finish-output output))
	g-matrix)
    (no-such-node-for-element-error (condition)
      (format *error-output*
              "No such node ~a for element ~a.~%"
              (node-name condition)
              (element-name condition))
      (finish-output *error-output*)
      nil)
    (mismatched-coupling-element (condition)
      (format *error-output*
              "Coupling ~a must contains only inductances or capacitances.~%"
              (element-name condition))
      (finish-output *error-output*)
      nil)))

;;;
;;; update Si matrix
;;;

(defun update-si-matrix (si-matrix netlist &rest parameters &key
                                                              (lock nil)
                                                              (debug-mode nil)
                                                              (output *standard-output*))
  (declare (ignorable parameters lock debug-mode output))
  (handler-case
      (let ((elements-list (select (list (where :class-type 'passive-class)
					 (where :class-type 'coupling-class)
					 (where :class-type 'source-class))
                                   netlist
                                   :lock lock))
	    (i 0)
	    (j 0))
	(when debug-mode
	  (format output
                  "~%~%---- update-si-matrix ----~%~%")
	  (format output
                  "Updating Si[~a x ~a].~%"
                  (grid:dim0 si-matrix)
                  (grid:dim1 si-matrix))
          (finish-output output))
	(dolist (element elements-list)
	  (typecase element
	    (source-class
	     (when (current-source-class-p element)
	       (setf (grid:gref si-matrix i j) +1d0)
	       (incf i))
	     (incf j))
	    (coupling-class
	     (incf j (length (coupling-class-elements-list element))))
	    (passive-class
	     (incf j))))
	(when debug-mode
	  (format output
                  "Si =~%~a~%"
                  si-matrix)
          (finish-output output))
	si-matrix)))

;;;
;;; update Sv matrix
;;;

(defun update-sv-matrix (sv-matrix netlist &rest parameters &key
                                                              (lock nil)
                                                              (debug-mode nil)
                                                              (output *standard-output*))
  (declare (ignorable parameters lock debug-mode output))
  (handler-case
      (let ((sources-list (select (where :class-type 'source-class
                                         :class "voltage-source")
                                  netlist
                                  :lock lock))
	    (nodes-list (select (where :class-type 'node-class)
                                netlist
                                :lock lock))
	    (i 0)
	    (j 0)
	    (k 0))
	(when debug-mode
	  (format output
                  "~%~%---- update-sv-matrix ----~%~%")
	  (format output
                  "Updating Sv[~a x ~a].~%"
                  (grid:dim0 sv-matrix)
                  (grid:dim1 sv-matrix))
          (finish-output output))
	(dolist (source sources-list)
	  (setq k 0)
	  (dolist (node-name (source-class-nodes-list source))
	    (unless (reference-class-node-p (find-element (where :name node-name)
                                                          nodes-list
                                                          :lock lock))
	      (setq j (1- (find-node-position node-name nodes-list)))
	      (unless j
		(error 'no-such-node-for-element-error
                       :element-name (element-class-name source)
                       :node-name node-name))
	      (setf (grid:gref sv-matrix i j) (expt -1d0 k)))
	    (incf k))
	  (incf i))
	(when debug-mode
	  (format output
                  "Sv =~%~a~%"
                  sv-matrix)
          (finish-output output))
	sv-matrix)
    (no-such-node-for-element-error (condition)
      (format *error-output*
              "No such node ~a for element ~a.~%"
              (node-name condition)
              (element-name condition))
      (finish-output *error-output*)
      nil)))

(defun update-l-matrix (l-matrix netlist &rest parameters &key
                                                            (lock nil)
                                                            (debug-mode nil)
                                                            (output *standard-output*))
  (declare (ignorable parameters lock debug-mode output))
  (let ((elements-list (select (list (where :class-type 'passive-class)
				     (where :class-type 'coupling-class)
				     (where :class-type 'source-class))
                               netlist
                               :lock lock))
	(i 0)
	(j 0)
	(k 0)
	(p 0)
	(m-value 0d0))
    (when debug-mode
      (format output
              "~%~%---- update-l-matrix ----~%~%")
      (format output
              "Updating L[~a x ~a].~%"
              (grid:dim0 l-matrix)
              (grid:dim1 l-matrix))
      (finish-output output))
    (dolist (element elements-list)
      (typecase element
	(passive-class
	 (when (inductance-class-p element)
	   (setf (grid:gref l-matrix i j) (- (passive-class-value element))))
	 (incf i)
	 (incf j))
	(coupling-class
	 (setq k 0)
	 (setq m-value 1d0)
	 (dolist (coupling-element (coupling-class-elements-list element))
	   (setf (grid:gref l-matrix (+ k i) (+ k j)) (- (passive-class-value coupling-element)))
	   (incf k))
	 (when debug-mode
	   (format output
                   "~%Coupling coefficients vector ~a.~%Coupling dimension ~a."
                   (coupling-class-value element) k)
           (finish-output output))
	 (loop for ii from 0 below k do
	   (loop for jj from 0 below k do
	     (unless (eql ii jj)
	       (when debug-mode
		 (format output
                         "~%Mutual coupling (~a, ~a)."
                         ii
                         jj)
                 (finish-output output))
	       (if (> jj ii)
		   (setq p (/ (+ (* ii (- (* 2 k) ii 3)) (* 2 (- jj 1))) 2))
		   (setq p (/ (+ (* jj (- (* 2 k) jj 3)) (* 2 (- ii 1))) 2)))
	       (when debug-mode
		 (format output
                         "~%Coupling coefficient position ~a.~%i = ~a, ii = ~a, j = ~a, jj = ~a, M(~a, ~a) = "
                         p
                         i
                         ii
                         j
                         jj
                         (+ i ii)
                         (+ j jj))
                 (finish-output output))
	       (setq m-value (* (grid:gref (coupling-class-value element) p)
				(sqrt (* (abs (grid:gref l-matrix (+ i ii) (+ j ii)))
					 (abs (grid:gref l-matrix (+ i jj) (+ j jj)))))))
	       (when debug-mode
		 (format output
                         "~a."
                         m-value)
                 (finish-output output))
	       (setf (grid:gref l-matrix (+ i ii) (+ j jj)) (- m-value)))))
	 (incf i k)
	 (incf j k))
	(source-class
	 (incf j))))
    (when debug-mode
      (format output
              "~%L =~%~a~%"
              l-matrix)
      (finish-output output))
    l-matrix))

;;
;; update C matrix
;;

(defun update-c-matrix (c-matrix netlist &rest parameters &key
                                                            (lock nil)
                                                            (debug-mode nil)
                                                            (output *standard-output*))
  (declare (ignorable parameters lock debug-mode output))
  (let ((elements-list (select (list (where :class-type 'source-class)
				     (where :class-type 'passive-class)
				     (where :class-type 'coupling-class))
                               netlist
                               :lock lock))
	(nodes-list (exclude (list (where :class "reference")
				   (where :class "gnd")
				   (where :class "0"))
                             (select (where :class-type 'node-class)
                                     netlist
                                     :lock lock)
                             :lock lock))
	(i 0)
	(j 0)
	(k 0))
    (when debug-mode
      (format output
              "~%~%---- update-c-matrix ----~%~%")
      (format output
              "Updating C[ ~a x ~a ].~%"
              (grid:dim0 c-matrix)
              (grid:dim1 c-matrix))
      (finish-output output))
    (dolist (element elements-list)
      (typecase element
	(passive-class
	 (when (capacitance-class-p element)
	   (setq k 1)
	   (dolist (node-name (passive-class-nodes-list element))
	     (setq j (find-node-position node-name nodes-list))
	     (when j
	       (setf (grid:gref c-matrix i j) (* (expt -1d0 (1+ k)) (passive-class-value element))))
	     (incf k)))
	 (incf i))
	(coupling-class
	 (incf i (length (coupling-class-elements-list element))))))
    (when debug-mode
      (format output
              "C =~%~a~%"
              c-matrix)
      (finish-output output))
    c-matrix))

;;;
;;; Update Ki vector
;;;

(defun update-ki-vector (ki-vector netlist &rest parameters &key
                                                              (lock nil)
                                                              (debug-mode nil)
                                                              (output *standard-output*))
  (declare (ignorable parameters lock debug-mode output))
  (let ((current-sources-list (select (list (where :class-type 'source-class
                                                   :class "current-source"))
                                      netlist
                                      :lock lock))
	(i 0))
    (when debug-mode
      (format output
              "~%~%---- update-ki-vector ----~%~%")
      (format output
              "Updating Ki[ ~a ].~%"
              (grid:dim0 ki-vector))
      (finish-output output))
    (dolist (current-source current-sources-list)
      (let ((temp-value (source-class-value current-source)))
        (if temp-value
            (progn
              (setf (grid:gref ki-vector i) temp-value)
              (incf i))
            (error 'invalid-source-element
                   :source-name (element-class-name current-source)))))
    (when debug-mode
      (format output
              "Ki =~%~a~%"
              ki-vector)
      (finish-output output))
    ki-vector))

;;;
;;; Update Kv vector
;;;

(defun update-kv-vector (kv-vector netlist &rest parameters &key
                                                              (lock nil)
                                                              (debug-mode nil)
                                                              (output *standard-output*))
  (declare (ignorable parameters lock debug-mode output))
  (let ((voltage-sources-list (select (list (where :class-type 'source-class
                                                   :class "voltage-source"))
                                      netlist
                                      :lock lock))
	(i 0))
    (when debug-mode
      (format output
              "~%~%---- update-kv-vector ----~%~%")
      (format output
              "Updating Kv[ ~a ].~%"
              (grid:dim0 kv-vector))
      (finish-output output))
    (loop
      for voltage-source in voltage-sources-list
      do
         (let ((temp-value (source-class-value voltage-source)))
           (if temp-value
               (progn
                 (setf (grid:gref kv-vector i) temp-value)
                 (incf i))
               (error 'invalid-source-element
                      :source-name (element-class-name voltage-source)))))
    (when debug-mode
      (format output
              "Kv =~%~a~%"
              kv-vector)
      (finish-output output))
    kv-vector))

;;;
;;; Solution via matrices creation scheme and update
;;;
;;; 1 - create single matrices: P, R, Si, G and Sv for A;
;;; 2 - create single matrices: L and C for B;
;;; 3 - create single matrices: Ki and Kv for K;
;;; 3 - assign initial condition for every model in K (Ki and Kv);
;;; 4 - assign initial condition for every model in P, R, Si, G, Sv, L and C (A and B);
;;; 5 - for every iteration in time:
;;;     a - update models;
;;;     b - update P, R, Si, G, Sv, L and C;
;;;     c - update Ki and Kv
;;;     d - assemble A and B matrices;
;;;     e - assemble K matrix;
;;;     f - invert (B + h A);
;;;     g - assign D = (B + h A) ^ -1;
;;;     h - Ynew = D B Y + h D K;
;;;     i - store Ynew selected values in a file;
;;; 6 - stop
;;;

;;
;; assemble A, B and K matrix

(defun assemble-system (p-matrix r-matrix g-matrix si-matrix sv-matrix l-matrix c-matrix ki-vector kv-vector &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((a-matrix nil)
	(b-matrix nil)
	(k-vector nil))
    (when debug-mode
      (format output
              "Assembling A, B matrices and K vector.~%")
      (finish-output output))
    (when (or p-matrix r-matrix g-matrix l-matrix c-matrix)
      (setq a-matrix (grid:concatenate-grids p-matrix
					     (grid:make-foreign-array 'double-float
                                                                      :dimensions (list (grid:dim0 p-matrix)
											(grid:dim1 g-matrix))
                                                                      :initial-element 0d0)
                                             :axis 1))
      (setq a-matrix (grid:concatenate-grids a-matrix
					     (grid:concatenate-grids r-matrix
                                                                     g-matrix
                                                                     :axis 1)
                                             :axis 0))
      (when si-matrix
	(setq a-matrix (grid:concatenate-grids a-matrix
					       (grid:concatenate-grids si-matrix
								       (grid:make-foreign-array 'double-float
                                                                                                :dimensions (list (grid:dim0 si-matrix)
														  (grid:dim1 g-matrix))
                                                                                                :initial-element 0d0)
                                                                       :axis 1)
                                               :axis 0)))
      (when sv-matrix
	(setq a-matrix (grid:concatenate-grids a-matrix
					       (grid:concatenate-grids (grid:make-foreign-array 'double-float
                                                                                                :dimensions (list (grid:dim0 sv-matrix)
														  (grid:dim1 p-matrix))
                                                                                                :initial-element 0d0)
                                                                       sv-matrix
                                                                       :axis 1)
                                               :axis 0)))
      (when debug-mode
	(format output
                "~%A = ~a"
                a-matrix)
        (finish-output output))
      (setq b-matrix (grid:make-foreign-array 'double-float
                                              :dimensions (list (grid:dim0 p-matrix)
								(+ (grid:dim1 p-matrix)
								   (grid:dim1 g-matrix)))
                                              :initial-element 0d0))
      (setq b-matrix (grid:concatenate-grids b-matrix
					     (grid:concatenate-grids l-matrix
                                                                     c-matrix
                                                                     :axis 1)
                                             :axis 0))
      (setq b-matrix (grid:concatenate-grids b-matrix
					     (grid:make-foreign-array 'double-float
                                                                      :dimensions (list (+ (grid:dim0 si-matrix)
											   (grid:dim0 sv-matrix))
											(+ (grid:dim1 p-matrix)
											   (grid:dim1 g-matrix)))
                                                                      :initial-element 0d0)
                                             :axis 0))
      (when debug-mode
	(format output
                "~%B = ~a"
                b-matrix)
        (finish-output output))
      (setq k-vector (grid:make-foreign-array 'double-float
                                              :dimensions (+ (grid:dim0 p-matrix)
							     (grid:dim0 g-matrix))
                                              :initial-element 0d0))
      (when ki-vector
	(setq k-vector (grid:concatenate-grids k-vector
                                               ki-vector
                                               :axis 0)))
      (when kv-vector
	(setq k-vector (grid:concatenate-grids k-vector
                                               kv-vector
                                               :axis 0)))
      (when debug-mode
	(format output
                "~%K = ~a"
                k-vector)
        (finish-output output)))
    (values a-matrix b-matrix k-vector)))

;;;
;;; evaluate a model:
;;; model state:
;;; (t0 t1 n time y)
;;;

(defun evaluate-model (model &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (handler-case
      (let* ((*package* (find-package :circuit-solver))
	     (name (element-class-name model))
	     (class (model-class-class model))
	     (value (model-class-value model))
	     (parameters-list (model-class-parameters-list model))
	     (states-list (model-class-states-list model))
	     (function-name (model-class-function-name model))
	     (function-symbol (find-symbol (string-upcase function-name)))
	     (function nil)
	     (old-function-value nil))
        (when debug-mode
          (format output
                  "function symbol ~s~%"
                  function-symbol)
          (finish-output output))
	(if function-symbol
	    (setq function (symbol-function function-symbol)
	          old-function-value (apply function (list :parameters parameters-list
                                                           :state states-list)))
	    (if (load (make-pathname :name function-name :type "vcs"))
	        (setq model (evaluate-model model :debug-mode debug-mode :output output))
	        (error 'unknown-function-error
                       :function-name function-name)))
	(cond
	  ((simple-function-p model)
	   (setf (model-class-value model) old-function-value))
	  ((differential-function-p model)
	   (setf (model-class-value model) (+ value (* *h* old-function-value))))
	  (t
	   (error 'undefined-model-class-error
                  :model-name name :model-class-name class)))
	(when debug-mode
	  (format output
                  "Evaluated model: ~a~%"
                  (sexpify model))
          (finish-output output))
	model)
    (unknown-function-error (condition)
      (format *error-output*
              "~%Could not find ~a function."
              (function-name condition))
      (finish-output *error-output*)
      nil)
    (undefined-model-class-error (condition)
      (format *error-output*
              "~%Undefined class ~a for model ~a.~%"
              (model-name condition)
              (model-class-name condition))
      (finish-output *error-output*)
      nil)))

;;;
;;; Update all models in netlist
;;;
;;; determine state for variable for model: currents and/or voltages. Probes say which
;;; of them should be selected for model calculations. The state vector is formed like:
;;;
;;;         position	 |          meaning
;;;    ------------------+----------------------------
;;;            0         |   previous model value
;;;        1 ; m + 1     |    current variables
;;;    m + 2 ; m + n + 2 |    voltage variables
;;;

(defun update-model (element netlist state-vector &rest parameters &key
                                                                     (lock nil)
                                                                     (debug-mode nil)
                                                                     (output *standard-output*))
  (declare (ignorable parameters lock debug-mode output))
  (handler-case
      (let* ((elements-list (select (list (where :class-type 'source-class)
					  (where :class-type 'passive-class)
					  (where :class-type 'coupling-class))
                                    netlist
                                    :lock lock))
	     (nodes-list (select (where :class-type 'node-class)
                                 netlist
                                 :lock lock))
	     (branches-number (- (grid:dim0 state-vector)
                                 (length nodes-list)))
	     (state nil)
	     (i 0))
	(typecase element
	  ((or source-class passive-class coupling-class)
	   (when (element-class-model element)
	     (when debug-mode
	       (format output
                       "Evaluating model for ~a.~%"
                       (element-class-name element))
               (finish-output output))
	     (setf (element-class-model element) (update-model (element-class-model element)
                                                               netlist state-vector
                                                               :lock lock
                                                               :debug-mode debug-mode
                                                               :output output))
	     (setf (element-class-value element) (element-class-value (element-class-model element))))
	   (when (coupling-class-p element)
	     (dolist (inductance (coupling-class-elements-list element))
	       (setq inductance (update-model inductance
                                              netlist
                                              state-vector
                                              :lock lock
                                              :debug-mode debug-mode
                                              :output output)))))
	  (model-class
	   (push (model-class-value element) state)
	   (when (model-class-probes-list element)
	     (dolist (probe (model-class-probes-list element))
	       (when probe
		 (cond
		   ((voltage-probe-class-p probe)
		    (dolist (node-name (probe-class-nodes-list probe))
		      (let ((node (first (select (where :class-type 'node-class
                                                        :name node-name)
                                                 nodes-list))))
			(unless node
			  (error 'no-node-for-probe-error
                                 :node-name node-name
                                 :probe-name (element-class-name probe)))
			(cond
			  ((reference-class-node-p node)
			   (push 0d0 state))
			  (t
			   (setq i (find-node-position node-name
                                                       nodes-list
                                                       :lock lock))
			   (unless i
			     (error 'no-node-for-probe-error
                                    :node-name node-name
                                    :probe-name (element-class-name probe)))
			   (push (grid:gref state-vector (+ i branches-number)) state))))))
		   ((current-probe-class-p probe)
		    (dolist (element-name (probe-class-elements-list probe))
		      (multiple-value-bind (found-element i-found)
			  (find-element (where :name element-name) elements-list)
			(unless found-element
			  (error 'no-element-for-probe-error
                                 :element-name element-name
                                 :probe-name (element-name probe)))
			(push (grid:gref state-vector i-found) state))))
		   (t
		    (error 'undefined-probe-type-error
                           :probe-name (element-class-name probe)))))))
	   (setf (model-class-states-list element) state)
	   (when debug-mode
	     (format output
                     "Evaluating model ~a:~%Parameters = ~a~%State = ~a~%"
                     (element-class-name element)
                     (model-class-parameters-list element)
                     (model-class-states-list element))
             (finish-output output))
	   (setq element (evaluate-model element
                                         :debug-mode debug-mode
                                         :output output))))
	element)
    (no-node-for-probe-error (condition)
      (format *error-output*
              "No ~a node for probe ~a.~%"
              (node-name condition)
              (probe-name condition))
      (finish-output *error-output*)
      nil)
    (no-element-for-probe-error (condition)
      (format *error-output*
              "No ~a element for probe ~a.~%"
              (element-name condition)
              (probe-name-condition))
      (finish-output *error-output*)
      nil)
    (undefined-probe-type-error (condition)
      (format *error-output*
              "Undefined probe type (~a).~%"
              (probe-name condition))
      (finish-output *error-output*)
      nil)))

;;;
;;; print back simulation progress bar
;;;

(defun print-progress-bar (step little-mark big-mark &rest parameters &key (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (let ((percent nil))
    (unless *old-step*
      (setq *old-step* step))
    (setq percent (* 1d2 (/ (coerce (- step *old-step*) 'double-float)
                            (coerce *steps-number* 'double-float))))
    (cond
      ((and (not (zerop percent))
            (zerop (mod percent little-mark)))
       (format output
               "=")
       (finish-output output))
      ((or (zerop step)
           (>= percent big-mark))
       (setq *old-step* step)
       (format output
               "~d%"
               (floor (/ (* 1d2 step)
                         *steps-number*)))
       (finish-output output))
      ((= step *steps-number*)
       (format output
               "100% done!~%")
       (finish-output output)))))

;;;
;;; select solutions to write onto file
;;;

(defun select-probes (netlist y-vector output-file-stream &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (handler-case
      (let ((elements-list (select (list (where :class-type 'source-class)
					 (where :class-type 'passive-class)
					 (where :class-type 'coupling-class))
                                   (netlist-class-elements-list netlist)))
	    (nodes-list (select (where :class-type 'node-class)
                                (netlist-class-elements-list netlist)))
	    (probes-list (select (where :class-type 'probe-class)
                                 (netlist-class-elements-list netlist))))
	(unless output-file-stream
	  (error 'file-not-opened-error
                 :file-pathname (pathname output-file-stream)))
	(when debug-mode
	  (format output
                  "~%Writing to simulation file.")
          (finish-output output))
	(format output-file-stream
                "~f "
                *time*)
        (finish-output output-file-stream)
	(dolist (probe probes-list)
	  (cond
	    ((voltage-probe-class-p probe)
	     (dolist (node-name (probe-class-nodes-list probe))
	       (multiple-value-bind (found-node found-node-position)
		   (find-element (where :name node-name) nodes-list)
		 (unless found-node
		   (error 'probe-not-found-error
                          :probe-name (element-class-name probe)
                          :node-name node-name))
		 (incf found-node-position (- (grid:dim0 y-vector)
					      (length nodes-list)))
		 (when debug-mode
		   (format output
                           "~%Writing Y(~a) = ~a."
                           found-node-position
                           (grid:gref y-vector found-node-position))
                   (finish-output output))
		 (format output-file-stream
                         "~f "
                         (grid:gref y-vector found-node-position))
                 (finish-output output-file-stream))))
	    ((current-probe-class-p probe)
	     (dolist (element-name (probe-class-elements-list probe))
	       (multiple-value-bind (found-element found-element-position) (find-element (where :name element-name) elements-list)
		 (unless found-element
		   (error 'probe-not-found-error
                          :probe-name (element-name probe)
                          :element-name element-name))
		 (when debug-mode
		   (format output
                           "~%Writing Y(~a) = ~a."
                           found-element-position
                           (grid:gref y-vector found-element-position))
                   (finish-output output))
		 (format output-file-stream
                         "~f "
                         (grid:gref y-vector found-element-position))
                 (finish-output output-file-stream))))))
	(format output-file-stream
                "~%")
        (finish-output output-file-stream)
        t)
    (file-writing-error (condition)
      (format *error-output*
              "~%Could not write file ~a~%"
              (file-pathname condition))
      (finish-output *error-output*)
      nil)
    (file-not-opened-error (condition)
      (format *error-output*
              "~%File not opened.~%")
      (finish-output *error-output*)
      nil)
    (probe-not-found-error (condition)
      (format *error-output*
              "~%Probe ~a "
              (probe-name condition))
      (finish-output *error-output*)
      (when (node-name condition)
	(format *error-output*
                "node ~a not found"
                (node-name condition))
        (finish-output *error-output*))
      (when (element-name condition)
	(format *error-output*
                "element ~a not found"
                (element-name condition))
        (finish-output *error-output*))
      (format *error-output*
              ".~%")
      (finish-output *error-output*)
      nil)))

;;;
;;; setup output file
;;;

(defun open-simulation-file (netlist output-file-pathname)
  (handler-case
      (let ((probes-list (select (where :class-type 'probe-class)
                                 (netlist-class-elements-list netlist)))
	    (output-file-stream (open output-file-pathname
                                      :direction :output
                                      :if-exists :supersede)))
	(when output-file-stream
	  (format output-file-stream
                  "time ")
          (finish-output output-file-stream)
	  (dolist (probe probes-list)
	    (cond
	      ((voltage-probe-class-p probe)
	       (dolist (node-name (probe-class-nodes-list probe))
		 (format output-file-stream
                         "~a-~a "
                         (element-class-name probe)
                         node-name)
                 (finish-output output-file-stream)))
	      ((current-probe-class-p probe)
	       (dolist (element-name (probe-class-elements-list probe))
		 (format output-file-stream
                         "~a-~a "
                         (element-class-name probe)
                         element-name)
                 (finish-output output-file-stream)))))
	  (format output-file-stream
                  "~%~%")
          (finish-output output-file-stream))
	output-file-stream)
    (file-error (condition)
      (format *error-output*
              "~%Error opening file.~%")
      (finish-output *error-output*)
      nil)))

;;;
;;; setup initial conditions
;;;

(defun setup-initial-conditions (netlist y-vector &rest parameters &key (debug-mode nil) (output *standard-output*))
  (declare (ignorable parameters debug-mode output))
  (handler-case
      (let ((elements-list (exclude (list (where :class-type 'initial-condition-class)
					  (where :class-type 'probe-class))
                                    (netlist-class-elements-list netlist)))
	    (initial-conditions-list (select (where :class-type 'initial-condition-class)
                                             (netlist-class-elements-list netlist))))
	(dolist (initial-condition initial-conditions-list)
	  (when debug-mode
	    (format output
                    "Found initial condition ~a.~%"
                    (element-class-name initial-condition))
            (finish-output output))
	  (multiple-value-bind (element i)
	      (find-element (where :name (element-target-name initial-condition))
                            elements-list)
	    (unless element
	      (error 'unknown-element-for-initial-condition-error
                     :element-name (element-target-name initial-condition)
                     :initial-condition-name (element-class-name initial-condition)))
	    (typecase element
	      (source-class
	       (error 'initial-condition-error
                      :initial-condition-name (element-class-name initial-condition)
                      :source-name (element-class-name element)))
	      (passive-class
	       (when debug-mode
		 (format output
                         "Setting y(~a) = ~a.~%"
                         i
                         (passive-class-value initial-condition))
                 (finish-output output))
	       (setf (grid:gref y-vector i) (initial-condition-class-value initial-condition)))
	      (coupling-class
	       (error 'initial-condition-error
                      :initial-condition-name (element-class-name initial-condition)
                      :coupling-name (element-class-name element)))
	      (node-class
	       (when (reference-class-node-p (element-class element))
		 (error 'initial-condition-error
                        :initial-condition-name (element-class-name initial-condition)
                        :node-name (element-class-name element)))
	       (setf (grid:gref y-vector (1- i)) (initial-condition-class-value initial-condition)))
	      (model-class
	       (setf (model-class-value element) (initial-condition-class-value initial-condition)))
	      (t
	       (error "Initial condition ~a: ~a could not have an initial condition: only passive and models.~%"
                      (element-class-name initial-condition)
                      (element-class-name element))))))
	y-vector)
    (unknown-element-for-initial-condition-error (condition)
      (format *error-output*
              "Could not find element ~a to set initial condition ~a.~%"
              (element-name condition)
              (initial-condition-name condition))
      (finish-output *error-output*)
      nil)
    (initial-condition-source-error (condition)
      (format *error-output*
              "~%Could not set initial condition ~a"
              (initial-condition-name condition))
      (finish-output *error-output*)
      (when (source-name condition)
	(format *error-output*
                " for source ~a"
                (source-name condition))
        (finish-output *error-output*))
      (when (coupling-name condition)
	(format *error-output*
                " for coupling ~a (only its passive element can)"
                (coupling-name condition))
        (finish-output *error-output*))
      (when (node-name condition)
	(format *error-output*
                " for reference node ~a"
                (node-name condition))
        (finish-output *error-output*))
      (format *error-output*
              ".~%")
      (finish-output *error-output*)
      nil)))

;;
;; solve all problems
;;
;; D(n) = (B(n) + h A(n)) ^ -1
;; Y(n) = D(n) B(n) Y(n - 1) + h D(n) K(n)
;;

(defun solve-problem (netlist-file-pathname time-start time-stop time-steps &rest parameters &key
                                                                                               (verbose nil)
                                                                                               (debug-mode nil)
                                                                                               (progress-bar nil)
                                                                                               (parallel nil)
                                                                                               (epsilon 1d-6 epsilon-p)
                                                                                               (maximum-refinement-iteration-count 1000 maximum-refinement-iteration-count-p)
                                                                                               (output *standard-output*))
  (declare (ignorable parameters verbose debug-mode progress-bar parallel epsilon maximum-refinement-iteration-count output))
  (when epsilon-p
    (check-type epsilon double-float)
    (assert (> epsilon 0d0)))
  (when maximum-refinement-iteration-count-p
    (check-type maximum-refinement-iteration-count (integer 1)))
  (handler-case
      (let ((problem (make-problem :netlist-file-pathname netlist-file-pathname))
            (thread-functions nil)
            (tasks-count 0)
            (channel nil)
            (promise nil)
            (lock nil))
	(when verbose
	  (format output
                  "~%Circuit Solver - Version ~a.~a.~a.~a~%Written by Dott. Ing. Angelo Rossi & Dott. Ing. Marco Maccioni.~%Released under GPL3 License (C) ~@r."
                  major
                  minor
                  build
                  revision
                  year)
	  (format output
                  "~%Running on ~a machine type ~a.~%"
                  (software-type)
                  (machine-type))
          (finish-output output))
	(unless (> time-stop time-start)
	  (error 'simulation-time-interval-error
                 :t1 time-start
                 :t0 time-stop))
	(setq *t0* time-start
	      *t1* time-stop)
	(if (and (< time-steps *minimum-steps-number*)
                 (not debug-mode))
	    (setq *steps-number* *minimum-steps-number*)
            (setq *steps-number* time-steps))
	(setq *h* (/ (- *t1* *t0*)
                     (float *steps-number*)))
	(when verbose
	  (format output
                  "~%Setting steps number to ~a."
                  *steps-number*)
          (finish-output output))
        (when debug-mode
          (format output
                  "Starting time ~a, ending time ~a, time steps ~a.~%"
                  *t0*
                  *t1*
                  *steps-number*)
          (finish-output output))
	(let ((netlist (read-netlist netlist-file-pathname))
	      (error-found 0))
          (when netlist
	    (setq netlist (include-subcircuits netlist
                                               :verbose verbose
                                               :debug-mode debug-mode
                                               :output output))
	    (setq error-found (check-netlist netlist))
	    (when (zerop error-found)
	      (when verbose
	        (format output
                        "~%Input file: ~a"
                        netlist-file-pathname)
	        (format output
                        "~%Debug Mode: ~a"
                        debug-mode)
	        (format output
                        "~%Loaded netlist: ~a"
                        (element-class-name netlist))
                (finish-output output))
	      (when debug-mode
	        (format output
                        "~%~a"
                        (sexpify netlist))
                (finish-output output))
	      (when verbose
	        (format output
                        "~%Start at ~a s upto ~a s with Delta t = ~a s (~a steps)."
                        *t0*
                        *t1*
                        *h*
                        *steps-number*)
                (finish-output output))
	      (when debug-mode
	        (format output
                        "~%Found ~a nodes and ~a elements."
		        (length (select (where :class-type 'node-class)
                                        (netlist-class-elements-list netlist)))
		        (length (exclude (list (where :class-type 'node-class)
					       (where :class-type 'subcircuit-class))
                                         (netlist-class-elements-list netlist))))
                (finish-output output))
	      (let ((p-matrix (create-p-matrix (netlist-class-elements-list netlist)
                                               :debug-mode debug-mode
                                               :output output))
		    (r-matrix (create-r-l-matrix (netlist-class-elements-list netlist)
                                                 :debug-mode debug-mode
                                                 :output output))
		    (g-matrix (create-g-c-matrix (netlist-class-elements-list netlist)
                                                 :debug-mode debug-mode
                                                 :output output))
		    (si-matrix (create-si-matrix (netlist-class-elements-list netlist)
                                                 :debug-mode debug-mode
                                                 :output output))
		    (sv-matrix (create-sv-matrix (netlist-class-elements-list netlist)
                                                 :debug-mode debug-mode
                                                 :output output))
		    (l-matrix (create-r-l-matrix (netlist-class-elements-list netlist)
                                                 :debug-mode debug-mode
                                                 :output output))
		    (c-matrix (create-g-c-matrix (netlist-class-elements-list netlist)
                                                 :debug-mode debug-mode
                                                 :output output))
		    (ki-vector (create-ki-vector (netlist-class-elements-list netlist)
                                                 :debug-mode debug-mode
                                                 :output output))
		    (kv-vector (create-kv-vector (netlist-class-elements-list netlist)
                                                 :debug-mode debug-mode
                                                 :output output))
		    (y-old-vector (create-k-y-vector (netlist-class-elements-list netlist)
                                                     :debug-mode debug-mode
                                                     :output output))
		    (y-new-vector (create-k-y-vector (netlist-class-elements-list netlist)
                                                     :debug-mode debug-mode
                                                     :output output)))
	        (setq y-old-vector (setup-initial-conditions netlist y-old-vector
                                                             :debug-mode debug-mode
                                                             :output output))
	        (let* ((output-file-pathname (make-pathname :directory (pathname-directory netlist-file-pathname)
                                                            :name (pathname-name netlist-file-pathname)
                                                            :type "sim"))
		       (output-file-stream (open-simulation-file netlist
                                                                 output-file-pathname)))
		  (when verbose
		    (format output
                            "~&Output file: ~a~2%"
                            output-file-pathname)
                    (finish-output output)
		    (format output
                            "~2&Solving: ")
                    (finish-output output))
                  (when parallel
                    (setq lock (bt:make-recursive-lock (symbol-name (gensym "lock-")))
                          thread-functions (list (generate-thread-function (promise debug-mode)
                                                   (dolist (element (netlist-class-elements-list netlist))
		                                     (setq element (update-model element
                                                                                 (netlist-class-elements-list netlist)
                                                                                 y-old-vector
                                                                                 :lock lock
                                                                                 :debug-mode debug-mode
                                                                                 :output output))))
                                                 (generate-thread-function (promise debug-mode)
                                                   (setq p-matrix (update-p-matrix p-matrix
                                                                                   (netlist-class-elements-list netlist)
                                                                                   :lock lock
                                                                                   :debug-mode debug-mode
                                                                                   :output output)))
                                                 (generate-thread-function (promise debug-mode)
			                           (setq r-matrix (update-r-matrix r-matrix
                                                                                   (netlist-class-elements-list netlist)
                                                                                   :lock lock
                                                                                   :debug-mode debug-mode
                                                                                   :output output)))
                                                 (generate-thread-function (promise debug-mode)
			                           (setq g-matrix (update-g-matrix g-matrix
                                                                                   (netlist-class-elements-list netlist)
                                                                                   :lock lock
                                                                                   :debug-mode debug-mode
                                                                                   :output output)))
                                                 (generate-thread-function (promise debug-mode)
                                                   (setq si-matrix (update-si-matrix si-matrix
                                                                                     (netlist-class-elements-list netlist)
                                                                                     :debug-mode debug-mode
                                                                                     :output output)))
                                                 (generate-thread-function (promise debug-mode)
			                           (setq sv-matrix (update-sv-matrix sv-matrix
                                                                                     (netlist-class-elements-list netlist)
                                                                                     :debug-mode debug-mode
                                                                                     :output output)))
                                                 (generate-thread-function (promise debug-mode)
                                                   (setq l-matrix (update-l-matrix l-matrix
                                                                                   (netlist-class-elements-list netlist)
                                                                                   :debug-mode debug-mode
                                                                                   :output output)))
                                                 (generate-thread-function (promise debug-mode)
			                           (setq c-matrix (update-c-matrix c-matrix
                                                                                   (netlist-class-elements-list netlist)
                                                                                   :debug-mode debug-mode
                                                                                   :output output)))
                                                 (generate-thread-function (promise debug-mode)
			                           (setq ki-vector (update-ki-vector ki-vector
                                                                                     (netlist-class-elements-list netlist)
                                                                                     :debug-mode debug-mode
                                                                                     :output output)))
                                                 (generate-thread-function (promise debug-mode)
                                                   (setq kv-vector (update-kv-vector kv-vector
                                                                                     (netlist-class-elements-list netlist)
                                                                                     :debug-mode debug-mode
                                                                                     :output output))))
                          tasks-count (length thread-functions))
                    (when debug-mode
                      (format output
                              "Number of tasks: ~a~%"
                              tasks-count)
                      (finish-output output))
                    (setf lparallel:*kernel* (lparallel:make-kernel tasks-count
                                                                    :name (symbol-name (gensym "kernel-"))))
                    (setq channel (lparallel:make-channel)))
	          (loop
                    for i from 0 to *steps-number*
                    do
		       (setq *time* (+ *t0*
                                       (* *h*
                                          (float i))))
		       (when (and debug-mode
                                  (not progress-bar))
		         (format output
                                 "~2%----~%Iteration #~a~%time = ~a~%----~2%"
                                 i
                                 *time*)
                         (finish-output output))

		       ;; Update matrices

                       (if parallel
                           (progn
                             (setq promise (lparallel:promise))
                             (loop
                               for thread-function in thread-functions
                               do
                                  (lparallel:submit-task channel thread-function))
                             (lparallel:fulfill promise t)
                             (loop
                               finally (when debug-mode
                                         (format output
                                                 "All tasks completed!~%")
                                         (finish-output output))
                               for i from 0 below tasks-count
                               do
                                  (lparallel:receive-result channel)))
                           (progn
		             (dolist (element (netlist-class-elements-list netlist))
		               (setq element (update-model element
                                                           (netlist-class-elements-list netlist)
                                                           y-old-vector
                                                           :debug-mode debug-mode
                                                           :output output)))
		             (setq p-matrix (update-p-matrix p-matrix
                                                             (netlist-class-elements-list netlist)
                                                             :debug-mode debug-mode
                                                             :output output)
			           r-matrix (update-r-matrix r-matrix
                                                             (netlist-class-elements-list netlist)
                                                             :debug-mode debug-mode
                                                             :output output)
			           g-matrix (update-g-matrix g-matrix
                                                             (netlist-class-elements-list netlist)
                                                             :debug-mode debug-mode
                                                             :output output)
			           si-matrix (update-si-matrix si-matrix
                                                               (netlist-class-elements-list netlist)
                                                               :debug-mode debug-mode
                                                               :output output)
			           sv-matrix (update-sv-matrix sv-matrix
                                                               (netlist-class-elements-list netlist)
                                                               :debug-mode debug-mode
                                                               :output output)
			           l-matrix (update-l-matrix l-matrix
                                                             (netlist-class-elements-list netlist)
                                                             :debug-mode debug-mode
                                                             :output output)
			           c-matrix (update-c-matrix c-matrix
                                                             (netlist-class-elements-list netlist)
                                                             :debug-mode debug-mode
                                                             :output output)
			           ki-vector (update-ki-vector ki-vector
                                                               (netlist-class-elements-list netlist)
                                                               :debug-mode debug-mode
                                                               :output output)
			           kv-vector (update-kv-vector kv-vector
                                                               (netlist-class-elements-list netlist)
                                                               :debug-mode debug-mode
                                                               :output output))))
		       (multiple-value-bind (a-matrix b-matrix k-vector)
		           (assemble-system p-matrix
                                            r-matrix
                                            g-matrix
                                            si-matrix
                                            sv-matrix
                                            l-matrix
                                            c-matrix
                                            ki-vector
                                            kv-vector
                                            :debug-mode debug-mode
                                            :output output)
		         (let ((alpha-matrix (gsl:elt+ (gsl:elt* *h*
                                                                 (grid:copy-to a-matrix 'grid:foreign-array))
                                                       (grid:copy-to b-matrix 'grid:foreign-array)))
			       (beta-matrix (gsl:elt+ (gsl:elt* *h*
                                                                (grid:copy-to k-vector 'grid:foreign-array))
                                                      (gsl:matrix-product (grid:copy-to b-matrix 'grid:foreign-array)
                                                                          (grid:copy-to y-old-vector 'grid:foreign-array)))))
		           (multiple-value-bind (decomposition-matrix permutation-matrix sign)
                               (gsl:lu-decomposition (grid:copy-to alpha-matrix 'grid:foreign-array))
                             (when debug-mode
			       (format output
                                       "~&Permutation sign: ~a~%"
                                       sign)
                               (finish-output output))
                             (let ((residuals (grid:make-foreign-array 'double-float :dimensions (grid:dimensions y-new-vector) :initial-element 0d0))
                                   (initial-solution (gsl:lu-solve (grid:copy-to decomposition-matrix 'grid:foreign-array)
                                                                   (grid:copy-to beta-matrix 'grid:foreign-array)
                                                                   permutation-matrix
                                                                   t)))
                               (loop
                                 for j from 0 below maximum-refinement-iteration-count
                                 do
			            (setq y-new-vector (gsll:lu-refine (grid:copy-to alpha-matrix 'grid:foreign-array)
                                                                       (grid:copy-to decomposition-matrix 'grid:foreign-array)
                                                                       permutation-matrix
                                                                       (grid:copy-to beta-matrix 'grid:foreign-array)
                                                                       initial-solution
                                                                       residuals))
                                    (when (< (grid:norm residuals) epsilon)
                                      (return)))
			       (when debug-mode
			         (format output
                                         "~&Y(n+1) =~%~a~%"
                                         y-new-vector)
			         (format output
                                         "~&Y(n) =~%~a~%"
                                         y-old-vector)
                                 (finish-output output))
                               (unless (select-probes netlist
                                                      y-new-vector
                                                      output-file-stream
                                                      :debug-mode debug-mode
                                                      :output output)
                                 (return-from solve-problem))
			       (setq y-old-vector y-new-vector)))
		           (when (and (not debug-mode)
				      progress-bar)
			     (print-progress-bar i 2 20 :output output)))))
		  (when output-file-stream
		    (close output-file-stream))
		  output-file-pathname))))))
    (gsll:input-domain (condition)
      (format *error-output*
              "Simulation error at time (~a): ~a.~%"
              *time*
              condition)
      (finish-output *error-output*)
      nil)
    (simulation-time-interval-error (condition)
      (format *error-output*
              "Simulation final time (~a) less than or equal to start time (~a).~%"
              (:t0 condition)
              (:t1 condition))
      (finish-output *error-output*)
      nil)))

(defun kst (netlist-file-pathname t0 t1 steps &rest parameters &key (verbose nil) (debug-mode nil) (progress-bar nil) (output *standard-output*))
  (declare (ignorable parameters verbose debug-mode progress-bar output))
  (asdf:run-shell-command "kst2 ~S"
                          (solve-problem netlist-file-pathname
                                         t0
                                         t1
                                         steps
                                         :verbose verbose
                                         :debug-mode debug-mode
                                         :output output)))
