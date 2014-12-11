;;;; circuit_solver.lisp

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
(defparameter *t0* 0d0)
(defparameter *t1* 0d0)
(defparameter *h* 0d0)

;;;
;;; 05/12/2014 - 23:15:00
;;;
;;; exception handling
;;;

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

;;; 
;;; data structures:
;;; classes for circuital elements and problem description
;;;
;;; - element is the base class bearing an numerical id and a string name
;;; - netlist is the set of circuital elements and netlists
;;; - node is the object for describing circuital elements junction
;;;

;;;
;;; basic netlist element class definition:
;;;
;;; - a numerical ID;
;;; - a characters string for NAME. 
;;;

(defclass element-class ()
  ((id 
    :initarg :id 
    :initform -1
    :accessor element-class-id)
   (name 
    :initarg :name 
    :initform ""
    :accessor element-class-name)))

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
    :accessor netlist-class-file-pathname
    :accessor element-class-file-pathname)
   (author
    :initarg :author
    :initform ""
    :accessor netlist-class-author
    :accessor element-class-author)
   (date
    :initarg :date
    :initform ""
    :accessor netlist-class-date
    :accessor element-class-date)
   (elements-list
    :initarg :elements-list
    :initform nil
    :accessor netlist-class-elements-list
    :accessor element-class-elements-list)))

;;;
;;; node element class
;;;

(defclass node-class (element-class)
  ((class
    :documentation "node CLASS: reference, voltage-current, active-reactive-power or undefined."
    :initarg :class
    :initform ""
    :accessor node-class-class
    :accessor element-class-class)
   (state
    :documentation "node STATE: undiscovered, discovered and processed."
    :initarg :state
    :initform nil
    :accessor node-class-state
    :accessor element-class-state)
   (number
    :documentation "node NUMBER: 0 = reference, n = other nodes."
    :initarg :number
    :initform -1
    :accessor node-class-number
    :accessor element-class-number)))

;;;  
;;; passive circuital element
;;;

(defclass passive-class (element-class)
  ((class
    :documentation "passive CLASS: resistance, conductance, inductance, capacitance or undefined."
    :initarg :class
    :initform "undefined"
    :accessor passive-class-class
    :accessor element-class-class)
   (nodes-list
    :documentation "NODES-LIST are the pin connections for the element: (\"N1\" \"N2\")"
    :initarg :nodes-list
    :initform ()
    :accessor passive-class-nodes-list
    :accessor element-class-nodes-list)
   (model
    :documentation "passive MODEL."
    :initarg :model
    :initform nil
    :accessor passive-class-model
    :accessor element-class-model)
   (value
    :documentation "passive constant VALUE e.g. R1 = 10 Ohm: VALUE = 10 Ohm."
    :initarg :value
    :initform 0d0
    :accessor passive-class-value
    :accessor element-class-value)))

;;;
;;; inductances coupling element class
;;;

(defclass coupling-class (element-class)
  ((class
    :documentation "coupling CLASS: motional or transformational."
    :initarg :class
    :initform "undefined"
    :accessor coupling-class-class
    :accessor element-class-class)
   (elements-list
    :documentation "list of the inductances taking part in the coupling."
    :initarg :elements-list
    :initform ()
    :accessor coupling-class-elements-list
    :accessor element-class-elements-list)
   (model
    :documentation "model definition for k parameter."
    :initarg :model
    :initform nil
    :accessor coupling-class-model
    :accessor element-class-model)
   (value
    :documentation "k parameter constant value."
    :initarg :value
    :initform 0d0
    :accessor coupling-class-value
    :accessor element-class-value)))

;;;    
;;; current/voltage source class definition
;;;

(defclass source-class (element-class)
  ((class
    :documentation "source CLASS: current, voltage or undefined."
    :initarg :class
    :initform "undefined"
    :accessor source-class-class
    :accessor element-class-class)
   (nodes-list
    :documentation "source connection nodes to the circuit."
    :initarg :nodes-list
    :initform ()
    :accessor source-class-nodes-list
    :accessor element-class-nodes-list)
   (model
    :documentation "source model."
    :initarg :model
    :initform nil
    :accessor source-class-model
    :accessor element-class-model)
   (value
    :documentation "source constant value e.g. 10 A or 1000 V."
    :initarg :value
    :initform nil
    :accessor source-class-value
    :accessor element-class-value)))

;;;
;;; subcircuit class definition
;;;

(defclass subcircuit-class (element-class)
  ((file-pathname
    :documentation "FILE-NAME for subcircuit definition."
    :initarg :file-pathname
    :initform ""
    :accessor subcircuit-class-file-pathname
    :accessor element-class-file-pathname)
   (nodes-list
    :documentation "list of connection nodes."
    :initarg :nodes-list
    :initform nil
    :accessor subcircuit-class-nodes-list
    :accessor element-class-nodes-list)))

;;;
;;; model class definition
;;;

(defclass model-class (element-class)
  ((class
    :documentation "model CLASS: function, differential, lisp-function, lisp-differential or undefined."
    :initarg :class
    :initform "undefined"
    :accessor model-class-class
    :accessor element-class-class)
   (parameters-list
    :documentation "PARAMETERS-LIST of model input parameters."
    :initarg :parameters-list
    :initform nil
    :accessor model-class-parameters-list
    :accessor element-class-parameters-list)
   (function-name
    :documentation "hardcoded lisp function name."
    :initarg :function-name
    :initform ""
    :accessor model-class-function-name    
    :accessor element-class-function-name)
   (external-function-name
    :documentation "external lisp function for model."
    :initarg :function
    :initform nil
    :accessor model-class-external-function-name
    :accessor element-class-external-function-name)
   (probes-list
    :documentation "list of probes to sample circuital quantities."
    :initarg :probes-list
    :initform nil
    :accessor model-class-probes-list
    :accessor element-class-probes-list)
   (states-list
    :documentation "model state vector."
    :initarg :states-list
    :initform nil
    :accessor model-class-states-list
    :accessor element-class-states-list)
   (value
    :documentation "model value."
    :initarg :value
    :initform 0d0
    :accessor model-class-value
    :accessor element-class-value)))

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

;;;
;;; initial condition specifier
;;;

(defclass initial-condition-class (element-class)
  ((target-name
    :documentation "name for element initial conditions: branch (current), node (voltage) or model (quantity)."
    :initarg :target-name
    :initform nil
    :accessor initial-condition-class-target-name
    :accessor element-class-target-name)
   (value
    :documentation "value for initial condition."
    :initarg :value
    :initform nil
    :accessor initial-condition-class-value
    :accessor element-class-value)))

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

;;;
;;; spline class
;;;

(defclass spline-data-class (element-class)
  ((data-vectors
    :documentation "data itself."
    :initarg :data-vectors
    :initform nil
    :accessor spline-class-data-vectors)))

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
						   (objectify subsexp)) (rest sexp)))))
		      
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
		   (objectify subsexp)) sexp)))
    (unknown-object-error (condition)
      (format *error-output* "~%Unknown object found: ~s~%" (object condition))
      nil)))
	   
;;
;; predicate methods.
;;

;;
;; node class.
;;

(defmethod undefined-class-p ((object node-class))
  (or (string-equal (node-class-class object) "undefined")
      (string-equal (node-class-class object) "")))

(defmethod reference-class-node-p ((object node-class))
  (or (string-equal (node-class-class object) "reference")
      (string-equal (node-class-class object) "gnd")
      (string-equal (node-class-class object) "0")))

;;
;; passive class.
;;

(defmethod undefined-class-p ((object passive-class))
  (or (string-equal (passive-class-class object) "undefined")
      (string-equal (passive-class-class object) "")))

(defmethod has-model-p ((object passive-class))
  (typep object 'model-class))

(defmethod has-value-p ((object passive-class))
  (null (passive-class-value object)))

;;
;; coupling class.
;;

(defmethod has-model-p ((object coupling-class))
  (typep object 'model-class))

(defmethod has-value-p ((object coupling-class))
  (null (coupling-class-value object)))

;;
;; source class.
;;

(defmethod undefined-class-p ((object source-class))
  (or (string-equal (source-class-class object) "undefined")
      (string-equal (source-class-class object) "")))

(defmethod voltage-source-class-p ((object source-class))
  (string-equal (source-class-class object) "voltage-source"))

(defmethod current-source-class-p ((object source-class))
  (string-equal (source-class-class object) "current-source"))

(defmethod has-model-p ((object source-class))
  (typep (source-class-model object) 'model-class))

(defmethod has-value-p ((object source-class))
  (null (source-class-value object)))

;;
;; model class.
;;

(defmethod undefined-class-p ((object model-class))
  (or (string-equal (model-class-class object) "undefined")
      (string-equal (model-class-class object) "")))

(defmethod simple-function-p ((object model-class))
  (string-equal (model-class-class object) "function"))

(defmethod differential-function-p ((object model-class))
  (string-equal (model-class-class object) "differential"))

(defmethod has-value-p ((object model-class))
  (null (model-class-value object)))

;;
;; probe class.
;;

(defmethod undefined-class-p ((object probe-class))
  (or (string-equal (probe-class-class object) "undefined")
      (string-equal (probe-class-class object) "")))

(defmethod voltage-probe-class-p ((object probe-class))
  (string-equal (probe-class-class object) "voltage-probe"))

(defmethod current-probe-class-p ((object probe-class))
  (string-equal (probe-class-class object) "current-probe"))

;;
;; serialization: create a sexp from an object
;;

;;
;; create a sexp for element: :NAME name :ID id
;;

(defmethod sexpify ((object element-class))
  (let ((return-value (list (type-of object))))
    (when (element-class-name object)
      (setf return-value (append return-value (list :name (element-class-name object)))))
    (unless (eql (element-class-id object) -1)
      (setf return-value (append return-value (list :id (element-class-id object)))))
    return-value))

;;
;; create a sexp for netlist element: :NAME name [ :ID id ] :ELEMENTS-LIST elements-list
;;

(defmethod sexpify ((object netlist-class))
  (let ((return-value (call-next-method object)))
    (unless (string-equal (netlist-class-author object) "")
      (setf return-value (append return-value (list :author (netlist-class-author object)))))
    (unless (string-equal (netlist-class-date object) "")
      (setf return-value (append return-value (list :date (netlist-class-date object)))))
    (setf return-value (append return-value (list :elements-list (mapcar #'sexpify (netlist-class-elements-list object)))))
    return-value))

;;
;; create a sexp for node element: :NAME name [ :ID id ] :CLASS element-class [ :STATE state ] [ :NUMBER number ]
;;

(defmethod sexpify ((object node-class))
  (let ((return-value (call-next-method object)))
    (unless (undefined-class-p object)
      (setf return-value (append return-value (list :class (node-class-class object)))))
    (unless (eql (node-class-state object) nil)
      (setf return-value (append return-value (list :state (node-class-state object)))))
    (unless (eql (node-class-number object) -1)
      (setf return-value (append return-value (list :number (node-class-number object)))))
    return-value))

;;
;; create a sexp for passive element: :NAME name [ :ID id ] :CLASS element-class :NODES-LIST nodes-list { :MODEL model | :VALUE value }
;;

(defmethod sexpify ((object passive-class))
  (let ((return-value (call-next-method object)))
    (unless (undefined-class-p object)
      (setf return-value (append return-value (list :class (passive-class-class object)))))
    (when (passive-class-nodes-list object)
      (setf return-value (append return-value (list :nodes-list (passive-class-nodes-list object)))))
    (when (has-model-p object)
      (setf return-value (append return-value (list :model (sexpify (passive-class-model object))))))
    (when (has-value-p object)
      (setf return-value (append return-value (list :value (passive-class-value object)))))
    return-value))

;; 
;; create a sexp for coupling element: :NAME name [ :ID id ] :ELEMENTS-LIST elements-list { :MODEL model | :VALUE value }
;;

(defmethod sexpify ((object coupling-class))
  (let ((return-value (call-next-method object)))
    (when (coupling-class-elements-list object)
      (setf return-value (append return-value (list :elements-list (mapcar #'sexpify (coupling-class-elements-list object))))))
    (when (has-model-p object)
      (setf return-value (append return-value (list :model (sexpify (coupling-class-model object))))))
    (when (has-value-p object)
      (setf return-value (append return-value (list :value (coupling-class-value object)))))
    return-value))

;;
;; create a sexp for source element: :NAME name [ :ID id ] :CLASS element-class :NODES-LIST nodes-list { :MODEL model | :VALUE value }
;;

(defmethod sexpify ((object source-class))
  (let ((return-value (call-next-method object)))
    (unless (undefined-class-p object)
      (setf return-value (append return-value (list :class (source-class-class object)))))
    (when (source-class-nodes-list object)
      (setf return-value (append return-value (list :nodes-list (source-class-nodes-list object)))))
    (when (has-model-p object)
      (setf return-value (append return-value (list :model (sexpify (source-class-model object))))))
    (when (has-value-p object)
      (setf return-value (append return-value (list :value (source-class-value object)))))
    return-value))

;;
;; create a sexp for subcircuit element: :NAME name [ :ID id ] :FILE-NAME file-name :NODES-LIST nodes-list
;;

(defmethod sexpify ((object subcircuit-class))
  "Create a sexp for subcircuit element: :NAME name [ :ID id ] :FILE-NAME file-name :NODES-LIST nodes-list"
  (let ((return-value (call-next-method object)))
    (when (pathnamep (subcircuit-class-file-pathname object))
      (setf return-value (append return-value  (list :file-pathname (subcircuit-class-file-pathname object)))))
    (when (subcircuit-class-nodes-list object)
      (setf return-value (append return-value (list :nodes-list (subcircuit-class-nodes-list object)))))
    return-value))

(defmethod sexpify ((object model-class))
  (let ((return-value (call-next-method object)))
    (unless (undefined-class-p object)
      (setf return-value (append return-value (list :class (model-class-class object)))))
    (when (model-class-function-name object)
      (setf return-value (append return-value (list :function-name (model-class-function-name object)))))
    (when (model-class-external-function-name object)
      (setf return-value (append return-value (list :external-function-name (model-class-external-function-name object)))))
    (when (model-class-parameters-list object)
      (setf return-value (append return-value (list :parameters-list (model-class-parameters-list object)))))
    (when (model-class-states-list object)
      (setf return-value (append return-value (list :states-list (model-class-states-list object)))))
    (when (model-class-probes-list object)
      (setf return-value (append return-value (mapcar #'sexpify (model-class-probes-list object)))))
    (when (has-value-p object)
      (setf return-value (append return-value (list :value (model-class-value object)))))
    return-value))

(defmethod sexpify ((object probe-class))
  (let ((return-value (call-next-method object)))
    (unless (undefined-class-p object)
      (setf return-value (append return-value (list :class (probe-class-class object)))))
    (when (probe-class-elements-list object)
      (setf return-value (append return-value (list :elements-list (probe-class-elements-list object)))))
    (when (probe-class-nodes-list object)
      (setf return-value (append return-value (list :nodes-list (probe-class-nodes-list object)))))
    return-value))

(defmethod sexpify ((object initial-condition-class))
  (let ((return-value (call-next-method object)))
    (when (initial-condition-class-target-name object)
      (setf return-value (append return-value (list :target-name (initial-condition-class-target-name object)))))
    (when (initial-condition-class-value object)
      (setf return-value (append return-value (list :value (initial-condition-class-value object)))))
    return-value))

;;
;; name manipulators
;;

(defmethod rename-element ((object element-class) radix)
  (setf (element-class-name object) (concatenate 'string radix ":" (element-class-name object)))
  object)

;;
;; Predicates.
;;

(defun resistance-class-p (object)
  (when (typep object 'passive-class)
    (string-equal (passive-class-class object) "resistance")))

(defun inductance-class-p (object)
  (when (typep object 'passive-class)
    (string-equal (passive-class-class object) "inductance")))

(defun capacitance-class-p (object)
  (when (typep object 'passive-class)
    (string-equal (passive-class-class object) "capacitance")))

(defun conductance-class-p (object)
  (when (typep object 'passive-class)
    (string-equal (passive-class-class object) "conductance")))

(defun coupling-class-p (object)
  (typep object 'coupling-class))

;;
;; selection and exclusion criterion functions
;;

(defun where (&optional &key class-type id name number class)
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

(defun select (selectors netlist)
  (let ((selection nil))
    (dolist (element netlist)
      (let ((where-return-value nil))
	(if (listp selectors)
	    (dolist (selector selectors)
	      (setf where-return-value (or (funcall selector element) where-return-value)))
	    (setf where-return-value (funcall selectors element)))
	(when (eql where-return-value t)
	  (setf selection (append selection (list element))))))
    selection))

;;;
;;; create a list where all elements do not satisfy the WHERE clause
;;;

(defun exclude (selectors netlist)
  (let ((selection nil))
    (dolist (element netlist)
      (let ((where-return-value nil))
	(if (listp selectors)
	    (dolist (selector selectors)
	      (setf where-return-value (or (funcall selector element) where-return-value)))
	    (setf where-return-value (funcall selectors element)))
	(unless (eql where-return-value t)
	  (setf selection (append selection (list element))))))
    selection))

(defmethod check-element-with-selectors ((object element-class) selectors)
  (let ((return-value nil))
    (if (listp selectors)
	(dolist (selector selectors)
	  (setf return-value (or return-value
				 (funcall selector object))))
	(setf return-value (funcall selectors object)))
    return-value))

(defmethod check-element-with-selectors ((object source-class) selectors)
  (let ((return-value (call-next-method object selectors)))
    (if (listp selectors)
	(dolist (selector selectors)
	  (setf return-value (or return-value
				  (funcall selector object))))
	(setf return-value (funcall selectors object)))
    return-value))

(defmethod check-element-with-selectors ((object node-class) selectors)
  (let ((return-value (call-next-method object selectors)))
    (if (listp selectors)
	(dolist (selector selectors)
	  (setf return-value (or return-value
				  (funcall selector object))))
	(setf return-value (funcall selectors object)))
    return-value))

(defmethod check-element-with-selectors ((object probe-class) selectors)
  (let ((return-value (call-next-method object selectors)))
    (if (listp selectors)
	(dolist (selector selectors)
	  (setf return-value (or return-value
				  (funcall selector object))))
	(setf return-value (funcall selectors object)))
    return-value))

(defmethod check-element-with-selectors ((object model-class) selectors)
  (let ((return-value (call-next-method object selectors)))
    (if (listp selectors)
	(dolist (selector selectors)
	  (setf return-value (or return-value
				  (funcall selector object))))
	(setf return-value (funcall selectors object)))
    return-value))

(defmethod check-element-with-selectors ((object coupling-class) selectors)
  (let ((return-value (call-next-method object selectors)))
    (if (listp selectors)
	(dolist (selector selectors)
	  (setf return-value (or return-value
				  (funcall selector object))))
	(setf return-value (funcall selectors object)))
    return-value))
  
;;;
;;; find an element in a list that satisfy the WHERE clause or a list of WHERE clauses.
;;;

;; (defun find-element (selectors netlist)
;;   (handler-case
;;       (let ((selector-return-value nil)
;; 	    (found-element nil)
;; 	    (found-element-position 0))
;; 	(dolist (element netlist)
;; 	  (typecase element
;; 	    ((or source-class passive-class node-class probe-class model-class)
;; 	     (if (listp selectors)
;; 		 (dolist (selector selectors)
;; 		   (setf selector-return-value (or selector-return-value
;; 						   (funcall selector element))))
;; 		 (setf selector-return-value (funcall selectors element)))
;; 	     (when selector-return-value
;; 	       (setf found-element element)
;; 	       (return))
;; 	     (incf found-element-position))
;; 	    (coupling-class
;; 	     (multiple-value-bind (coupling-inductance-element coupling-inductance-position) 
;; 		 (find-element selectors (coupling-class-elements-list element))
;; 	       (when coupling-inductance-element
;; 		 (setf found-element coupling-inductance-element)
;; 		 (incf found-element-position coupling-inductance-position)
;; 		 (return)))
;; 	     (incf found-element-position (length (coupling-class-elements-list element))))
;; 	    (t
;; 	     (error 'unknown-class-object-error :object element))))
;; 	(values found-element found-element-position))
;;     (unknown-class-object-error (condition)
;;       (format *error-output* "~%Unknow object found: ~s~%" (object condition))
;;       nil)))

(defun find-element (selectors netlist)
  (let ((element-position 0))
    (dolist (element netlist)	  
      (when (check-element-with-selectors element selectors)
	(return (values element element-position)))
      (incf element-position))))

;;; 
;;; count function
;;;

(defun count-elements (selectors netlist)
  (length (select selectors netlist)))

;;;
;;; update element members
;;;

(defmethod update ((object element-class) &optional &key id name)
  (when id
    (setf (element-class-id object) id))
  (when name
    (setf (element-class-name object) name))
  object)

(defmethod update ((object netlist-class) &optional &key id name elements-list)
  (let ((return-value (call-next-method object :id id :name name)))
    (when elements-list
      (setf (netlist-class-elements-list return-value) elements-list))
    return-value))

(defmethod update ((object node-class) &optional &key id name class number)
  (let ((return-value (call-next-method object :id id :name name)))
    (when class
      (setf (node-class-class object) class))
    (when number
      (setf (node-class-number object) number))
    return-value))

(defmethod update ((object passive-class) &optional &key id name class nodes-list model value)
  (handler-case
      (let ((return-value (call-next-method object :id id :name name)))      
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
      (format *error-output* "Only value or model shall be selected for ~a.~%" (text condition))
      nil)))

(defmethod update ((object coupling-class) &optional &key id name elements-list model value)
  (handler-case
      (let ((return-value (call-next-method object :id id :name name)))
	(when elements-list
	  (setf (coupling-class-elements-list return-value) elements-list))
	(when model
	  (setf (coupling-class-model return-value) model))
	(when value
	  (setf (coupling-class-value object) value))
	return-value)
    (value-or-model-entry-error (condition)
      (format *error-output* "Only value or model shall be selected for ~a.~%" (text condition))
      nil)))

(defmethod update ((object source-class) &optional &key id name class nodes-list model value)
  (let ((return-value (call-next-method object :id id :name name)))
    (when class
      (setf (source-class-class return-value) class))
    (if nodes-list
	(setf (source-class-nodes-list return-value) nodes-list))
    (when model
      (setf (source-class-model return-value) model))
    (when value
      (setf (source-calss-value return-value) value))
    return-value))

(defmethod update ((object subcircuit-class) &optional &key id name file-pathname nodes-list)
  (let ((return-value (call-next-method objexct :id id :name name)))
    (when file-pathname
      (setf (subcircuit-class-file-pathname return-value) file-pathname))
    (when nodes-list
      (setf (subcircuit-class-nodes-list return-value) nodes-list))
    return-value))

(defmethod update ((object model-class) &optional &key id name class file-pathname parameters-list function-name value)
  (let ((return-value (call-next-method object :id id :name name)))    
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

(defmethod update ((object probe-class) &optional &key id name class elements-list nodes-list)
  (let ((return-value (call-next-method object :id id :name name)))
    (when class
      (setf (probe-class-class return-value) class))
    (when elements-list
      (setf (probe-class-elements-list return-value) elements-list))
    (when nodes-list
      (setf (probe-class-nodes-list return-value) nodes-list))
    return-value))

(defmethod update ((object initial-condition-class) &optional &key id name target-name value)
  (let ((return-value (call-next-method object :id id :name name)))
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
    (setf nodes-list (append nodes-list (list node)))
    (setf (netlist-class-elements-list return-value) (append elements-list nodes-list))
    return-value))

;;;
;;; merge element nodes to connect to the target netlist
;;;

(defmethod merge-element ((object probe-class) nodes-pairs &optional &key (debug-mode nil) (output *standard-output*))
  (let ((source-element object))
    (when debug-mode
      (format output "~%Merging ~a -> " (sexpify source-element)))
    (dolist (nodes-pair nodes-pairs)
      (when (voltage-probe-class-p source-element)
	(setf (probe-class-nodes-list source-element) (substitute-if (second nodes-pair) #'(lambda (x)
											     (equalp x (first nodes-pair))) (probe-class-nodes-list source-element)))))
    (when debug-mode
      (format output "~a." (sexpify source-element)))
    source-element))

(defmethod merge-element ((object model-class) nodes-pairs &optional &key (debug-mode nil) (output *standard-output*))
  (let ((source-element object))
    (when debug-mode
      (format output "~%Merging ~a -> " (sexpify source-element)))
    (dolist (source-element-probe (model-class-probes-list source-element))
      (setf source-element-probe (merge-element source-element-probe nodes-pairs :debug-mode debug-mode :output output)))
    (when debug-mode
      (format output "~a" (sexpify source-element)))
    source-element))

(defmethod merge-element ((object passive-class) nodes-pairs &optional &key (debug-mode nil) (output *standard-output*))
  (let ((source-element object))
    (when debug-mode
      (format output "~%Merging ~a -> " (sexpify source-element)))
    (dolist (nodes-pair nodes-pairs)
      (setf (passive-class-nodes-list source-element) (substitute-if (second nodes-pair) #'(lambda (x)
										       (equalp x (first nodes-pair))) (passive-class-nodes-list source-element)))
      (when (passive-class-model source-element)
	(setf (passive-class-model source-element) (merge-element (passive-class-model source-element) nodes-pairs :debug-mode debug-mode :output output))))
    (when debug-mode 
      (format output "~a." (sexpify source-element)))
    source-element))

(defmethod merge-element ((object coupling-class) nodes-pairs &optional &key (debug-mode nil) (output *standard-output*))
  (let ((source-element object))
    (when debug-mode
      (format output "~%Merging ~a -> " (sexpify source-element)))
    (dolist (coupling-element (coupling-class-elements-list source-element))
      (setf coupling-element (merge-element coupling-element nodes-pairs :debug-mode debug-mode :output output)))
    (when (coupling-class-model source-element)
      (setf (coupling-class-model source-element) (merge-element (coupling-class-model source-element) nodes-pairs :debug-mode debug-mode :output output)))
    (when debug-mode
      (format output "~a" (sexpify source-element)))
    source-element))

(defmethod merge-element ((object subcircuit-class) nodes-pairs &optional &key (debug-mode nil) (output *standard-output*))
  (let ((source-element object))
    (when debug-mode
      (format output "~%Merging ~a -> " (sexpify source-element)))
    (dolist (nodes-pair nodes-pairs)
      (setf (subcircuit-class-nodes-list source-element) (substitute-if (second nodes-pair) #'(lambda (x)
										       (equalp x (first nodes-pair))) (subcircuit-class-nodes-list source-element))))
    (when debug-mode
      (format output "~a." (sexpify source-element)))
    source-element))

;;;
;;; connect netlist2 to netlist1 (conversely netlist1 to netlist2) resulting a netlist
;;; user must provide a node to node matrix e.g.: 
;;;               (("netlist1:N1" "netlist2:N1") ("netlist1:N2" "netlist2:N10"))
;;;

(defun connect (source-netlist target-netlist nodes-pairs &optional &key (debug-mode nil) (output *standard-output*))
  (let ((return-value target-netlist)
	(source-elements-list (exclude (where :class-type 'node-class) (netlist-class-elements-list source-netlist)))
	(target-elements-list (exclude (where :class-type 'node-class) (netlist-class-elements-list target-netlist)))
	(source-nodes-list (select (where :class-type 'node-class) (netlist-class-elements-list source-netlist)))
	(target-nodes-list (select (where :class-type 'node-class) (netlist-class-elements-list target-netlist))))
    (when debug-mode
      (format output "~%~%Connecting ~a to ~a with connections ~a." (element-class-name source-netlist) (element-class-name target-netlist) nodes-pairs))
    (dolist (nodes-pair nodes-pairs)
      (setf source-nodes-list (remove-if #'(lambda (x)
					     (equalp (first nodes-pair) (element-class-name x))) source-nodes-list)))
    (when debug-mode
      (format output "~%~%Nodes to merge:~%~a" (mapcar #'sexpify source-nodes-list)))
    (setf target-nodes-list (append target-nodes-list source-nodes-list))
    (dolist (source-element source-elements-list)
      (setf source-element (merge-element source-element nodes-pairs :debug-mode debug-mode :output output))
      (setf target-elements-list (append target-elements-list (list source-element))))
    (setf (netlist-class-elements-list return-value) target-elements-list)
    (setf (netlist-class-elements-list return-value) (append (netlist-class-elements-list return-value) target-nodes-list))
    return-value))

;;;
;;; read a netlist from a file
;;;

(defun read-netlist (file-pathname &optional &key (debug-mode nil) (output *standard-output*))
  "Read a complete netlist from an existing file."
  (handler-case
      (let ((*package* (find-package :circuit-solver)))
	(when debug-mode
	  (format output "~a~%" file-pathname))
	(with-open-file (input-file-stream file-pathname :direction :input :if-does-not-exist nil)
	  (if input-file-stream
	      (objectify (read input-file-stream))
	      (error 'file-not-found-error :file-pathname file-pathname))))
    (file-not-found-error (condition)
      (format *error-output* "~%file ~a does not exist.~%" (file-pathname condition))
      nil)))

;;;
;;; write a netlist in a file
;;;

(defun write-netlist (file-name netlist)
  "Write a complete netlist in a file."
  (with-open-file (out file-name :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print (mapcar #'sexpify netlist-class) 
	     out))))

;;;
;;; find node position in nodes list
;;;

(defun find-node-position (name nodes-list)
  (position name nodes-list
	    :key (function element-class-name)
	    :test (function string-equal)))

(defun merge-names (parent child)
  (concatenate 'string parent ":" child))

;;;
;;; rename netlist
;;;

(defmethod rename-netlist-element ((object element-class) name &optional &key (debug-mode nil) (output *standard-output*))
  (let ((return-value object))
    (when debug-mode
      (format output "~%Renaming ~a type ~a to " (element-class-name return-value) (type-of return-value)))
    (setf (element-class-name return-value) (merge-names name (element-class-name return-value)))
    (when debug-mode
      (format output "~a." (element-class-name return-value)))
    return-value))

(defmethod rename-netlist-element ((object probe-class) name &optional &key (debug-mode nil) (output *standard-output*))
  (handler-case
      (let ((return-value (call-next-method object name :debug-mode debug-mode :output output)))
	(cond
	  ((voltage-probe-class-p return-value)
	   (setf (probe-class-nodes-list return-value) (mapcar #'(lambda (x)
								   (merge-names name x)) (probe-class-nodes-list return-value))))
	  ((current-probe-class-p return-value)
	   (setf (probe-class-elements-list return-value) (mapcar #'(lambda (x)
								      (merge-names name x)) (probe-class-elements-list return-value))))
	  (t
	   (error 'undefined-probe-type-error :probe-type (element-class return-value) :probe-name (element-class-name return-value))))
	return-value)
    (undefined-probe-type-error (condition)
      (format *error-output* "~%Undefined probe type ~a for ~a." (probe-type condition) (probe-name condition))
      nil)))

(defmethod rename-netlist-element ((object model-class) name &optional &key (debug-mode nil) (output *standard-output*))
  (let ((return-value (call-next-method object name :debug-mode debug-mode :output output)))
    (when (element-probes-list return-value)
      (setf (element-probes-list return-value) (mapcar #'(lambda (x)
							   (rename-netlist-element x name debug-mode)) (element-probes-list return-value))))
    return-value))

(defmethod rename-netlist-element ((object passive-class) name &optional &key (debug-mode nil) (output *standard-output*))
  (let ((return-value (call-next-method object name :debug-mode debug-mode :output output)))
    (when (passive-class-model return-value)
      (setf (passive-class-model return-value) (rename-netlist-element (passive-class-model return-value) name debug-mode)))
    (when (passive-class-nodes-list return-value)
      (setf (passive-class-nodes-list return-value) (mapcar #'(lambda (x)
							  (concatenate 'string name ":" x)) (passive-class-nodes-list return-value))))
    return-value))

(defmethod rename-netlist-element ((object source-class) name &optional &key (debug-mode nil) (output *standard-output*))
  (let ((return-value (call-next-method object name :debug-mode debug-mode :output output)))
    (when (source-class-model return-value)
      (setf (source-class-model return-value) (rename-netlist-element (source-class-model return-value) name debug-mode)))
    (when (source-class-nodes-list return-value)
      (setf (source-class-nodes-list return-value) (mapcar #'(lambda (x)
							  (concatenate 'string name ":" x)) (source-class-nodes-list return-value))))
    return-value))

(defmethod rename-netlist-element ((object coupling-class) name &optional &key (debug-mode nil) (output *standard-output*))
  (let ((return-value (call-next-method object name :debug-mode debug-mode :output output)))
    (dolist (coupling-element (coupling-class-elements-list return-value))
      (setf coupling-element (rename-netlist-element coupling-element name :debug-mode debug-mode :output output)))
    (when (coupling-class-model return-value)
      (setf (coupling-class-model return-value) (rename-netlist-element (coupling-class-model return-value) name :debug-mode debug-mode :output output)))
    return-value))

(defmethod rename-netlist-element ((object subcircuit-class) name &optional &key (debug-mode nil) (output *standard-output*))
  (let ((return-value (call-next-method object name :debug-mode debug-mode :output output)))
    (setf (subcircuit-class-nodes-list return-value) (mapcar #'(lambda (x)
							(concatenate 'string name ":" x)) (subcircuit-class-nodes-list return-value)))
    return-value))

(defmethod rename-netlist-element ((object netlist-class) name &optional &key (debug-mode nil) (output *standard-output*))
  (let ((return-value object))
    (when debug-mode
      (format output "~%~%Renaming netlist ~a to " (element-class-name object)))
    (setf (element-class-name return-value) (concatenate 'string name ":" (element-class-name return-value)))
    (when debug-mode
      (format output "~a." (element-class-name return-value)))
    (dolist (element (netlist-class-elements-list return-value))
      (setf my-element (rename-netlist-element element name :debug-mode debug-mode :output output)))
    return-value))

(defmethod element-with-node ((object passive-class) node-name)
  (when (position node-name (passive-class-nodes-list object) :test 'string-equal)
    object))

(defmethod element-with-node ((object source-class) node-name)
  (when (position node-name (source-class-nodes-list object) :test 'string-equal)
    object))

(defmethod element-with-node ((object subcircuit-class) node-name)
  (when (position node-name (subcircuit-class-nodes-list object) :test 'string-equal)
    object))

(defmethod element-with-node ((object probe-class) node-name)
  (when (position node-name (probe-class-nodes-list object) :test 'string-equal)
    object))

(defmethod element-with-node ((object coupling-class) node-name)
  (let ((return-value nil))
    (dolist (coupling-element (coupling-class-elements-list object))
      (when (position node-name (passive-class-nodes-list object) :test 'string-equal)
	(push coupling-element return-value)))
    return-value))

(defmethod element-with-node ((object node-class) node-name)
  (when (string-equal node-name (element-class-name object))
    object))

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

(defun include-subcircuits (netlist &optional &key (debug-mode nil) (output *standard-output*))
  (handler-case
      (let ((return-value netlist)
	    (subcircuit-calls-list nil))
	(format output "~%Including subcircuits.")
	(loop do
	     (setf subcircuit-calls-list (select (where :class-type 'subcircuit-class) 
						 (netlist-class-elements-list return-value)))
	     (when subcircuit-calls-list
	       (when debug-mode
		 (format output "~%~%Subcircuit calls: ~a~%" (mapcar #'sexpify subcircuit-calls-list)))
	       (let ((subcircuits-list (mapcar #'(lambda (x)
						   (read-netlist (subcircuit-class-file-pathname x) :debug-mode debug-mode :output output)) subcircuit-calls-list))
		     (subcircuit nil)
		     (i 0)) 
		 (dolist (subcircuit-call subcircuit-calls-list)
		   (setf subcircuit (nth i subcircuits-list))
		   (when debug-mode
		     (format output "~%~%Original subcircuit netlist:~%~a" (sexpify subcircuit)))
		   (setf subcircuit (rename-netlist-element subcircuit (element-class-name subcircuit-call) :debug-mode debug-mode :output output))
		   (let ((subcircuit-nodes-list (select (where :class-type 'node-class) 
							(netlist-class-elements-list subcircuit))))
		     (when (> (length (subcircuit-class-nodes-list subcircuit-call)) (length subcircuit-nodes-list))
		       (error 'wrong-subcircuit-nodes-list-error :subcircuit-name (element-class-name subcircuit) 
			      :actual-nodes-count (length subcircuit-nodes-list) 
			      :needed-nodes-count (length (subcircuit-class-nodes-list subcircuit-call))))
		     (let ((connection-nodes-list nil))
		       (loop for i from 0 below (length (subcircuit-class-nodes-list subcircuit-call)) do
			    (setf connection-nodes-list (append connection-nodes-list (list (list (element-class-name (nth i subcircuit-nodes-list)) 
												  (nth i (subcircuit-class-nodes-list subcircuit-call)))))))
		       (when debug-mode
			 (format output "~%~%Connections nodes pairs ~a" connection-nodes-list))
		       (setf return-value (connect subcircuit return-value connection-nodes-list :debug-mode debug-mode :output output))
		       (setf (netlist-class-elements-list return-value) (exclude (where :name (element-class-name subcircuit-call) :class-type 'subcircuit-class) 
										 (netlist-class-elements-list return-value)))
		       (when debug-mode
			 (format output "~%~%Resulting netlist:~%~a" (sexpify return-value)))))
		   (incf i))))
	   until (eql subcircuit-calls-list nil))
	(format output " Done!")
	return-value)
    (wrong-subcircuit-nodes-list-error (condition)
      (format *error-output* "~%Subcircuit ~a has got ~a instead of ~a in the include command." (subcircuit-name condition) (actual-nodes-count condition) (needed-nodes-count condition)))))

(defun check-netlist (netlist &optional &key (debug-mode nil) (output *standard-output*))
  (handler-case
      (let* ((elements-list (exclude (where :class-type 'node-class) (netlist-class-elements-list netlist)))
	     (nodes-list (select (where :class-type 'node-class) (netlist-class-elements-list netlist)))
	     (error-found 0))
	(when (< (length nodes-list) 2)
	  (format output "Less than two nodes for netlist ~a.~%" (element-class-name netlist))
	  (setf error-found 1))
	(let ((reference-nodes (select (list (where :class "reference")
					     (where :class "gnd")
					     (where :class "0")) nodes-list)))
	  (cond
	    ((eql (length reference-nodes) 0)
	     (format output "~%No reference node in netlist ~a." (element-class-name netlist))
	     (setf error-found 2))
	    ((> (length reference-nodes) 1)
	     (format output "~%Too many reference nodes in netlist ~a: ~a." (element-class-name netlist) (mapcar #'sexpify reference-nodes))
	     (setf error-found 3))))
	(dolist (element elements-list)
	  (when (> (length (select (where :name (element-class-name element)) elements-list)) 1)
	    (format output "~%Object ~a defined more than once." (element-class-name element))
	    (setf error-found 4))
	  (typecase element
	    (coupling-class
	     (when (< (length (coupling-class-elements-list element)) 2)
	       (error 'wrong-number-of-elements-for-coupling-error :element-name (element-class-name element))
	       (setf error-found 5))
	     (unless (eql (/ (* (length (coupling-class-elements-list element))
				(1- (length (coupling-class-elements-list element)))) 2)
			  (grid:dim0 (coupling-class-value element)))
	       (error 'mismatched-number-of-coupling-values-vs-coupling-inductances-error :coupling-name (element-class-name element)))
	     (unless (check-objects (coupling-class-elements-list element) (list (where :class "inductance")
										 (where :class "capacitance")))
	       (error 'invalid-passive-element-in-coupling :coupling-name (element-class-name element))))))
	error-found)
    (wrong-number-of-elements-for-coupling-error (condition)
      (format *error-output* "~%Less than coupling elements in ~a.~%" (element-name condition))
      nil)
    (mismatched-number-of-coupling-values-vs-coupling-inductances-error (condition)
      (format *error-output "~%Coupling ~a mismatched number of coupling values vs coupling inductances." (coupling-name condition))
      nil)
    (invalid-passive-element-in-coupling (condition)
      (format *error-output* "~%Invalid passive element in coupling ~a~%" (coupling-name condition))
      nil)))
	      

;;;
;;; create K or Y vector
;;;

(defun create-k-y-vector (netlist &optional &key (debug-mode nil) (output *standard-output*))
  (let ((elements-list (select (list (where :class-type 'source-class)
					(where :class-type 'passive-class)) netlist))
	(couplings-list (select (where :class-type 'coupling-class) netlist))
	(nodes-list (exclude (list (where :class "reference") 
				   (where :class "REFERENCE")
				   (where :class "gnd")
				   (where :class "GND")
				   (where :class "0")) (select (where :class-type 'node-class) netlist)))
	(k-y-vector nil)
	(k-y-rows 0))
    (when debug-mode
      (format output "~%Creating K or Y vector: "))
    (setf k-y-rows (length nodes-list))
    (incf k-y-rows (length elements-list))
    (dolist (coupling couplings-list)
      (incf k-y-rows (length (coupling-class-elements-list coupling))))
    (when (> k-y-rows 0)
      (setf k-y-vector (grid:make-foreign-array 'double-float :dimensions k-y-rows :initial-element 0d0)))
    (when debug-mode
      (if (> k-y-rows 0)
	  (format output "(~a)." k-y-rows)
	  (format output "ø.")))
    k-y-vector))

;;;
;;; create the p-matrix once for all
;;;

(defun create-p-matrix (netlist &optional &key (debug-mode nil) (output *standard-output*))
  (let ((nodes-list (select (where :class-type 'node-class) netlist))
	(couplings-list (select (where :class-type 'coupling-class) netlist))
	(p-matrix nil)
	(p-rows 0)
	(p-cols 0))
    (when debug-mode
      (format output "~%Creating P matrix: "))
    (setf p-rows (1- (length nodes-list)))
    (setf p-cols (length (select (list (where :class-type 'source-class)
				       (where :class-type 'passive-class)) netlist)))
    (dolist (coupling couplings-list)
      (incf p-cols (length (coupling-class-elements-list coupling))))
    (when (and (> p-cols 0) (> p-rows 0))
      (setf p-matrix (grid:make-foreign-array 'double-float :dimensions (list p-rows p-cols) :initial-element 0d0)))
    (when debug-mode
      (if (and (> p-cols 0) (> p-rows 0))
	  (format output "P(~a x ~a)." p-rows p-cols)
	  (format output "P = ø.")))
    p-matrix))

;;;
;;; create g-point matrix
;;;

(defun create-g-c-matrix (netlist &optional &key (debug-mode nil) (output *standard-output*))
  (let* ((nodes-list (select (where :class-type 'node-class) netlist))
	 (elements-list (select (where :class-type 'passive-class) netlist))
	 (couplings-list (select (where :class-type 'coupling-class) netlist))
	 (g-c-matrix nil)
	 (g-c-rows 0)
	 (g-c-cols 0))
    (when debug-mode
      (format output "~%Creating G or C matrix: "))
    (setf g-c-cols (1- (length nodes-list)))
    (setf g-c-rows (length elements-list))
    (dolist (coupling couplings-list)
      (incf g-c-rows (length (coupling-class-elements-list coupling))))
    (when (and (> g-c-rows 0) (> g-c-cols 0))
      (setf g-c-matrix (grid:make-foreign-array 'double-float :dimensions (list g-c-rows g-c-cols) :initial-element 0d0)))
    (when debug-mode
      (if (and (> g-c-rows 0) (> g-c-cols 0))
	  (format output "(~a, ~a)." g-c-rows g-c-cols)
	  (format output "ø.")))
    g-c-matrix))

;;;
;;; create l-matrix for synchronous machine abduction
;;;

(defun create-r-l-matrix (netlist &optional &key (debug-mode nil) (output *standard-output*))
  (let* ((elements-list (select (where :class-type 'passive-class) netlist))
	 (couplings-list (select (where :class-type 'coupling-class) netlist))
	 (r-l-matrix nil)
	 (r-l-rows 0)
	 (r-l-cols 0))
    (when debug-mode
      (format output "~%Creating R or L matrix: "))
    (setf r-l-cols (length (select (list (where :class-type 'passive-class)
					 (where :class-type 'source-class)) netlist)))
    (setf r-l-rows (length elements-list))
    (dolist (coupling couplings-list)
      (incf r-l-rows (length (coupling-class-elements-list coupling)))
      (incf r-l-cols (length (coupling-class-elements-list coupling))))
    (when (and (> r-l-rows 0) (> r-l-cols 0))
      (setf r-l-matrix (grid:make-foreign-array 'double-float :dimensions (list r-l-rows r-l-cols) :initial-element 0d0)))
    (when debug-mode
      (if (and (> r-l-rows 0) (> r-l-cols 0))
	  (format output "(~a, ~a)." r-l-rows r-l-cols)
	  (format output "ø.")))
    r-l-matrix))

;;;
;;; create a beautiful Si matrix
;;;

(defun create-si-matrix (netlist &optional &key (debug-mode nil) (output *standard-output*))
  (let* ((elements-list (select (list (where :class-type 'passive-class)
				      (where :class-type 'source-class)) netlist))
	 (current-sources-list (select (list (where :class-type 'source-class :class "current-source")) netlist))
	 (couplings-list (select (where :class-type 'coupling-class) netlist))
	 (si-matrix nil)
	 (si-rows 0)
	 (si-cols 0))
    (when debug-mode
      (format output "~%Creating Si matrix: "))
    (setf si-rows (length current-sources-list))
    (setf si-cols (length elements-list))
    (dolist (coupling couplings-list)
      (incf si-cols (length (coupling-class-elements-list coupling))))
    (when (and (> si-rows 0) (> si-cols 0))
      (setf si-matrix (grid:make-foreign-array 'double-float :dimensions (list si-rows si-cols) :initial-element 0d0)))
    (when debug-mode
      (if (and (> si-rows 0) (> si-cols 0))
	  (format output "Si(~a, ~a)." si-rows si-cols)
	  (format output "Si = ø.")))
    si-matrix))

;;;
;;; create the Sv matrix for your joy
;;;

(defun create-sv-matrix (netlist &optional &key (debug-mode nil) (output *standard-output*))
  (let ((nodes-list (select (where :class-type 'node-class) netlist))
	(voltage-sources-list (select (list (where :class-type 'source-class :class "voltage-source")) netlist))
	(sv-matrix nil)
	(sv-rows 0)
	(sv-cols 0))
    (when debug-mode
      (format output "~%Creating Sv matrix: "))
    (setf sv-rows (length voltage-sources-list))
    (setf sv-cols (1- (length nodes-list)))
    (when (and (> sv-rows 0) (> sv-cols 0))
      (setf sv-matrix (grid:make-foreign-array 'double-float :dimensions (list sv-rows sv-cols) :initial-element 0d0)))
    (when debug-mode
      (if (and (> sv-rows 0) (> sv-cols 0))
	  (format output "Sv(~a, ~a)." sv-rows sv-cols)
	  (format output "Sv = ø.")))
    sv-matrix))

;;
;; create know factor matrix (current sources part).
;;

(defun create-ki-vector (netlist &optional &key (debug-mode nil) (output *standard-output*))
  (let ((current-sources-list (select (list (where :class-type 'source-class :class "current-source")) netlist))
	(ki-vector nil)
	(ki-rows 0))
    (when debug-mode
      (format output "~%Creating Ki matrix: "))
    (setf ki-rows (length current-sources-list))
    (when (> ki-rows 0)
      (setf ki-vector (grid:make-foreign-array 'double-float :dimensions ki-rows :initial-element 0d0)))
    (when debug-mode
      (if (> ki-rows 0)
	  (format output "Ki(~a)." ki-rows)
	  (format output "Ki = ø.")))
    ki-vector))

;;
;; create the kv part of "sto cazzo".
;;

(defun create-kv-vector (netlist &optional &key (debug-mode nil) (output *standard-output*))
  (let ((voltage-sources-list (select (list (where :class-type 'source-class :class "voltage-source")) netlist))
	(kv-vector nil)
	(kv-rows 0))
    (when debug-mode
      (format output "~%Creating Kv matrix: "))
    (setf kv-rows (length voltage-sources-list))
    (when (> kv-rows 0)
      (setf kv-vector (grid:make-foreign-array 'double-float :dimensions kv-rows :initial-element 0d0)))
    (when debug-mode
      (if (> kv-rows 0)
	  (format output "Kv(~a)." kv-rows)
	  (format output "Kv = ø.")))
    kv-vector))

;;
;; update functions stuff
;;

(defun update-p-matrix (p-matrix netlist &optional &key (debug-mode nil) (output *standard-output*))
  (let ((nodes-list (select (where :class-type 'node-class) netlist))
	(elements-list (select (list (where :class-type 'source-class)
				     (where :class-type 'passive-class)
				     (where :class-type 'coupling-class)) netlist))
	(i 0)
	(j 0))
    (when debug-mode
      (format output "~%~%---- update-p-matrix ---- ~%~%")
      (format output "Updating P[~a x ~a].~%" (grid:dim0 p-matrix) (grid:dim1 p-matrix)))
    (dolist (node (exclude (list (where :class "reference")
				 (where :class "gnd")
				 (where :class "0")) nodes-list))
      (setf j 0)
      (let ((node-name (element-class-name node)))
	(dolist (element elements-list)
	  (typecase element
	    (source-class
	     (let ((element-node-names-list (source-class-nodes-list element)))
	       (when (string-equal node-name (first element-node-names-list))
		 (setf (grid:gref p-matrix i j) -1d0)
		 (when debug-mode
		   (format output "P(~a, ~a) = ~a.~%" i j (grid:gref p-matrix i j))))
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
		       (format output "P(~a, ~a) = ~a.~%" i j (grid:gref p-matrix i j))))
		   (when (string-equal node-name (second element-node-names-list))
		     (setf (grid:gref p-matrix i j) +1d0)))
		 (incf j)))))))
      (incf i))
    (when debug-mode
      (format output "P =~%~a~%" p-matrix))
    p-matrix))

;;; 
;;; update R matrix
;;;

(defun update-r-matrix (r-matrix netlist &optional &key (debug-mode nil) (output *standard-output*))
  (let ((elements-list (select (list (where :class-type 'passive-class)
				     (where :class-type 'coupling-class)
				     (where :class-type 'source-class)) netlist))
	(i 0)
	(j 0))
    (when debug-mode
      (format output "~%~%---- update-r-matrix ----~%~%")
      (format output "Updating R[~a x ~a].~%" (grid:dim0 r-matrix) (grid:dim1 r-matrix)))
    (dolist (element elements-list)
      (typecase element
	(coupling-class
	 (setf j (+ j (length (coupling-class-elements-list element))))
	 (setf i (+ i (length (coupling-class-elements-list element)))))
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
      (format output "R =~%~a~%" r-matrix))
    r-matrix))

(defmethod submatrix-update ((element passive-class) i matrix &optional &key (debug-mode nil) (output *standard-output*))
  (handler-case
      (let ((element-node-names-list (passive-class-nodes-list element))	   
	    (j 0)
	    (k 0))
	(cond
	  ((or (resistance-class-p element)
	       (inductance-class-p element))
	   (setf k 0)
	   (dolist (element-node-name element-node-names-list)
	     (let ((found-node (find-element (where :name element-node-name) nodes-list)))
	       (unless found-node
		 (error 'no-such-node-for-element-error :node-name element-node-name :element-name (element-class-name element)))
	       (setf j (find-node-position element-node-name (exclude (list (where :class "reference")
									    (where :class "gnd")
									    (where :class "0")) nodes-list)))
	       (unless (reference-class-node-p found-node)
		 (setf (grid:gref g-matrix i j) (expt -1d0 k)))
	       (incf k))))
	  ((conductance-class-p element)
	   (setf k 0)
	   (dolist (element-node-name element-node-names-list)
	     (let ((found-node (find-element (where :name element-node-name) nodes-list)))
	       (unless found-node
		 (error 'no-such-node-for-element-error :node-name element-node-name :element-name (element-class-name element)))
	       (setf j (find-node-position element-node-name (exclude (list (where :class "reference")
									    (where :class "gnd")
									    (where :class "0")) nodes-list)))
	       (unless (reference-class-node-p found-node)
		 (setf (grid:gref g-matrix i j) (* (expt -1d0 k) (passive-class-value element))))
	       (incf k)))))
	matrix)
    (no-such-node-for-element-error (condition)
      (format *error-output* "No such node ~a for element ~a.~%" (node-name condition) (element-name condition))
      nil)))    

(defmethod submatrix-update ((element coupling-class) i matrix &optional &key (debug-mode nil) (output *standard-output*))
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
		 (error 'mismatched-coupling-element :element-name (element-class-name element)))
	       (setf k 0)
	       (dolist (element-node-name element-node-names-list)
		 (let ((found-node (find-element (where :name element-node-name) nodes-list)))
		   (unless found-node
		     (error 'no-such-node-for-element :node-name element-node-name :element-name (element-class-name coupling-element)))
		   (setf j (find-node-position element-node-name (exclude (list (where :class "reference")
										(where :class "gnd")												   
										(where :class "0")) nodes-list)))
		   (unless (reference-class-node-p found-node)
		     (setf (grid:gref g-matrix i j) (expt -1d0 k)))
		   (incf k)))))
	    (t
	     (error 'mismatched-coupling-element :element-name (element-class-name element)))))
	matrix)
    (no-such-node-for-element-error (condition)
      (format *error-output* "No such node ~a for element ~a.~%" (node-name condition) (element-name condition))
      nil)
    (mismatched-coupling-element (condition)
      (format *error-output* "Coupling ~a must contains only inductances or capacitances.~%" (element-name condition))
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

(defun update-g-matrix (g-matrix netlist &optional &key (debug-mode nil) (output *standard-output*))
  (handler-case
      (let ((elements-list (select (list (where :class-type 'source-class)
					 (where :class-type 'passive-class)
					 (where :class-type 'coupling-class)) netlist))
	    (nodes-list (select (where :class-type 'node-class) netlist))
	    (i 0)
	    (j 0)
	    (k 0))
	(when debug-mode
	  (format output "~%~%---- update-g-matrix ----~%~%")
	  (format output "Updating G[~a x ~a].~%" (grid:dim0 g-matrix) (grid:dim1 g-matrix)))
	(dolist (element elements-list)
	  (typecase element
	    (passive-class
	     (let ((element-node-names-list (passive-class-nodes-list element)))
	       (cond
		 ((or (resistance-class-p element)
		      (inductance-class-p element))
		  (setf k 0)
		  (dolist (element-node-name element-node-names-list)
		    (let ((found-node (find-element (where :name element-node-name) nodes-list)))
		      (unless found-node
			(error 'no-such-node-for-element-error :node-name element-node-name :element-name (element-class-name element)))
		      (setf j (find-node-position element-node-name (exclude (list (where :class "reference")
										   (where :class "gnd")
										   (where :class "0")) nodes-list)))
		      (unless (reference-class-node-p found-node)
			(setf (grid:gref g-matrix i j) (expt -1d0 k)))
		      (incf k))))
		 ((conductance-class-p element)
		  (setf k 0)
		  (dolist (element-node-name element-node-names-list)
		    (let ((found-node (find-element (where :name element-node-name) nodes-list)))
		      (unless found-node
			(error 'no-such-node-for-element-error :node-name element-node-name :element-name (element-class-name element)))
		      (setf j (find-node-position element-node-name (exclude (list (where :class "reference")
										   (where :class "gnd")
										   (where :class "0")) nodes-list)))
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
		      (error 'mismatched-coupling-element :element-name (element-class-name element)))
		    (setf k 0)
		    (dolist (element-node-name element-node-names-list)
		      (let ((found-node (find-element (where :name element-node-name) nodes-list)))
			(unless found-node
			  (error 'no-such-node-for-element :node-name element-node-name :element-name (element-class-name coupling-element)))
			(setf j (find-node-position element-node-name (exclude (list (where :class "reference")
										     (where :class "gnd")												   
										     (where :class "0")) nodes-list)))
			(unless (reference-class-node-p found-node)
			  (setf (grid:gref g-matrix i j) (expt -1d0 k)))
			(incf k)))))
		 (t
		  (error 'mismatched-coupling-element :element-name (element-class-name element))))	   
	       (incf i)))))
	(when debug-mode
	  (format output "G =~%~a~%" g-matrix))
	g-matrix)
    (no-such-node-for-element-error (condition)
      (format *error-output* "No such node ~a for element ~a.~%" (node-name condition) (element-name condition))
      nil)
    (mismatched-coupling-element (condition)
      (format *error-output* "Coupling ~a must contains only inductances or capacitances.~%" (element-name condition))
      nil)))

;;;
;;; update Si matrix
;;;

(defun update-si-matrix (si-matrix netlist &optional &key (debug-mode nil) (output *standard-output*))
  (handler-case
      (let ((elements-list (select (list (where :class-type 'passive-class)
					 (where :class-type 'coupling-class)
					 (where :class-type 'source-class)) netlist))
	    (i 0)
	    (j 0))
	(when debug-mode
	  (format output "~%~%---- update-si-matrix ----~%~%")
	  (format output "Updating Si[~a x ~a].~%" (grid:dim0 si-matrix) (grid:dim1 si-matrix)))
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
	  (format output "Si =~%~a~%" si-matrix))
	si-matrix)))

;;;
;;; update Sv matrix
;;;

(defun update-sv-matrix (sv-matrix netlist &optional &key (debug-mode nil) (output *standard-output*))
  (handler-case
      (let ((sources-list (select (where :class-type 'source-class :class "voltage-source") netlist))
	    (nodes-list (select (where :class-type 'node-class) netlist))
	    (i 0)
	    (j 0)
	    (k 0))
	(when debug-mode
	  (format output "~%~%---- update-sv-matrix ----~%~%")
	  (format output "Updating Sv[~a x ~a].~%" (grid:dim0 sv-matrix) (grid:dim1 sv-matrix)))
	(dolist (source sources-list)
	  (setf k 0)
	  (dolist (node-name (source-class-nodes-list source))	    
	    (unless (reference-class-node-p (find-element (where :name node-name) nodes-list))
	      (setf j (1- (find-node-position node-name nodes-list)))
	      (unless j
		(error 'no-such-node-for-element-error :element-name (element-class-name source) :node-name node-name))
	      (setf (grid:gref sv-matrix i j) (expt -1d0 k)))
	    (incf k))
	  (incf i))
	(when debug-mode
	  (format output "Sv =~%~a~%" sv-matrix))
	sv-matrix)
    (no-such-node-for-element-error (condition)
      (format *error-output* "No such node ~a for element ~a.~%" (node-name condition) (element-name condition)))))

(defun update-l-matrix (l-matrix netlist &optional &key (debug-mode nil) (output *standard-output*))
  (let ((elements-list (select (list (where :class-type 'passive-class)
				     (where :class-type 'coupling-class)
				     (where :class-type 'source-class)) netlist))
	(i 0)
	(j 0)
	(k 0)
	(p 0)
	(m-value 0d0))
    (when debug-mode
      (format output "~%~%---- update-l-matrix ----~%~%")
      (format output "Updating L[~a x ~a].~%" (grid:dim0 l-matrix) (grid:dim1 l-matrix)))
    (dolist (element elements-list)
      (typecase element
	(passive-class
	 (when (inductance-class-p element) 
	   (setf (grid:gref l-matrix i j) (- (passive-class-value element))))
	 (incf i)
	 (incf j))
	(coupling-class
	 (setf k 0)
	 (setf m-value 1d0)	 
	 (dolist (coupling-element (coupling-class-elements-list element))
	   (setf (grid:gref l-matrix (+ k i) (+ k j)) (- (passive-class-value coupling-element)))
	   (incf k))
	 (when debug-mode
	   (format output "~%Coupling coefficients vector ~a.~%Coupling dimension ~a." (coupling-class-value element) k))
	 (loop for ii from 0 below k do
	      (loop for jj from 0 below k do
		   (unless (eql ii jj)
		     (when debug-mode
		       (format output "~%Mutual coupling (~a, ~a)." ii jj))
		     (if (> jj ii)
			 (setf p (/ (+ (* ii (- (* 2 k) ii 3)) (* 2 (- jj 1))) 2))
			 (setf p (/ (+ (* jj (- (* 2 k) jj 3)) (* 2 (- ii 1))) 2)))
		     (when debug-mode
		       (format output "~%Coupling coefficient position ~a.~%i = ~a, ii = ~a, j = ~a, jj = ~a, M(~a, ~a) = " p i ii j jj (+ i ii) (+ j jj)))
		     (setf m-value (* (grid:gref (coupling-class-value element) p) 
				      (sqrt (* (abs (grid:gref l-matrix (+ i ii) (+ j ii))) 
					       (abs (grid:gref l-matrix (+ i jj) (+ j jj)))))))
		     (when debug-mode
		       (format output "~a." m-value))
		     (setf (grid:gref l-matrix (+ i ii) (+ j jj)) (- m-value)))))
	 (incf i k)
	 (incf j k))
	(source-class
	 (incf j))))
    (when debug-mode
      (format output "~%L =~%~a~%" l-matrix))
    l-matrix))

;;
;; update C matrix
;;

(defun update-c-matrix (c-matrix netlist &optional &key (debug-mode nil) (output *standard-output*))
  (let ((elements-list (select (list (where :class-type 'source-class)
				     (where :class-type 'passive-class)
				     (where :class-type 'coupling-class)) netlist))
	(nodes-list (exclude (list (where :class "reference")
				   (where :class "gnd")
				   (where :class "0")) (select (where :class-type 'node-class) netlist)))
	(i 0)
	(j 0)
	(k 0))
    (when debug-mode
      (format output "~%~%---- update-c-matrix ----~%~%")
      (format output "Updating C[ ~a x ~a ].~%" (grid:dim0 c-matrix) (grid:dim1 c-matrix)))
    (dolist (element elements-list)
      (typecase element
	(passive-class
	 (when (capacitance-class-p element)
	   (setf k 1)
	   (dolist (node-name (passive-class-nodes-list element))
	     (setf j (find-node-position node-name nodes-list))
	     (when j
	       (setf (grid:gref c-matrix i j) (* (expt -1d0 (1+ k)) (passive-class-value element))))    
	     (incf k)))	     
	 (incf i))
	(coupling-class
	 (incf i (length (coupling-class-elements-list element))))))
    (when debug-mode
      (format output "C =~%~a~%" c-matrix))
    c-matrix))

;;;
;;; Update Ki vector
;;;

(defun update-ki-vector (ki-vector netlist &optional &key (debug-mode nil) (output *standard-output*))
  (let ((current-sources-list (select (list (where :class-type 'source-class :class "current-source")) netlist))
	(i 0))
    (when debug-mode
      (format output "~%~%---- update-ki-vector ----~%~%")
      (format output "Updating Ki[ ~a ].~%" (grid:dim0 ki-vector)))
    (dolist (current-source current-sources-list)
      (setf (grid:gref ki-vector i) (source-class-value current-source))
      (incf i))
    (when debug-mode
      (format output "Ki =~%~a~%" ki-vector))
    ki-vector))

;;;
;;; Update Kv vector
;;;

(defun update-kv-vector (kv-vector netlist &optional &key (debug-mode nil) (output *standard-output*))
  (let ((voltage-sources-list (select (list (where :class-type 'source-class :class "voltage-source")) netlist))
	(i 0))
    (when debug-mode
      (format output "~%~%---- update-kv-vector ----~%~%")
      (format output "Updating Kv[ ~a ].~%" (grid:dim0 kv-vector)))
    (dolist (voltage-source voltage-sources-list)
      (setf (grid:gref kv-vector i) (source-class-value voltage-source))
      (incf i))
    (when debug-mode
      (format output "Kv =~%~a~%" kv-vector))
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

(defun assemble-system (p-matrix r-matrix g-matrix si-matrix sv-matrix l-matrix c-matrix ki-vector kv-vector &optional &key (debug-mode nil) (output *standard-output*))
  (let ((a-matrix nil)
	(b-matrix nil)
	(k-vector nil))
    (when debug-mode
      (format output "Assembling A, B matrices and K vector.~%"))    
    (when (or p-matrix r-matrix g-matrix l-matrix c-matrix)
      (setf a-matrix (grid:concatenate-grids p-matrix
					     (grid:make-foreign-array 'double-float :dimensions (list (grid:dim0 p-matrix) 
												      (grid:dim1 g-matrix)) :initial-element 0d0) :axis 1))
      (setf a-matrix (grid:concatenate-grids a-matrix
					     (grid:concatenate-grids r-matrix g-matrix :axis 1) :axis 0))
      (when si-matrix
	(setf a-matrix (grid:concatenate-grids a-matrix 
					       (grid:concatenate-grids si-matrix
								       (grid:make-foreign-array 'double-float :dimensions (list (grid:dim0 si-matrix) 
																(grid:dim1 g-matrix)) :initial-element 0d0) :axis 1) :axis 0)))
      (when sv-matrix
	(setf a-matrix (grid:concatenate-grids a-matrix 
					       (grid:concatenate-grids (grid:make-foreign-array 'double-float :dimensions (list (grid:dim0 sv-matrix) 
																(grid:dim1 p-matrix)) :initial-element 0d0) sv-matrix :axis 1) :axis 0)))
      (when debug-mode
	(format output "~%A = ~a" a-matrix))
      (setf b-matrix (grid:make-foreign-array 'double-float :dimensions (list (grid:dim0 p-matrix)
									      (+ (grid:dim1 p-matrix)
										 (grid:dim1 g-matrix))) :initial-element 0d0))
      (setf b-matrix (grid:concatenate-grids b-matrix 
					     (grid:concatenate-grids l-matrix c-matrix :axis 1) :axis 0))
      (setf b-matrix (grid:concatenate-grids b-matrix
					     (grid:make-foreign-array 'double-float :dimensions (list (+ (grid:dim0 si-matrix)
													 (grid:dim0 sv-matrix))
												      (+ (grid:dim1 p-matrix)
													 (grid:dim1 g-matrix))) :initial-element 0d0) :axis 0))
      (when debug-mode
	(format output "~%B = ~a" b-matrix))
      (setf k-vector (grid:make-foreign-array 'double-float :dimensions (+ (grid:dim0 p-matrix)
									   (grid:dim0 g-matrix)) :initial-element 0d0)) 
      (when ki-vector
	(setf k-vector (grid:concatenate-grids k-vector ki-vector :axis 0)))
      (when kv-vector
	(setf k-vector (grid:concatenate-grids k-vector kv-vector :axis 0)))
      (when debug-mode
	(format output "~%K = ~a" k-vector)))
    (values a-matrix b-matrix k-vector)))

;;;
;;; evaluate a model:
;;; model state:
;;; (t0 t1 n time y)
;;;

(defun evaluate-model (model &optional &key (debug-mode nil) (output *standard-output*))
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
	(when function-symbol
	  (setf function (symbol-function function-symbol))
	  (setf old-function-value (apply function (list :parameters parameters-list :state states-list))))
	(unless function-symbol
	  (if (load (make-pathname :name function-name :type "vcs"))
	      (setf model (evaluate-model model :debug-mode debug-mode :output output))
	      (error 'unknown-function-error :function-name function-name)))
	(cond
	  ((simple-function-p model)
	   (setf (model-class-value model) old-function-value))
	  ((differential-function-p model)
	   (setf (model-class-value model) (+ value (* *h* old-function-value))))
	  (t
	   (error 'undefined-model-class-error :model-name name :model-class-name class)))
	(when debug-mode
	  (format output "Evaluated model: ~a~%" (sexpify model)))
	model)
    (unknown-function-error (condition)
      (format *error-output* "~%Could not find ~a function." (function-name condition))
      nil)
    (undefined-model-class-error (condition)
      (format *error-output* "~%Undefined class ~a for model ~a.~%" (model-name condition) (model-class-name condition))
      nil)))

;;;             
;;; Update all models in netlist	       
;;;
;;; determine state for variable for model: currents and/or voltages. Probes say which 
;;; of them should be selected for model calculations. The state vector is formed like:
;;;
;;;         position	|          meaning
;;;    ------------------+----------------------------
;;;            0         |   previous model value
;;;        1 ; m + 1     |    current variables
;;;    m + 2 ; m + n + 2 |    voltage variables
;;;    

(defun update-model (element netlist state-vector &optional &key (debug-mode nil) (output *standard-output*))
  (handler-case
      (let* ((elements-list (select (list (where :class-type 'source-class)
					  (where :class-type 'passive-class)
					  (where :class-type 'coupling-class)) netlist))
	     (nodes-list (select (where :class-type 'node-class) netlist))
	     (branches-number (- (grid:dim0 state-vector) (length nodes-list)))
	     (state nil)
	     (i 0))
	(typecase element
	  ((or source-class passive-class coupling-class)
	   (when (element-class-model element)
	     (when debug-mode
	       (format output "Evaluating model for ~a.~%" (element-class-name element)))
	     (setf (element-class-model element) (update-model (element-class-model element) netlist state-vector :debug-mode debug-mode :output output))
	     (setf (element-class-value element) (element-class-value (element-class-model element))))
	   (when (coupling-class-p element)
	     (dolist (inductance (coupling-class-elements-list element))
	       (setf inductance (update-model inductance netlist state-vector :debug-mode debug-mode :output output)))))
	  (model-class
	   (push (model-class-value element) state)
	   (when (model-class-probes-list element)
	     (dolist (probe (model-class-probes-list element))
	       (when probe
		 (cond
		   ((voltage-probe-class-p probe)
		    (dolist (node-name (probe-class-nodes-list probe))
		      (let ((node (first (select (where :class-type 'node-class :name node-name) nodes-list))))
			(unless node
			  (error 'no-node-for-probe-error :node-name node-name :probe-name (element-class-name probe)))
			(cond
			  ((reference-class-node-p node)
			   (push 0d0 state))
			  (t
			   (setf i (find-node-position node-name nodes-list))
			   (unless i
			     (error 'no-node-for-probe-error :node-name node-name :probe-name (element-class-name probe)))
			   (push (grid:gref state-vector (+ i branches-number)) state))))))
		   ((current-probe-class-p probe)
		    (dolist (element-name (probe-class-elements-list probe))
		      (multiple-value-bind (found-element i-found) 
			  (find-element (where :name element-name) elements-list)
			(unless found-element
			  (error 'no-element-for-probe-error :element-name element-name :probe-name (element-name probe)))
			(push (grid:gref state-vector i-found) state))))
		   (t
		    (error 'undefined-probe-type-error :probe-name (element-class-name probe)))))))
	   (setf (model-class-states-list element) state)
	   (when debug-mode
	     (format output "Evaluating model ~a:~%Parameters = ~a~%State = ~a~%" (element-class-name element) (model-class-parameters-list element) (model-class-states-list element)))
	   (setf element (evaluate-model element :debug-mode debug-mode :output output))))
	element)
    (no-node-for-probe-error (condition)
      (format *error-output* "No ~a node for probe ~a.~%" (node-name condition) (probe-name condition)))
    (no-element-for-probe-error (condition)
      (format *error-output* "No ~a element for probe ~a.~%" (element-name condition) (probe-name-condition)))
    (undefined-probe-type-error (condition)
      (format *error-output* "Undefined probe type (~a).~%" (probe-name condition)))))

;;;
;;; print back simulation progress bar
;;;

(defun print-progress-bar (step little-mark big-mark)
  (let ((percent (* 100 (/ step *steps-number*))))
    (cond
      ((< percent 100)
       (if (eql (mod percent big-mark) 0)
	   (format t " %~a " percent)
	   (when (eql (mod percent little-mark) 0)
	     (format t "="))))
      ((eql percent 100)
       (format t " 100% done!~%")))))

;;;
;;; select solutions to write onto file
;;;
  
(defun select-probes (netlist y-vector output-file-stream &optional &key (debug-mode nil) (output *standard-output*))
  (handler-case
      (let ((elements-list (select (list (where :class-type 'source-class)
					 (where :class-type 'passive-class)
					 (where :class-type 'coupling-class)) (netlist-class-elements-list netlist)))
	    (nodes-list (select (where :class-type 'node-class) (netlist-class-elements-list netlist)))
	    (probes-list (select (where :class-type 'probe-class) (netlist-class-elements-list netlist))))
	(unless output-file-stream
	  (error 'file-not-opened-error :file-pathname (pathname output-file-stream)))
	(when debug-mode
	  (format output "~%Writing to simulation file."))
	(format output-file-stream "~f " *time*)
	(dolist (probe probes-list)
	  (cond
	    ((voltage-probe-class-p probe)
	     (dolist (node-name (probe-class-nodes-list probe))
	       (multiple-value-bind (found-node found-node-position) 
		   (find-element (where :name node-name) nodes-list)
		 (unless found-node
		   (error 'probe-not-found-error :probe-name (element-class-name probe) :node-name node-name))
		 (incf found-node-position (- (grid:dim0 y-vector) 
					      (length nodes-list)))
		 (when debug-mode
		   (format output "~%Writing Y(~a) = ~a." found-node-position (grid:gref y-vector found-node-position)))
		 (format output-file-stream "~f " (grid:gref y-vector found-node-position)))))
	    ((current-probe-class-p probe)
	     (dolist (element-name (probe-class-elements-list probe))
	       (multiple-value-bind (found-element found-element-position) (find-element (where :name element-name) elements-list)
		 (unless found-element
		   (error 'probe-not-found-error :probe-name (element-name probe) :element-name element-name))
		 (when debug-mode
		   (format output "~%Writing Y(~a) = ~a." found-element-position (grid:gref y-vector found-element-position)))
		 (format output-file-stream "~f " (grid:gref y-vector found-element-position)))))))
	(format output-file-stream "~%"))
    (file-writing-error (condition)
      (format *error-output* "~%Could not write file ~a~%" (file-pathname condition))
      nil)
    (file-not-opened-error (condition)
      (format *error-output* "~%File not opened.~%")
      nil)
    (probe-not-found-error (condition)
      (format *error-output* "~%Probe ~a " (probe-name condition))
      (when (node-name condition)
	(format *error-output* "node ~a not found" (node-name condition)))
      (when (element-name condition)
	(format *error-output* "element ~a not found" (element-name condition)))
      (format *error-output* ".~%")
      nil)))

;;;
;;; setup output file
;;;

(defun open-simulation-file (netlist output-file-pathname)
  (handler-case
      (let ((probes-list (select (where :class-type 'probe-class) (netlist-class-elements-list netlist)))
	    (output-file-stream (open output-file-pathname :direction :output :if-exists :supersede)))
	(when output-file-stream
	  (format output-file-stream "time ")
	  (dolist (probe probes-list)
	    (cond
	      ((voltage-probe-class-p probe)
	       (dolist (node-name (probe-class-nodes-list probe))
		 (format output-file-stream "~a-~a " (element-class-name probe) node-name)))
	      ((current-probe-class-p probe)
	       (dolist (element-name (probe-class-elements-list probe))
		 (format output-file-stream "~a-~a " (element-class-name probe) element-name)))))
	  (format output-file-stream "~%~%"))
	output-file-stream)
    (file-error (condition)
      (format *error-output* "~%Error opening file.~%")
      nil)))

;;;
;;; setup initial conditions
;;;

(defun setup-initial-conditions (netlist y-vector &optional &key (debug-mode nil) (output *standard-output*))
  (handler-case
      (let ((elements-list (exclude (list (where :class-type 'initial-condition-class)
					  (where :class-type 'probe-class)) (netlist-class-elements-list netlist)))
	    (initial-conditions-list (select (where :class-type 'initial-condition-class) (netlist-class-elements-list netlist))))
	(dolist (initial-condition initial-conditions-list)
	  (when debug-mode
	    (format output "Found initial condition ~a.~%" (element-class-name initial-condition)))
	  (multiple-value-bind (element i)
	      (find-element (where :name (element-target-name initial-condition)) elements-list)
	    (unless element
	      (error 'unknown-element-for-initial-condition-error :element-name (element-target-name initial-condition) :initial-condition-name (element-class-name initial-condition)))
	    (typecase element
	      (source-class
	       (error 'initial-condition-error :initial-condition-name (element-class-name initial-condition) :source-name (element-class-name element)))
	      (passive-class
	       (when debug-mode
		 (format output "Setting y(~a) = ~a.~%" i (passive-class-value initial-condition)))
	       (setf (grid:gref y-vector i) (initial-condition-class-value initial-condition)))
	      (coupling-class
	       (error 'initial-condition-error :initial-condition-name (element-class-name initial-condition) :coupling-name (element-class-name element)))
	      (node-class
	       (when (reference-class-node-p (element-class element))
		 (error 'initial-condition-error :initial-condition-name (element-class-name initial-condition) :node-name (element-class-name element)))
	       (setf (grid:gref y-vector (1- i)) (initial-condition-class-value initial-condition)))
	      (model-class
	       (setf (model-class-value element) (initial-condition-class-value initial-condition)))
	      (t
	       (error "Initial condition ~a: ~a could not have an initial condition: only passive and models.~%" (element-class-name initial-condition) (element-class-name element))))))
	y-vector)
    (unknown-element-for-initial-condition-error (condition)
      (format *error-output* "Could not find element ~a to set initial condition ~a.~%" (element-name condition) (initial-condition-name condition))
      nil)
    (initial-condition-source-error (condition)
      (format *error-output* "~%Could not set initial condition ~a" (initial-condition-name condition))
      (when (source-name condition)
	(format *error-output* " for source ~a" (source-name condition)))
      (when (coupling-name condition)
	(format *error-output* " for coupling ~a (only its passive element can)" (coupling-name condition)))
      (when (node-name condition)
	(format *error-output* " for reference node ~a" (node-name condition)))
      (format *error-output* ".~%")
      nil)))

;;
;; solve all problems
;;
;; D(n) = (B(n) + h A(n)) ^ -1
;; Y(n) = D(n) B(n) Y(n - 1) + h D(n) K(n)
;;

(defun solve-problem (netlist-file-pathname t0 t1 steps &optional &key (debug-mode nil) (progress-bar nil) (output *standard-output*))
  (handler-case
      (progn
	(format output "~%Circuit Solver - Version ~a.~a.~a.~a~%Written by Dott. Ing. Angelo Rossi & Dott. Ing. Marco Maccioni.~%Released under GPL3 License (C) ~@r." major minor build revision year)
	(format output "~%Running on ~a machine type ~a.~%" (software-type) (machine-type))
	(unless (> t1 t0)
	  (error 'simulation-time-interval-error :t1 t1 :t0 t0))
	(setf *t0* t0)
	(setf *t1* t1)
	(if (and (< steps *minimum-steps-number*) (not debug-mode))
	    (setf *steps-number* *minimum-steps-number*)
	    (setf *steps-number* steps))
	(setf *h* (/ (- *t1* *t0*) (float *steps-number*)))
	(format output "Setting steps number to ~a.~%" *steps-number*)
	(let ((netlist (read-netlist netlist-file-pathname))
	      (error-found 0))
	  (setf netlist (include-subcircuits netlist :debug-mode debug-mode :output output))
	  (setf error-found (check-netlist netlist))
	  (when (zerop error-found)
	    (format output "~%Input file: ~a" netlist-file-pathname)
	    (format output "~%Debug Mode: ~a" debug-mode)
	    (format output "~%Loaded netlist: ~a" (element-class-name netlist))
	    (when debug-mode
	      (format output "~%~a" (sexpify netlist)))
	    (format output "~%Start at ~a s upto ~a s with Delta t = ~a s (~a steps)." *t0* *t1* *h* *steps-number*)
	    (when debug-mode
	      (format output "~%Found ~a nodes and ~a elements." 
		      (length (select (where :class-type 'node-class) (netlist-class-elements-list netlist)))
		      (length (exclude (list (where :class-type 'node-class) 
					     (where :class-type 'subcircuit-class)) (netlist-class-elements-list netlist)))))
	    (let ((p-matrix (create-p-matrix (netlist-class-elements-list netlist) :debug-mode debug-mode :output output))
		  (r-matrix (create-r-l-matrix (netlist-class-elements-list netlist) :debug-mode debug-mode :output output))
		  (g-matrix (create-g-c-matrix (netlist-class-elements-list netlist) :debug-mode debug-mode :output output))
		  (si-matrix (create-si-matrix (netlist-class-elements-list netlist) :debug-mode debug-mode :output output))
		  (sv-matrix (create-sv-matrix (netlist-class-elements-list netlist) :debug-mode debug-mode :output output))
		  (l-matrix (create-r-l-matrix (netlist-class-elements-list netlist) :debug-mode debug-mode :output output))
		  (c-matrix (create-g-c-matrix (netlist-class-elements-list netlist) :debug-mode debug-mode :output output))
		  (ki-vector (create-ki-vector (netlist-class-elements-list netlist) :debug-mode debug-mode :output output))
		  (kv-vector (create-kv-vector (netlist-class-elements-list netlist) :debug-mode debug-mode :output output))
		  (y-old-vector (create-k-y-vector (netlist-class-elements-list netlist) :debug-mode debug-mode :output output))
		  (y-new-vector (create-k-y-vector (netlist-class-elements-list netlist) :debug-mode debug-mode :output output)))
	      (setf y-old-vector (setup-initial-conditions netlist y-old-vector :debug-mode debug-mode :output output))	
	      (let* ((output-file-pathname (make-pathname :directory (pathname-directory netlist-file-pathname) :name (pathname-name netlist-file-pathname) :type "sim"))
		     (output-file-stream (open-simulation-file netlist output-file-pathname)))
		(format output "~&Output file: ~a~2%" output-file-pathname)
		(format output "~2&Solving: ")
		(loop for i from 0 to *steps-number* do
		     (setf *time* (+ t0 (* *h* (float i))))
		     (when (and debug-mode (not progress-bar))
		       (format output "~2%----~%Iteration #~a~%time = ~a~%----~2%" i *time*))       
		   ;;
		   ;; Update matrices 
		   ;;	      
		     (dolist (element (netlist-class-elements-list netlist))
		       (setf element (update-model element (netlist-class-elements-list netlist) y-old-vector :debug-mode debug-mode :output output)))
		     (setf p-matrix (update-p-matrix p-matrix (netlist-class-elements-list netlist) :debug-mode debug-mode :output output)
			   r-matrix (update-r-matrix r-matrix (netlist-class-elements-list netlist) :debug-mode debug-mode :output output)
			   g-matrix (update-g-matrix g-matrix (netlist-class-elements-list netlist) :debug-mode debug-mode :output output)
			   si-matrix (update-si-matrix si-matrix (netlist-class-elements-list netlist) :debug-mode debug-mode :output output)
			   sv-matrix (update-sv-matrix sv-matrix (netlist-class-elements-list netlist) :debug-mode debug-mode :output output)
			   l-matrix (update-l-matrix l-matrix (netlist-class-elements-list netlist) :debug-mode debug-mode :output output)
			   c-matrix (update-c-matrix c-matrix (netlist-class-elements-list netlist) :debug-mode debug-mode :output output)
			   ki-vector (update-ki-vector ki-vector (netlist-class-elements-list netlist) :debug-mode debug-mode :output output)
			   kv-vector (update-kv-vector kv-vector (netlist-class-elements-list netlist) :debug-mode debug-mode :output output))
		     (multiple-value-bind (a-matrix b-matrix k-vector) 
			 (assemble-system p-matrix r-matrix g-matrix si-matrix sv-matrix l-matrix c-matrix ki-vector kv-vector :debug-mode debug-mode :output output)
		       (let ((alpha-matrix (gsl:elt+ (gsl:elt* *h* (grid:copy-to a-matrix 'grid:foreign-array)) (grid:copy-to b-matrix 'grid:foreign-array)))
			     (beta-matrix (gsl:elt+ (gsl:elt* *h* (grid:copy-to k-vector 'grid:foreign-array)) (gsl:matrix-product (grid:copy-to b-matrix 'grid:foreign-array) (grid:copy-to y-old-vector 'grid:foreign-array)))))
			 (multiple-value-bind (decomposition-matrix permutation-matrix sign) (gsl:lu-decomposition (grid:copy-to alpha-matrix 'grid:foreign-array))
			   (when debug-mode
			     (format output "~&Permutation sign: ~a" sign))
			   (gsl:lu-solve decomposition-matrix beta-matrix permutation-matrix y-new-vector)		     
			   (when debug-mode
			     (format output "~&Y(n+1) =~%~a~%" y-new-vector)
			     (format output "~&Y(n) =~%~a~%" y-old-vector))
			   (select-probes netlist y-new-vector output-file-stream :debug-mode debug-mode :output output)
			   (setf y-old-vector y-new-vector)))
		       (when (and (eql nil debug-mode) progress-bar)
			 (print-progress-bar i 2 20))))
		(when output-file-stream
		  (close output-file-stream)))))))
    (simulation-time-interval-error (condition)
      (format *error-output* "Simulation final time (~a) less than or equal to start time (~a).~%" (:t0 condition) (:t1 condition))
      nil)))