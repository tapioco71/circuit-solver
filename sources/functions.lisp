;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; functions.lisp
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

;;
;; Built-in models
;;

;;;
;;; a model function
;;; parameters = (a1 a2 b0 b1 b2 g)
;;; state = (old-value current)
;;;

(defun gas-discharge-lamp (&key parameters state)
  (let ((current (pop state))
	(g0 (pop state))
	(a1 (getf parameters :a1))
	(a2 (getf parameters :a2))
	(b0 (getf parameters :b0))
	(b1 (getf parameters :b1))
	(b2 (getf parameters :b2))
	(b3 (getf parameters :b3))
	(fraG (getf parameters :fraG))
	(eqE (getf parameters :eqE)))
    (- (* (/ a2 (expt g0 fraG))
	  (expt (+ current a1) 2d0))
       (+ (- (* b2 (exp (* b3 g0)) eqE))
	  (* (+ (* b3 (expt g0 3d0)) (* b2 (expt g0 2d0))) eqE)
	  (+ (* b1 g0) b0)))))


;;
;; sinusoidal function y = A * sin(omega * t + phi)
;;

(defun sinusoidal-function (&optional &key parameters state)
  "sinusoidal-function(parameters, state) where parameters = (amplitude, frequency, phase)"
  state
  (* (getf parameters :amplitude) (sin (+ (* 2 pi (getf parameters :frequency) *time*) (getf parameters :phase)))))

;;
;; ramp function y = m * t, t >= t0
;;

(defun ramp-function (&optional &key parameters state)
  "sinusoidal-function(parameters, state) where parameters = (t0, m)"
  (let ((new-value (pop state))
	(t0 (getf parameters :t0))
	(m (getf parameters :m)))
    (if (>= *time* t0)
	(setf new-value (- (* m *time*) (* m t0)))
	0d0)
    new-value))

(defun ramp2-function (&optional &key parameters state)
  "sinusoidal-function(parameters, state) where parameters = (t0, v0, t1, v1)"
  (let ((new-value (pop state))
	(t0 (getf parameters :t0))
	(v0 (getf parameters :v0))
	(t1 (getf parameters :t1))
	(v1 (getf parameters :v1)))
    (cond
      ((< *time* t0)
       (setf new-value v0))
      ((and (>= *time* t0) (< *time* t1))
       (setf new-value (/ (+ (* *time* (- v0 v1)) (- (* t0 v1) (* t1 v0))) (- t0 t1))))
      ((>= *time* t1)
       (setf new-value v1)))
    new-value))

;;
;; factorial of an integer number: m = n!
;;

(defun factorial (n)
  "factorial of n: m = n! = n * (n - 1) * ... * 1"
  (if (< n 2)
      1
      (* n (factorial (- n 1)))))

;;
;; impulse-function: y = H <=> t0 <= t < t1, y = L <=> t < t0, t >= t1
;;

(defun impulse-function (&optional &key parameters state)
  "Generate a square pulse at time = start ending at time = stop."
  (let* ((new-voltage (pop state))
	 (stop (getf parameters :start))
	 (start (getf parameters :stop))
	 (low-value (getf parameters :low-value))
	 (high-value (getf parameters :high-value))
	 (polarity (getf parameters :polarity)))
    (unless polarity
      (setf polarity t))
    (unless start
      (setf start *t0*))
    (unless stop
      (setf stop *t1*))
    (if (and (>= *time* start) (< *time* stop))
	(if polarity
	    (setf new-voltage high-value)
	    (setf new-voltage low-value))
	(if polarity
	    (setf new-voltage low-value)
	    (setf new-voltage high-value)))
    new-voltage))

;;
;; square function
;;

(defun square-function (&optional &key parameters state)
  "Square function: frequency = 1 / period, duty cycle = duty-cycle."
  (let* ((new-voltage (pop state))
	 (period (getf parameters :period))
	 (duty-cycle (getf parameters :duty-cycle))
	 (low-value (getf parameters :low-value))
	 (high-value (getf parameters :high-value))

	 ;;
	 ;;	 (rising-time (getf parameters :rising-time))
	 ;;	 (falling-time (getf parameters :falling-time))
	 ;;

	 (t0 (getf parameters :t0))
	 (polarity (getf parameters :polarity))
	 (ton 0d0)
	 (toff 0d0)
	 (time (mod (- *time* t0) period)))
    (unless polarity
      (setf polarity t))
    (unless period
      (setf period (- *t1* *t0*)))
    (unless t0
      (setf t0 *t0*))
    (setf ton (* duty-cycle period))
    (setf toff (- 1 duty-cycle))
    (cond
      ((and (>= time 0d0) (< time ton))
       (if polarity
	   (setf new-voltage high-value)
	   (setf new-voltage low-value)))
      ((and (>= time ton) (<= time period))
       (if polarity
	   (setf new-voltage low-value)
	   (setf new-voltage high-value))))
    new-voltage))

(defun square-function-2 (&optional &key parameters state)
  (let* ((new-voltage (pop state))
	 (period (getf parameters :period))
	 (duty-cycle (getf parameters :duty-cycle))
	 (low-value (getf parameters :low-value))
	 (high-value (getf parameters :high-value))
	 (t0 (getf parameters :t0))
	 (polarity (getf parameters :polarity))
	 (ton 0d0)
	 (toff 0d0)
	 (time (mod (- *time* t0) period)))
    (unless period
      (setf period (- *t1* *t0*)))
    (unless t0
      (setf t0 *t0*))
    (setf ton (* duty-cycle period))
    (setf toff (- 1 duty-cycle))
    (cond
      ((not (eql (and (>= time 0d0) (< time ton)) polarity))
       (setf new-voltage high-value))
      ((not (eql (and (>= time ton) (<= time period)) polarity))
       (setf new-voltage low-value)))
    new-voltage))

;;
;; bistable function
;;

(defun bistable-function-1 (&optional &key parameters state)
  (let* ((new-voltage (pop state))
	 (low-value (getf parameters :low-value))
	 (high-value (getf parameters :high-value))
	 (t0 (getf parameters :t0))
	 (polarity (getf parameters :polarity)))
    (unless t0
      (setf t0 *t0*))
    (cond
      ((not (eql (>= *time* t0) polarity))
       (setf new-voltage high-value))
      ((not (eql (< *time* t0) polarity))
       (setf new-voltage low-value)))
    new-voltage))

;;
;; a simple diode from Sedra-Smith book (pag. 132)
;;

(defun simple-diode-4 (&optional &key parameters state)
  (let* ((v2 (pop state))
	 (v1 (pop state))
	 (Is (getf parameters :is))
	 (Vt (getf parameters :vt))
	 (n (getf parameters :n))
	 (order (getf parameters :order))
	 (g (/ Is (* n Vt))))
    (unless order
      (setf order 2))
    (when (> (- v1 v2) 0d0)
      (loop for i from 1 to order do
	   (setf g (+ (/ (expt (- v1 v2) i) (* (expt (* n Vt) (+ i 1)) (factorial (1+ i))))))))
    g))

(defun simple-diode-3 (&optional &key parameters state)
  (let* ((v2 (pop state))
	 (v1 (pop state))
	 (Is (getf parameters :is))
	 (Vt (getf parameters :vt))
	 (n (getf parameters :n))
	 (Vb (getf parameters :vb))
	 (g0 (/ Is (* n Vt))))
    (cond
      ((and
	(<= (- v1 v2) 0d0)
	(>= (- v1 v2) Vb))
       g0)
      ((< (- v1 v2) Vb)
       (- (* g0 (+ 1 (/ (- v1 v2) (* 2 n Vt)) (/ (expt (/ (- v1 v2) (* n Vt)) 2d0) 6d0)))))
      ((> (- v1 v2) 0d0)
       (* g0 (+ 1 (/ (- v1 v2) (* 2 n Vt)) (/ (expt (/ (- v1 v2) (* n Vt)) 2d0) 6d0)))))))

(defun simple-diode-2 (&optional &key parameters state)
  (let* ((v2 (pop state))
	 (v1 (pop state))
	 (Is (getf parameters :is))
	 (Vt (getf parameters :vt))
	 (n (getf parameters :n))
	 (g0 (/ Is (* n Vt))))
    (if (<= (- v1 v2) 0d0)
	g0
	(* g0 (+
	       1
	       (/ (- v1 v2) (* 2 n Vt))
	       (/ (expt (/ (- v1 v2) (* n Vt)) 2d0) 6d0)
	       (/ (expt (/ (- v1 v2) (* n Vt)) 3d0) 24d0)
	       (/ (expt (/ (- v1 v2) (* n Vt)) 4d0) 120d0))))))

(defun simple-diode-1 (&optional &key parameters state)
  (let ((v1 (pop state))
	(v2 (pop state))
	(Is (getf parameters :is))
	(Vt (getf parameters :vt))
	(n (getf parameters :n)))
    (* Is (- (exp (/ (- v1 v2) (* n Vt))) 1))))

(defun simple-diode (&optional &key parameters state)
  (let ((v1 (pop state))
	(v2 (pop state))
	(Is (getf parameters :is))
	(Vt (getf parameters :vt))
	(n (getf parameters :n)))
    (if (<= (abs (- v1 v2)) 1d-9)
	(/ Is (* n Vt))
	(/ (- v1 v2) (* Is (- (exp (/ (- v1 v2) (* n Vt))) 1))))))

;;
;; simple switch
;;

(defun simple-switch-1 (&optional &key parameters state)
  (let ((v1 (pop state))
	(v2 (pop state))
	(old-resistance-value (pop state))
	(upper-threshold (getf parameters :upper-threshold))
	(lower-threshold (getf parameters :lower-threshold))
	(on-resistance (getf parameters :on-resistance))
	(off-resistance (getf parameters :off-resistance))
	(return-value 0d0))
    (cond
      ((>= (- v1 v2) upper-threshold)
       (setf return-value on-resistance))
      ((<= (- v1 v2) lower-threshold)
       (setf return-value off-resistance))
      (t
       (setf return-value old-resistance-value)))
    return-value))

(defun simple-switch-2 (&optional &key parameters state)
  (let* ((v1 (pop state))
	 (v2 (pop state))
	 (old-conductance-value (pop state))
	 (upper-threshold (getf parameters :upper-threshold))
	 (lower-threshold (getf parameters :lower-threshold))
	 (on-conductance (getf parameters :on-conductance))
	 (off-conductance (getf parameters :off-conductance))
	 (on-time (getf parameters :on-time))
	 (off-time (getf parameters :off-time))
	 (return-value old-conductance-value)
	 (delta-v (- v1 v2)))
    (when (< on-time *h*)
      (setf on-time *h*))
    (when (< off-time *h*)
      (setf off-time *h*))
    (cond
      ((> delta-v upper-threshold)
       (if (> return-value on-conductance)
	   (setf return-value on-conductance)
	   (incf return-value (* *h* (/ (- on-conductance off-conductance) on-time)))))
      ((< delta-v lower-threshold)
       (if (< return-value off-conductance)
	   (setf return-value off-conductance)
	   (decf return-value (* *h* (/ (- on-conductance off-conductance) off-time)))))
      (t
       (setf return-value old-conductance-value)))
    (when (> return-value on-conductance)
      (setf return-value on-conductance))
    (when (< return-value off-conductance)
      (setf return-value off-conductance))
    return-value))

;; Ebers-Moll transistor model.

(defun ebers-moll (&optional &key parameters state)
  "Ebers-Moll transistor model."
  (let* ((vc (pop state))
	 (vb (pop state))
	 (ve (pop state))
	 (is (pop state))
	 (vt (pop state))
	 (vbe (- vb ve))
	 (vbc (- vb vc)))
    (* is (- (exp (/ vbe vt)) (exp (/ vbc vt))))))

;; Simple MOV model.

(defun simple-mov (&optional &key parameters state)
  "Simple MOV model."
  (let* ((va (pop state))
         (vb (pop state))
         (vmax (getf parameters :vmax))
         (gmax (getf parameters :gmax))
         (gmin (getf parameters :gmin)))
    (if (< (abs (- va vb))
           (abs vmax))
        (* gmin
           (- va vb))
        (* gmax
           (signum (- va vb))
           (abs vmax)))))
