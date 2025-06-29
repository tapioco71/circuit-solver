;;;; -*- mode: lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; functions.lisp
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

;; Functions.

(defun factorial (n)
  "Factorial of n: m = n! = n * (n - 1) * ... * 1"
  (if (< n 2)
      1
      (* n (factorial (- n 1)))))

(defun sigmoid (x &rest parameters &key
                                     (amplitude 1d0)
                                     (scale 1d0)
                                     (x0 0d0)
                                     (y0 -1/2))
  "Sigmoid function see https://en.wikipedia.org/wiki/Sigmoid_function"
  (declare (ignorable parameters amplitude scale x0 y0))
  (+ (* amplitude
        (/ 1d0
           (+ 1d0 (exp (- (* scale (- x x0)))))))
     y0))

;; Built-in models.
;;; a model function
;;; parameters = (a1 a2 b0 b1 b2 g)
;;; state = (old-value current)

(defun gas-discharge-lamp (&rest rest &key parameters state)
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (a1 0d0) (a2 0d0) (b0 0d0) (b1 0d0) (b2 0d0) (b3 0d0) (fraG 0d0) (eqE 0d0))
      parameters
    (declare (ignorable a1 a2 b0 b1 b2 b3 fraG eqE))
    (let ((current (pop state))
	  (g0 (pop state)))
      (- (* (/ a2 (expt g0 fraG))
	    (expt (+ current a1) 2d0))
         (+ (- (* b2 (exp (* b3 g0)) eqE))
	    (* (+ (* b3 (expt g0 3d0)) (* b2 (expt g0 2d0))) eqE)
	    (+ (* b1 g0) b0))))))

;;
;; sinusoidal function y = A * sin(omega * t + phi)
;;

(defun sinusoidal-function (&rest rest &key parameters state)
  "sinusoidal-function(parameters, state) where parameters = (amplitude, frequency, phase, offset, time start)"
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (amplitude 1d0) (frequency 50d0) (phase 0d0) (offset 0d0) (time-start 0d0))
      parameters
    (declare (ignorable amplitude frequency phase offset time-start))
    (let ((omega (* 2d0 pi frequency)))
      (if (>= *time* time-start)
          (+ offset
             (* amplitude
                (sin (+ (* omega *time*)
                        phase))))
          0d0))))

(defun harmonic-function (&rest rest &key parameters state)
  "harmonic-function(parameters, state) where parameters = list of (amplitude, frequency, phase, time start)."
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (harmonics '(:amplitude 1d0 :frequency 50.0d0 :phase 0d0)) (offset 0d0) (time-start 0d0))
      parameters
    (declare (ignorable harmonics offset time-start))
    (if (>= *time* time-start)
        (loop
          with value = 0.0
          initially (setq value offset)
          finally (return value)
          for harmonic in harmonics
          do
             (destructuring-bind (&key (amplitude 1d0) (frequency 50d0) (phase 0d0))
                 harmonic
               (declare (ignorable amplitude frequency phase))
               (incf value (* amplitude
                              (sin (+ (* 2d0 pi frequency (- *time* time-start))
                                      phase))))))
        0d0)))

;;
;; ramp function y = m * t, t >= t0
;;

(defun ramp-function (&rest rest &key parameters state)
  "Ramp function (parameters, state) where parameters = (t0, m)"
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (m 1d0) (time-start 0d0))
      parameters
    (declare (ignorable m time-start))
    (if (>= *time* time-start)
	(- (* m *time*) (* m time-start))
	0d0)))

(defun ramp-function-2 (&rest rest &key parameters state)
  "Ramp function (parameters, state) where parameters = (t0, v0, t1, v1)"
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (value-start 0d0) (value-end 1d0) (time-start 0d0) (time-end 1d-3))
      parameters
    (declare (ignorable value-start value-end time-start time-end))
    (cond
      ((< *time* time-start)
       value-start)
      ((and (>= *time* time-start)
            (< *time* time-end))
       (/ (+ (* *time*
                (- value-start value-end))
             (- (* time-start value-end)
                (* time-end value-start)))
          (- time-start time-end)))
      ((>= *time* time-end)
       value-end))))

;;
;; impulse-function: y = H <=> t0 <= t < t1, y = L <=> t < t0, t >= t1
;;

(defun impulse-function (&rest rest &key parameters state)
  "Generate a square pulse at time = start ending at time = stop."
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (low-value 0d0) (high-value 1d0) (polarity t) (time-start 0d0) (time-end 1d-3))
      parameters
    (declare (ignorable low-value high-value polarity time-start time-end))
    (if (and (>= *time* time-start)
             (< *time* time-end))
	(if polarity
	    high-value
	    low-value)
	(if polarity
	    low-value
	    high-value))))

;;
;; square function
;;

(defun square-function (&rest rest &key parameters state)
  "Square function: frequency = 1 / period, duty cycle = duty-cycle."
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key
                         (period 1d-3)
                         (duty-cycle 0.5d0)
                         (rising-time 1d-6)
                         (falling-time 1d-6)
                         (low-value 0d0)
                         (high-value 1d0)
                         (time-start 0d0)
                         (polarity t))
      parameters
    (declare (ignorable period duty-cycle low-value high-value))
    (let ((time-on 0d0)
	  (time-off 0d0)
	  (time (mod (- *time* time-start) period)))
      (setq time-on (* duty-cycle period)
            time-off (- 1 duty-cycle))
      (cond
        ((and (>= time 0d0) (< time time-on))
         (if polarity
	     high-value
	     low-value))
        ((and (>= time time-on)
              (<= time period))
         (if polarity
	     low-value
	     high-value))))))

(defun square-function-2 (&rest rest &key parameters state)
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key
                         (period 1d-3)
                         (duty-cycle 0.5d0)
                         (low-value 0d0)
                         (high-value 1d0)
                         (time-start 0d0)
                         (polarity t))
  (let ((time-on 0d0)
	(time-off 0d0)
	(time (mod (- *time* time-start) period)))
    (setq time-on (* duty-cycle period)
          time-off (- 1 duty-cycle))
    (cond
      ((not (eql (and (>= time 0d0) (< time time-on)) polarity))
       high-value)
      ((not (eql (and (>= time time-on) (<= time period)) polarity))
       low-value)))))

;; bistable function

(defun bistable-function-1 (&rest rest &key parameters state)
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (low-value 0d0) (high-value 1d0) (polarity t) (time-start 0d0))
      parameters
    (declare (ignorable low-value high-value polarity time-start))
    (cond
      ((not (eql (>= *time* time-start) polarity))
       high-value)
      ((not (eql (< *time* time-start) polarity))
       low-value))))

(defun simple-diode (&rest rest &key parameters state)
  "A simple diode from Sedra-Smith book (pag. 132)."
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (is 1d-3) (vt 252d-3) (n 1d0))
      parameters
    (declare (ignorable is vt n))
    (let ((v2 (pop state))
	  (v1 (pop state)))
      (when (and v1 v2)
        (if (<= (abs (- v1 v2)) 1d-9)
	    (abs (/ is (* n vt)))
	    (abs (/ (- v1 v2) (* is (- (exp (/ (- v1 v2) (* n vt))) 1)))))))))

(defun simple-diode-1 (&rest rest &key parameters state)
  "Simple diode model."
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (is 1d-3) (vt 252d-3) (n 1d0))
      parameters
    (declare (ignorable is vt n))
    (let ((v2 (pop state))
	  (v1 (pop state)))
      (when (and v1 v2)
        (* is (- (exp (/ (- v1 v2) (* n vt))) 1))))))

(defun simple-diode-2 (&rest rest &key parameters state)
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (is 1d-3) (vt 252d-3) (n 1d0))
      parameters
    (declare (ignorable is vt n))
    (let ((v2 (pop state))
          (v1 (pop state))
          (g0 (/ is (* n vt))))
      (when (and v2 v1)
        (if (<= (- v1 v2) 0d0)
	    g0
            (* g0
               (+ 1
	          (abs (/ (- v1 v2) (* 2 n vt)))
	          (abs (/ (expt (/ (- v1 v2) (* n vt)) 2d0) 6d0))
	          (abs (/ (expt (/ (- v1 v2) (* n vt)) 3d0) 24d0))
	          (abs (/ (expt (/ (- v1 v2) (* n vt)) 4d0) 120d0)))))))))

(defun simple-diode-3 (&rest rest &key parameters state)
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (is 1d-3) (vt 252d-3) (n 1d0) (vb 1d2))
      parameters
    (declare (ignorable is vt n vb))
    (let ((v2 (pop state))
	  (v1 (pop state))
	  (g0 (/ is (* n vt))))
      (when (and v1 v2)
        (cond
          ((and (<= (- v1 v2) 0d0)
	        (>= (- v1 v2) vb))
           g0)
          ((< (- v1 v2) vb)
           (abs (- (* g0 (+ 1
                            (/ (- v1 v2)
                               (* 2 n vt))
                            (/ (expt (/ (- v1 v2)
                                    (* n vt))
                                     2d0)
                               6d0))))))
          ((> (- v1 v2) 0d0)
           (abs (* g0
                   (+ 1
                      (/ (- v1 v2)
                         (* 2 n vt))
                      (/ (expt (/ (- v1 v2) (* n vt)) 2d0)
                         6d0))))))))))

(defun simple-diode-4 (&rest rest &key parameters state)
  "A diode modelled with sigmoids."
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key
                         (maximum-forward-conductance 1d0)
                         (maximum-backward-conductance 1d0)
                         (minimum-conductance 0d0)
                         (nf 1d0)
                         (nz 1d0)
                         (forward-voltage 0.7d0)
                         (breakdown-voltage 5.6d0))
      parameters
    (declare (ignorable maximum-forward-conductance
                        maximum-backward-conductance
                        minimum-conductance
                        nf
                        nz
                        forward-voltage
                        breakdown-voltage))
    (let ((v2 (pop state))
	  (v1 (pop state))
          (vak nil))
      (when (and v1 v2)
        (setq vak (- v1 v2))
        (cond
          ((< vak 0d0)
           (+ (sigmoid vak
                       :amplitude (- (abs maximum-forward-conductance)
                                     (abs minimum-conductance))
                       :scale (abs nf)
                       :x0 (abs forward-voltage)
                       :y0 0d0)
              (abs minimum-conductance)))
          (t
           (+ (sigmoid (- vak)
                       :amplitude (- (abs maximum-backward-conductance)
                                     (abs minimum-conductance))
                       :scale (abs nz)
                       :x0 (- (abs breakdown-voltage))
                       :y0 0d0)
              (abs minimum-conductance))))))))

(defun simple-diode-5 (&rest rest &key parameters state)
  "A diode model."
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key
                         (maximum-forward-conductance 1d0)
                         (maximum-backward-conductance 1d0)
                         (minimum-conductance 0d0)
                         (forward-voltage 0.7d0)
                         (breakdown-voltage 5.6d0))
      parameters
    (declare (ignorable maximum-forward-conductance
                        maximum-backward-conductance
                        minimum-conductance
                        forward-voltage
                        breakdown-voltage))
    (let ((v2 (pop state))
	  (v1 (pop state))
          (vak nil))
      (when (and v1 v2)
        (setq vak (- v1 v2))
        (if (and (> vak 0d0)
                 (> (abs vak) forward-voltage))
            maximum-forward-conductance
            (if (and (< vak 0d0)
                     (> (abs vak) maximum-backward-conductance))
                maximum-backward-conductance
                minimum-conductance))))))

(defun simple-switch-1 (&rest rest &key parameters state)
  "Simple switch."
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (lower-threshold 0d0) (upper-threshold 1d0) (on-resistance 1d-6) (off-resistance 1d6))
      parameters
    (declare (ignorable lower-threshold upper-threshold on-resistance off-resistance))
    (let ((v1 (pop state))
	  (v2 (pop state))
	  (old-resistance-value (pop state)))
      (cond
        ((>= (- v1 v2) upper-threshold)
         on-resistance)
        ((<= (- v1 v2) lower-threshold)
         off-resistance)
        (t
         old-resistance-value)))))

(defun simple-switch-2 (&rest rest &key parameters state)
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key
                         (on-time 0d0)
                         (off-time 1d0)
                         (on-conductance 1d6)
                         (off-conductance 1d-6)
                         (k-on 1d0)
                         (k-off 1d0))
      parameters
    (declare (ignorable on-time
                        off-time
                        on-conductance
                        off-conductance
                        k-on
                        k-off))
    (- (+ (sigmoid *time*
                   :amplitude (- on-conductance off-conductance)
                   :scale (abs k-on)
                   :x0 on-time
                   :y0 0d0)
          (sigmoid (- *time*)
                   :amplitude (- on-conductance off-conductance)
                   :scale (abs k-off)
                   :x0 (- off-time)
                   :y0 0d0))
       on-conductance)))

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

(defun simple-mov-1 (&rest rest &key parameters state)
  "Simple MOV model."
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (vmax 250d0) (gmin 1d-6) (gmax 1d6))
      parameters
    (declare (ignorable vmax gmin gmax))
    (let ((va (pop state))
          (vb (pop state)))
      (if (<= (abs (- va vb))
              (abs vmax))
          gmin
          gmax))))

(defun simple-mov-2 (&rest rest &key parameters state)
  "MOV model using sigmoid."
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key
                         (k 1d0)
                         (alpha 0.1d0))
      parameters
    (declare (ignorable k
                        alpha))
    (let ((v1 (pop state))
          (v2 (pop state))
          (delta-v nil))
      (when (and (numberp v1)
                 (numberp v2))
        (setq delta-v (- v1 v2))
        (* (expt k alpha)
           (expt (abs delta-v) (- alpha 1d0)))))))

(defun lightning-pulse-DEXP (&rest rest &key parameters state)
  "Double exponential (DEXP) lightning model (T.R. McComb, J.E. Lagnese)."
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (k 1d3) (a 1d-3) (b 1d-3) (time-start 0d0))
      parameters
    (declare (ignorable k a b time-start))
    (if (>= *time* time-start)
        (* k
           (- (exp (- (* a (- *time* time-start))))
              (exp (- (* b (- *time* time-start))))))
        0d0)))

(defun lightning-pulse-NTEXP (&rest rest &key parameters state)
  "Negative triple exponential (NTEXP) lightning model (T.R. McComb, J.E. Lagnese)."
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (k 1d3) (a 1d-3) (b 1d-3) (c 1d-3) (time-start 0d0))
      parameters
    (declare (ignorable k a b c time-start))
    (if (>= *time* time-start)
        (* k
           (- 1d0 (expt (* c *time*)))
           (- (expt (- (* a *time*)))
              (expt (- (* b *time*)))))
        0d0)))

(defun lightning-pulse-GEXP (&rest rest &key parameters state)
  "Gaussian (GTEXP) lightning model (T.R. McComb, J.E. Lagnese)."
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (k 1d3) (a 1d-3) (b 1d-3) (c 1d-3) (time-start 0d0))
      parameters
    (declare (ignorable k a b c time-start))
    (if (>= *time* time-start)
        (* k
           (- 1d0 (exp (- (* c (expt *time* 2d0)))))
           (- (exp (- (* a *time*)))
              (exp (- (* b *time*)))))
        0d0)))

(defun lightning-pulse-PTEXP (&rest rest &key parameters state)
  "Positive triple exponential (PTEXP) lightning model (T.R. McComb, J.E. Lagnese)."
  (declare (ignorable rest parameters state))
  (destructuring-bind (&key (k 1d3) (a 1d-3) (b 1d-3) (c 1d-3) (time-start 0d0))
      parameters
    (declare (ignorable k a b c time-start))
    (if (>= *time* time-start)
        (* k
           (+ 1d0 (expt (* c *time*)))
           (- (expt (- (* a *time*)))
              (expt (- (* b *time*)))))
        0d0)))

;;;; end of functions.lisp file.
