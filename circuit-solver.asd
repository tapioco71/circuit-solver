;;;; -*- mode: lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; circuit-solver.asd
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

(asdf:defsystem #:circuit-solver
  :name "circuit-solver"
  :author "Angelo Rossi <angelo.rossi.homelab@gmail.com>, Marco Maccioni <marco.maccioni@uniroma1.it>"
  :license "GPL v3.0"
  :version "0.2.6"
  :description "Electrical circuits solver using gsll."
  :serial t
  :depends-on (#:lparallel #:gsll)
  :components ((:file "sources/package")
               (:file "sources/conditions")
	       (:file "sources/functions")
               (:file "sources/element")
               (:file "sources/netlist")
               (:file "sources/node")
               (:file "sources/passive")
               (:file "sources/coupling")
               (:file "sources/sources")
               (:file "sources/subcircuit")
               (:file "sources/model")
               (:file "sources/probe")
               (:file "sources/initial-condition")
               (:file "sources/problem")
               (:file "sources/spline")
               (:file "sources/utilities")
               (:file "sources/circuit-solver")))
