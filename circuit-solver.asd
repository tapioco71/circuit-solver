;;;; -*- mode: common-lisp; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; circuit-solver.asd
;;;;
;;;; Copyright 2020-2022 Angelo Rossi, Marco Maccioni
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;; 1. Redistributions of source code must retain the above copyright notice,
;;;;    this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;; 3. Neither the name of the copyright holder nor the names of its
;;;;    contributors may be used to endorse or promote products derived from
;;;;    this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

(asdf:defsystem #:circuit-solver
  :name "circuit-solver"
  :author "Angelo Rossi <angelo.rossi.homelab@gmail.com>, Marco Maccioni <marco.maccioni@uniroma1.it>"
  :license "BSD"
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
