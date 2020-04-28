;;;; circuit-solver.asd

(asdf:defsystem #:circuit-solver
  :name "circuit-solver"
  :author "Angelo Rossi <angelo.rossi.homelab@gmail.com>, Marco Maccioni <marco.maccioni@uniroma1.it>"
  :license "GPL v3.0"
  :version "0.2.6"
  :description "Electrical circuits solver using gsll."
  :serial t
  :depends-on (#:gsll)
  :components ((:file "sources/package")
	       (:file "sources/functions")
               (:file "sources/circuit-solver")))
