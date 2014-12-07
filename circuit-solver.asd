;;;; circuit_solver.asd

(asdf:defsystem #:circuit-solver
  :name "circuit-solver"
  :author "Angelo Rossi <angelo.rossi.homelab@gmail.com>, Marco Maccioni <marco.maccioni@uniroma1.it>"
  :version "0.2.6"
  :description "Electrical circuits solver using gsll."
  :serial t
  :depends-on (#:gsll)
  :components ((:file "package")
	       (:file "functions")
               (:file "circuit-solver")))
