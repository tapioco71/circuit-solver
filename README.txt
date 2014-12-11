This is the stub README.txt for the "circuit_solver" project.
=============================================================

What is "circuit-solver"?
=========================
It is a Common Lisp program that simulates complex electromechanical systems. An electromechanical system is a physical one in which 
energy conversion could occur from electrical to mechanical and viceversa. For example a synchronous machine driven by a prime mover
would convert mechanical energy into electrical in the form of a voltage difference and an electrical current.

How to test the examples provided.
==================================
All the examples are into .net files. This kind of file reppresent a netlist for an electrical circuit: at this time only electrical
circuit could be simulated. For example test-trafo-gdl-01.net is a netlist about a circuit with an ac generator feeding a monophase
transformer that feeds a gas discharge lamp. To test the program use SBCL (alas it should work on other CL too), from a slime REPL, 
You can:

CL-USER> (ql:quickload :circuit-solver)
To load "circuit-solver":
  Load 1 ASDF system:
    circuit-solver
; Loading "circuit-solver"
..................................................
....
(:CIRCUIT-SOLVER)
CL-USER> (in-package :circuit-solver)
#<PACKAGE "CIRCUIT-SOLVER">
CIRCUIT-SOLVER>

at this point You can launche a simulation by executing:

CIRCUIT-SOLVER> (solve-problem #p"/home/angel/Development/lisp/circuit-solver/examples/test-trafo-gdl-01.net" 0d0 2d0 20000 :progress-bar t)

the program will answer with:

Circuit Solver - Version 0.3.0.0
Written by Dott. Ing. Angelo Rossi & Dott. Ing. Marco Maccioni.
Released under GPL3 License (C) MMXIV.
Running on Linux machine type X86-64.
Setting steps number to 20000.

Including subcircuits. Done!
Input file: /home/angel/Development/lisp/circuit-solver/examples/test-trafo-gdl-01.net
Debug Mode: NIL
Loaded netlist: test-trafo-gdl-01
Start at 0.0 s upto 2.0 s with Delta t = 1.e-4 s (20000 steps).
Output file: /home/angel/Development/lisp/circuit-solver/examples/test-trafo-gdl-01.sim


Solving:  %0 ========= %20 ========= %40 ========= %60 ========= %80 ========= 100% done!
T
CIRCUIT-SOLVER>

it means that simulation apparently went well and You can inspect the solution, by using, for example kst (https://kst-plot.kde.org/)
that it is available on Debian too (sudo apt-get install kst) and load the file "test-trafo-gdl-01.kst". 

May I write a netlist?
======================
Sure. .net files are plain ascii text ones, You can edit using emacs or whatever You like, but keep in mind that netlist description
is based on sexps. This is an example of a netlist for an RLC circuit feeded by an AC voltage source:

(NETLIST
 :NAME "test-ac-rlc-01"
 :ELEMENTS-LIST ((SOURCE
		  :NAME "V1"
		  :CLASS "voltage-source"
		  :MODEL (MODEL :NAME "sinusoidal-voltage-1"
				:FUNCTION-NAME "sinusoidal-function"
				:CLASS "function"
				:PARAMETERS-LIST (:AMPLITUDE 4243d0 :FREQUENCY 50d0 :PHASE 0d0))	
		  :NODES-LIST ("GND" "N1"))
		
		 (PASSIVE
		  :NAME "R1"
		  :CLASS "resistance"
		  :NODES-LIST ("N1" "N2")
		  :VALUE 1d)

		 (PASSIVE
		  :NAME "L1"
		  :CLASS "inductance"
		  :NODES-LIST ("N2" "N3")
		  :VALUE 10d-6)
		 		 
		 (PASSIVE
		  :NAME "C1"
		  :CLASS "capacitance"
		  :NODES-LIST ("N3" "GND")
		  :VALUE 33d-6)
		 
		 (NODE
		  :NAME "GND"
		  :CLASS "reference")
		 
		 (NODE
		  :NAME "N1"
		  :CLASS "v-i")
		 
		 (NODE
		  :NAME "N2"
		  :CLASS "v-i")
		 
		 (NODE
		  :NAME "N3"
		  :CLASS "v-i")
		 		 
		 (PROBE
		  :NAME "current"
		  :CLASS "current-probe"
		  :ELEMENTS-LIST ("V1" "R1" "L1" "C1"))

		 (PROBE
		  :NAME "voltage"
		  :CLASS "voltage-probe"
		  :NODES-LIST ("N1" "N2" "N3"))))
