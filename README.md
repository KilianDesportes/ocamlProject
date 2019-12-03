# OCaml mini project

DELIOT Maxence - DESPORTES Kilian

4IR - INSA Toulouse - 2019

Base project for Ocaml project on Ford-Fulkerson. This project contains some simple configuration files to facilitate editing Ocaml in VSCode.

To use, you should install the *OCaml* extension in VSCode. Other extensions might work as well but make sure there is only one installed.
Then open VSCode in the root directory of this repository.

Features :
 - full compilation as VSCode build task (Ctrl+Shift+b)
 - highlights of compilation errors as you type
 - code completion
 - automatic indentation on file save


A makefile also provides basic automation :
 - `make` to compile. This creates an ftest.native executable
 - `make format` to indent the entire project

Execution of algorithm :
 - `./ftest.native infile source sink outfile`
 the result of the algorithm will be in 'outfile'.

For example : Graph1, ford-fulkerson from node 0 to 5, written in file graph_out :
 - `./ftest.native graphs/graph1 0 5 graph_out`

We made a this algorithm work on real world problem, based on host-matching problem (https://www.anishathalye.com/2015/09/24/algorithms-in-the-real-world-host-matching/), with the infile 'graphs/host_hackers' which represents the same problem as presented on this link.


