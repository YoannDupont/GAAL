GAAL
====

Generic Ada Automata Library

This project can (obviously) be used as a library for automata, but it also has a "default" application using a text-based user interface (tui).


Compilation of default application
====

To compile, simply open a terminal in the project's root directory (containing gaal.gpr) and type the following command:

gnat make -P gaal.gpr -Xmode=release

This will compile the project with full optimisation scheme (-O3). Default mode is debug, which add the debug flag and the "no optimisation" (-O0) flag.


Default application
====

The default application is a tui whose that gives users the possibility to play around with automata.

The first step is to give a file giving explicit encoding of the alphabet that the user wills to use. An example of such file is given in the directory example.

After that the user will be given a set of commands that he can enter, along with their respective arities and a brief description. The program will loop endlessly until the quit command (--quit) is given or that it is terminated through any other way.