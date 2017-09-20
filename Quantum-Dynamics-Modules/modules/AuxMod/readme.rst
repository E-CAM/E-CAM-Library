.. _auxmod:

####################
AuxMod module
####################

.. sidebar:: Software Technical Information

  Language
    Fortran 90

  Licence
    MIT license (MIT)

  Documentation Tool
    Doxygen

.. contents:: :local:


Purpose of Module
_________________

Module **AuxMod** contains a set of subroutines which can be used for an easier construction of any program 
input file reader subroutines potentially encountered when building a new Fortran code, 
and a library of common MPI commands adapted for easier implementation when programming a Fortran MPI parallel code. 
The module consists of an input parser design to read any formatted file 
with the possibility to find a specific set of user pre-defined keywords 
and examine whether the read in variable types are consistent with the required before passing them further. 
The library of parallel subroutines contains a number of MPI commands for communicating information between all or
a pair of processor cores, and are adapted in a way for easier user implementation into his/her own code. 
The provided subroutines/libraries can also be considered as a Fortran template which the user can adapt or
update depending on his/her specific requirements.


Compiling
_________

The code should be compiled in the ./source sub-directory using a Fortran compiler with a MPI wrapper. 
A Makefile is present for an automatic compilation. 
Execute command ``make`` in the ``./source`` sub-directory to generate the ``AuxModRun.exe`` executable.
For AuxMod test purposes the numdiff package should be made available before running the tests. 
In case numdiff is not available on the system the diff command will be automatically used instead.
The user is advised to download and install numdiff from http://www.nongnu.org/numdiff/ to ensure that numerical
differences are ignored in the module tests.


Testing
_______

The module is accompanied with an example input file ``TESTINPUT`` located in the ``tests`` sub-directory
together with the reference output in sub-directory ``REFERENCE_OUTPUT``.
Before running the test the numdiff package used for comparison purposes should be made available, 
otherwise the diff command will be used instead but the user is warned that the test might fail 
due to numerical differences. 
The user is also advised to test the code manually by changing the values in the ``TESTINPUT`` input file.


Source Code
___________

The source code is given at https://gitlab.e-cam2020.eu/Quantum-Dynamics/PIM/tree/AuxMod/source .
The file ``parser.F90`` contains all the subroutines for the whitin-one-line data type recognition,
while the ``auxmod.F90`` contains the direct subroutines for input file reading,
and can be considered as a template for further modification. 
The ``prl.F90`` contains the adapted MPI commands.


Source Code Documentation
_________________________

The source code documentation is given at https://gitlab.e-cam2020.eu:10443/Quantum-Dynamics/PIM/tree/AuxMod/doc .
The documentation files (html and latex format) are obtained by executing the ``make`` command in the ./doc sub-directory.



