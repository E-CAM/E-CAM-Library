.. _auxmod:

######
AuxMod 
######

.. sidebar:: Software Technical Information

  Language
    Fortran 95/90

  Licence
    MIT license (MIT)

  Documentation Tool
    Doxygen

  Software Module Developed by
    Ari P. Seitsonen, Momir Mališ

.. contents:: :local:


Purpose of Module
_________________

Module **AuxMod** contains a set of subroutines which can be used for an easier construction of any program 
input file reader, 
and a library of common MPI commands adapted for easier implementation when programming a Fortran MPI parallel code. 
The module consists of an input parser designed to read any formatted file 
with the possibility to find a specific set of user pre-defined keywords 
and examine whether the read in variable types are consistent with the code requirements. 
The library of parallel subroutines contains a number of MPI commands for communicating information between all or 
a pair of processor cores, and are adapted for easier user implementation into his/her own code. 
The provided subroutines/libraries can also be considered as a Fortran template which the user can adapt or 
update depending on his/her specific requirements. 


Applications of the Module
__________________________

The AuxMod module was used to construct the input parser for the PaPIM code, while its modified MPI commands enable to 
parallelize the PaPIM code and the ClassMC module. 
Based on these example, the AuxMod provides a pre-constructed 
input reader and adapted MPI library for any future Fortran code development.


Compiling
_________

The code should be compiled in the ./source sub-directory using a Fortran compiler with a MPI wrapper. 
A Makefile is present for an automatic compilation. 
Execute command ``make`` in the ``./source`` sub-directory to generate the ``AuxModRun.exe`` executable:

::

	cd source

	make


Testing
_______

For AuxMod test purposes the ``numdiff`` package is used for automatic comparison purposes and should be made
available before running the tests, otherwise the ``diff`` command will be used automatically instead but the user
is warned that the test might fail due to numerical differences.
The user is advised to download and install ``numdiff`` from `here <http://www.nongnu.org/numdiff/>`_.
The module is accompanied with an example input file ``TESTINPUT`` located in the ``tests`` sub-directory 
together with the reference output in sub-directory ``REFERENCE_OUTPUT``:

::

	cd tests

	../source/AuxModRun.exe < TESTINPUT

The user is also advised to test the code manually by changing the values in the ``TESTINPUT`` input file. 


Source Code
___________

The source code is given at https://gitlab.e-cam2020.eu/Quantum-Dynamics/PIM/tree/AuxMod. 
The file ``parser.F90`` contains all the subroutines for the within-one-line data type recognition, 
while the ``auxmod.F90`` contains the direct subroutines for input file reading, 
and can be considered as a template for further modification. 
The ``prl.F90`` contains the adapted MPI commands. 


Source Code Documentation
_________________________

The source code documentation is given at https://gitlab.e-cam2020.eu:10443/Quantum-Dynamics/PIM/tree/AuxMod/doc.
The documentation files (html and latex format) are obtained by executing the ``make`` command in the ``./doc`` sub-directory:

::

	cd doc

	make


