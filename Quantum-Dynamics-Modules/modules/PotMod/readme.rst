.. _PotMod:

######
PotMod
######

.. sidebar:: Software Technical Information

  Language
    Fortran 95/90

  Licence
    MIT license (MIT)

  Documentation Tool
    Doxygen

  Software Module Developed by
    Momir Mališ

.. contents:: :local:


Purpose of Module
_________________

Module **PotMod** is a library of potential energy subroutines and interfaces to external potential energy calculation codes. 
It provides potential energies and corresponding gradients for included potentials or calls an external code to compute 
the required quantities. 
Currently, two subroutines are implemented within this module. 
A subroutine for the calculation of harmonic and Morse potential energies which requires a set of input parameters 
provided as an external file, and a subroutine containing the analytic ground state electronic energy for the 
:math:`\text{CH}_{5}^{+}` system. [Jin]_

.. The PotMod module also contains the interface to the external `CP2K <https://www.cp2k.org/>`_ code for ab-initio energy and gradient calculations. 
.. The user is required to prepare his/her own corresponding input files for the CP2K code. 


Applications of the Module
__________________________

This module is extensively used by the :ref:`PaPIM` code and :ref:`PIM_wd` and :ref:`ClassMC` modules for providing the necessary 
potentials and gradients of studied systems. 

.. Recently, apart from the aforementioned subroutines, a potential energy subroutine for small water molecule 
.. clusters has been implemented into the module necessary for the calculation of corresponding infrared spectrum. 
.. [Wang1]_ [Wang2]_

.. In addition the interface to the CP2K code has been used for the studies on protonated water dimers. (Reference required!)


Compiling
_________

The code should be compiled in the ``./source`` sub-directory using a Fortran compiler.
A ``Makefile`` is present for an automatic compilation.
Execute command 'make' in the ``./source`` sub-directory to generate the ``PotModRun.exe`` executable:

::

	cd source

	make


Testing
_______

For PotMod test purposes the ``numdiff`` package is used for automatic comparison purposes and should be made
available before running the tests, otherwise the ``diff`` command will be used automatically instead but the user
is warned that the test might fail due to numerical differences.
The user is advised to download and install ``numdiff`` from `here <http://www.nongnu.org/numdiff/>`_.
The module is accompanied by a corresponding Fortran 90 test subroutine and a reference output. 
The reference output is located in sub-directory ``./tests/REFERENCE_VALUE``. 
The test can be executed automatically by running the script ``./test.sh``:

::

	cd tests

	./test.sh

or manually by executing the compiled PotModRun.exe code within the sub-directory ``./tests`` 
(example ``../source/PotModRun.exe > out``) 
and comparing the output with the reference values in file ``REFERENCE_VALUE/tested_potentials``. 


Source Code
___________

The source code is given at https://gitlab.e-cam2020.eu/Quantum-Dynamics/PIM/tree/PotMod. 
File ``harmonic_potential.f90`` contains the subroutines for harmonic and Morse potential energy calculations, while 
file ``ch5_pes.f90`` contains the subroutines for calculation of CH\ :sub:`5`:sup:`+` \ potential energy. 
File ``PotMod.f90`` controls and calls the included subroutines (``harmonic_potential.f90`` and ``ch5_pes.f90``). 
The remaining subroutines (``GlobType.f90``, ``kinds.f90``, ``ReadFiles.f90``, ``PotModRun.f90``) are subroutines for 
test purposes, where ``GlobType.f90`` contains the definition of derived types used by PotMod module. 


Source Code Documentation
_________________________

The source code documentation is given at https://gitlab.e-cam2020.eu/Quantum-Dynamics/PIM/tree/PotMod/doc/. 
The documentation files (html and latex format) are obtained by executing the ``make`` command in the ``./doc`` 
sub-directory:

::

	cd doc

	make


References
__________

.. [Jin] Z. Jin, B. Braams, J. Bowman *J. Phys. Chem. A* **110** (2006) 1569 `DOI: 10.1021/jp053848o
         <https://pubs.acs.org/doi/abs/10.1021/jp053848o>`_

