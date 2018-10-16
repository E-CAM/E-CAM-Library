.. _PIM_qcf:

#######
PIM_qcf
#######

.. sidebar:: Software Technical Information

  Language
    Fortran 90/95

  Licence
    MIT license (MIT)

  Documentation Tool
    Doxygen

  Software Module Developed by
    Momir Mališ

.. contents:: :local:


Purpose of Module
_________________

Module **PIM_qcf** is a library of quantum cross- and auto-correlation functions used for computation of quantum time-dependent correlation functions 
within the Phase Integration Method (PIM). 
Two auto-correlation functions are currently implemented, the quantum position-position point charge dipole moment correlation function, and 
the velocity-velocity point charge dipole moment correlation function, all in the Kubo representation of the correlation functions. 
The user can follow the two examples to construct his/her own quantum correlation function.


Applications of the module
__________________________

This module has been used in the calculation of :math:`\text{CH}_{5}^{+}` infrared spectrum in the gas phase as well as for the 
computation of infrared spectrum of small water molecule clusters and protonated water dimer system. 


Compiling
_________

A Fortran 90/95 compiler with MPI wrapper is required for successful compilation of the code. 
Although the correlation function subroutines are serial, the remaining code is parallelized so MPI wrappers have to be used. 
Quantum correlation subroutines within PIM_qcf modules are compiled by executing the command ``make`` in the ``./source`` directory. 
The same make command generates a ``RunPIMqcf.exe`` executable for testing of the correlation functions. 


Testing
_______

For PIM_qcf test purposes the ``numdiff`` package is used for automatic comparison purposes and should be made
available before running the tests, otherwise the ``diff`` command will be used automatically instead but the user
is warned that the test might fail due to numerical differences.
The user is advised to download and install ``numdiff`` from `here <http://www.nongnu.org/numdiff/>`_.
Tests and corresponding reference values are located in sub-directories ``./tests/xxx``, where ``xxx`` stands 
for ``oh``, ``ch4``, and ``ch5`` systems. 
Before running the tests the code has to be properly compiled by running the ``make`` command in the 
``./source`` sub-directory:

::

	cd tests

	./test.sh

Tests can be executed automatically by running the command ``./test.sh`` in the ``./tests`` sub-directory 
for all three systems, or separately for each system by running the command ``./test.sh`` within the corresponding 
sub-directory. 
All test are executed on one processor core.
Due to small numerical discrepancies between generated outputs and reference values which can cause the tests to fail, 
the user is advised to manually examine the numerical differences between generated output and the corresponding 
reference values in case the tests fail. 


Source Code
___________

The PIM_qcf module source code is located at: https://gitlab.e-cam2020.eu:10443/Quantum-Dynamics/PIM/tree/PIMqcf.


Source Code Documentation
_________________________

The source code documentation can be generated automatically in ``./doc`` sub-directory, 
html and latex format, by executing the following command in the ``./doc`` directory:

::

	doxygen PIMqcf_doxygen_settings


