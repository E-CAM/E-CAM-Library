.. _PIM_qcf:

####################
PIM_qcf
####################

.. sidebar:: Software Technical Information

  Language
    Fortran 90/95

  Licence
    MIT license (MIT)

  Documentation Tool
    Doxygen

.. contents:: :local:

.. This is an example of what a *module* for E-CAM looks like. Please add to this template any additional items that are
.. straightforward to fill out in the general case. You are free add any level of complexity you wish (within the bounds of
.. what ReST_ can do).

.. To add your module, fork this GitLab repository to your account on GitLab. Clone your repository, make a feature branch
.. and add a directory that will contain your module information. Copy this :download:`readme.rst` file there. Push your
.. changes back to GitLab and immediately open a merge request from your feature branch against our repository. We can
.. discuss your module in the merge request and help you get it accepted.

.. Add technical info as a sidebar and allow text below to wrap around it

Purpose of Module
_________________

Module PIM_qcf is a library of quantum cross- and auto-correlation functions used for computation of quantum time-dependent correlation functions 
within the Phase Integration Method (PIM). 
Two auto-correlation functions are currently implemented, the quantum position-position point charge dipole moment correlation function, and 
the velocity-velocity point charge dipole moment correlation function, all in the Kubo representation of the correlation functions. 
The user can  follow the two examples in constructing his/her own quantum correlation function.


Applications of the module
__________________________

This module has been used in the calculation of :math:`\text{CH}_{5}^{+}` infrared spectrum in the gas phase as well as for the 
computation of infrared spectrum of small water molecule clusters and protonated water dimer system. 


Compiling
_________

A Fortran 90/95 compiler with MPI wraper is required for successfully compilation of the code. 
Although the correlation function subroutines are serial, the remaining code is parallelized so MPI wrapers have to be used. 
Quantum correlation subroutines within PIM_qcf modules are compiled by executing the command ``make`` in the ``./source`` directory. 
The same make command generates a ``RunPIMqcf.exe`` executable for testing of the correlation functions. 
The user is advise to download and install ``numdiff`` from here_ prior to code testing. 
All test are executed on one processor core.

.. _here: http://www.nongnu.org/numdiff/


Testing
_______

Tests and corresponding reference values are located in sub-directories ``./tests/xxx``, where ``xxx`` stands 
for ``oh``, ``ch4``, and ``ch5`` systems. 
Before running the tests the code has to be properly compiled by running the ``make`` command in the 
``./source`` sub-directory:

::

	cd tests

	./test.sh

The ``numdiff`` package is used for comparison purposes and should be made available before running the tests, 
otherwise the ``diff`` command will be used automatically instead but the user is warned that the test might fail 
due to numerical differences. 
Tests can be executed automatically by running the command ``./test.sh`` in the ``./tests`` sub-directory 
for all three systems, or separately for each system by running the command ``./test.sh`` within the corresponding 
sub-directory. 
Due to small numerical discrepancies between generated outputs and reference values which can cause the tests to fail, 
the user is advise to manually examine the numerical differences between generated output and the corresponding 
reference values in case the tests fail. 


Source Code
___________

The PIM_qcf module source code is located at: https://gitlab.e-cam2020.eu:10443/Quantum-Dynamics/PIM/tree/PIMqcf.


Source Code Documentation
_________________________

The source code documentation can be generated automatically in ./doc sub-directory, 
html and latex format, by executing the ``doxygen PIMqcf_doxygen_settings`` command in the ./doc directory:

::

	doxygen PIMqcf_doxygen_settings

