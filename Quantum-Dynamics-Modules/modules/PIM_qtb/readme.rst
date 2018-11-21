.. _PIM_qtb:

#######
PIM_qtb
#######

.. sidebar:: Software Technical Information

  Language
    Fortran 90/95

  Licence
    MIT license (MIT)

  Documentation Tool
    Sphinx

.. contents:: :local:


Purpose of Module
_________________


Module **PIM_qtb**  generates trajectories based on several classical and semi-classical stochastic methods:
- Langevin classical dynamics
- Quantum Thermal Bath [Dam]_
- Adaptive Quantum Thermal Bath [Man]_ 

These trajectories can be used to sample initial conditions for intramolecular vibrational-energy redistribution (IVR) dynamics. 

Applications of the module
__________________________

This module has been used to calculate time correlation function of an anharmonic oscillator parameterized on a hydroxyle bond OH and to compute pair correlation functions of a 13 atoms' neon cluster described by a Lennard-Jones potential. 


Compiling
_________

A Fortran 90/95 compiler with MPI wrapper is required for successful compilation of the code. 
Although the correlation function subroutines are serial, the remaining code is parallelized so MPI wrappers have to be used. 
Quantum correlation subroutines within PIM_qtb modules are compiled by executing the command ``make`` in the ``./source`` directory. 
The same make command generates a ``PaPIM.exe`` executable for testing of the correlation functions. 


Testing
_______

For PIM_qtb test purposes the ``numdiff`` package is used for automatic comparison purposes and should be made
available before running the tests, otherwise the ``diff`` command will be used automatically instead but the user
is warned that the test might fail due to numerical differences.
The user is advised to download and install ``numdiff`` from `here <http://www.nongnu.org/numdiff/>`_.
Tests and corresponding reference values are located in sub-directories ``./tests/xxx``, where ``xxx`` stands 
for ``oh`` and ``lj`` systems. 
``lj`` tests also requires a Python distribution.
Before running the tests the code has to be properly compiled by running the ``make`` command in the 
``./source`` sub-directory:

::

	cd tests

	./test.sh

Tests can be executed automatically by running the command in the ``./tests`` sub-directory :
#. ``./test+lgv.sh`` for tests on OH bonds compared to previous classical implementation  
#. ``./test_lj.sh`` for tests on a Ne:. 
All test are executed on one processor core.
Due to small numerical discrepancies between generated outputs and reference values which can cause the tests to fail, 
the user is advised to manually examine the numerical differences between generated output and the corresponding 
reference values in case the tests fail. 


Source Code
___________

The PIM_qtb module source code is located at: https://gitlab.e-cam2020.eu:10443/thomas.ple/PIM.git (Temporary link).


Source Code Documentation
_________________________

A brief summary of the methods can be found in the following link:

.. toctree::
       :glob:
    :maxdepth: 1

    ./langevin_for_PaPIM

It can also be compiled by executing the following commands with "Sphinx" python module installed:

::

   sphinx-build -b html source build
   make html

The source code documentation can be generated automatically in ``./doc`` sub-directory, 
html and latex format, by executing the following command in the ``./doc`` directory:

::

	doxygen PIMqcf_doxygen_settings

References
__________
.. [Dam] H. Dammak, Y. Chalopin, M. Laroche, M. Hayoun, J.-J. Greffet,  Quantum Thermal Bath for Molecular Dynamics Simulation, Phys. Rev. Lett. 103 (2009) 190601.

.. [Man]   E. Mangaud,  S. Huppert,  T. Pl'e,  P. Depondt,  S. Bonella,  F. Finocchi, Quantum thermal bath with enforced fluctuation-dissipation theorem for reliable simulations of nuclear quantum effects, Journal of Chemical Theory and Computation, Submitted (2018).




