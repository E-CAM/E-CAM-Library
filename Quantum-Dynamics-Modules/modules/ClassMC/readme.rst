.. _classmc:

####################
ClassMC
####################

.. sidebar:: Software Technical Information

  Language
    Fortran 95/90

  Licence
    MIT license (MIT)

  Documentation Tool
    Doxygen

.. contents:: :local:


Purpose of Module
_________________

Module **ClassMC** samples the system phase space using the classical Boltzmann distribution function and calculates the 
time correlation functions from the sampled initial conditions. 
The sampling is achieved by the Monte Carlo Metropolis algorithm. 
The corresponding system properties can be calculated from the sampled phase space with appropriate operators. 
The sampled phase space points can be propagated in time using classical molecular dynamics in order to investigate the time 
evolution of the system and calculate the corresponding correlation functions. 
Currently the electric dipole moment operator is implemented for the calculation of electric dipole moment autocorrelation 
functions from which system IR spectra can be directly obtained. 
The system potential energy is calculated using external subroutines provided by the user. 
Example external subroutines are provided for the :math:`\text{OH}` and :math:`\text{CH}_{4}` systems, with 
potential energies are described by an harmonic potential, 
and the electric dipole moments by point charge approximation. An external subroutines for calculation of 
:math:`\text{CH}_{5}^{+}` system potential energy and electric dipole moment, based on fitted values, is also given. 


Applications of the Module
__________________________

The main application of ClassMC code is classical sampling of the system's phase space and computing classical observables, 
which are necessary for comparison with the real experimental data or quantum simulations in order to detect and explain the, 
sometimes hardly detectable, quantum effects which are responsible for exact system properties. 
In this respect, the ClassMC module was extensively used in the study of the :math:`\text{CH}_{5}^{+}` system classical distribution 
and its classically obtained infrared spectrum in order to identify the quantum tunnelling effects responsible for the 
redshift of C-H stretching bands and the overall shape of the infrared spectrum. 


Compiling
_________

Fortran compiler with a MPI wrapper together with ``lapack`` libraries have to be available to successfully compile the code. 
The user is advised to examine the ``Makefile`` in the ``./source``` sub-directory prior to code compilation in order to 
select an appropriate compiler and to check or adapt the compiler options to his local environment, or to generally 
modify the compiler options to his requirements. 
Upon adapting the ``Makefile``, the code compilation is executed by command ``make`` in the ``./source`` sub-directory: 

::

	cd source

	make

An executable ``ClassMCRun.exe`` is created upon successful compilation. 


Testing
_______

For PaPIM test purposes the ``numdiff`` package is used for automatic comparison purposes and should be made
available before running the tests, otherwise the ``diff`` command will be used automatically instead but the user
is warned that the test might fail due to numerical differences.
The user is advised to download and install ``numdiff`` from `here <http://www.nongnu.org/numdiff/>`_.
Tests and corresponding reference values are located in sub-directories ``./tests/xxx/CLASSICAL``, where ``xxx`` stands 
for ``oh``, ``ch4``, and ``ch5`` systems. 
Before running the tests the module ClassMC has to be properly compiled by running the ``make`` command in the 
``./source`` sub-directory. 
Tests can be executed automatically by running the command ``./test.sh`` in the ``./tests`` sub-directory 
for all three systems, or separately for each system by running the command ``./test.sh`` within the corresponding 
system ``CLASSICAL`` sub-directory:

::

	cd tests

	./test.sh [number of cores]

Tests are by default executed on two processor cores. 
This can be changed by setting the value of required 
cores as an integer number after the command ``./test.sh`` (example ``./test.sh 20``, for the use of 20 processor 
cores in the test). 
The number of processor cores should not exceed 50. 
Due to small numerical discrepancies between generated outputs and reference values which can cause the tests to fail, 
the user is advised to manually examine the numerical differences between generated output and the corresponding 
reference values in case the tests fail. 


Source Code
___________

The ClassMC module source code is located at: https://gitlab.e-cam2020.eu/Quantum-Dynamics/PIM/tree/ClassMC.


Source Code Documentation
_________________________

The source code documentation is given at https://gitlab.e-cam2020.eu/Quantum-Dynamics/PIM/tree/ClassMC/doc.
The documentation files (html and latex format) are obtained by executing the ``make`` command in the ``./doc`` directory:

::

	cd ./doc

	make


