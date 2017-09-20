.. _classmc:

####################
ClassMC
####################

.. sidebar:: Software Technical Information

  Language
    Fortran 90

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

Module ClassMC samples the system phase space using the classical Boltzmann distribution function and calculates the
time correlation functions from the sampled initial conditions.
The sampling is achieved by the Monte Carlo Metropolis algorithm. 
The corresponding system properties can be calculated from the sampled phase space with appropriate operators. 
The sampled phase space points can be propagated in time using classical molecular dynamics in order to obtain the time
evolution of the system.
Currently the electric dipole moment operator is implemented for calculation of electric dipole moment for the 
calculation of electric dipole moment autocorrelation functions from which system IR spectra can be directly obtained. 
The system potential energy and electric dipole moment are calculated using external subroutines provided by the user. 
Example external subroutines are provided for the OH and CH\ :sub:`5` \ systems, respectively, whose corresponding
potential energies are described by the harmonic potential,
while the electric dipole moments by point charge approximation. An external subroutines for calculation of
CH\ :sub:`5`:sup:`+` \ system potential energy and electric dipole moment, based on fitted values, is also given.



Compiling
_________

Fortran compiler with a MPI wrapper together with lapack libraries have to be available to successfully compile the code.
The user is advise to examine the Makefile in the ``./source``` sub-directory prior to code compilation in order to
select an appropriate compiler and to check or adapt the compiler options to his local environment, or to generally
modify the compiler options to his requirements.
Upon adapting the ``Makefile``, the code compilation is executed by command ``make`` in the ``./source`` sub-directory.
An executable ``ClassMCRun.exe`` is created upon successful compilation.
For ClassMC test purposes the numdiff package should be made available before running the tests. 
In case the numdiff is not available on the system the diff command will be automatically used instead. 
The user is advise to download and install numdiff from http://www.nongnu.org/numdiff/ .
The ClassMC documentation is obtained by executing the ``make`` command in the ./doc directory.


Testing
_______

Tests and corresponding reference values are located in sub-directories ``./tests/xxx/CLASSICAL``, where ``xxx`` stands
for ``oh``, ``ch4``, and ``ch5`` systems.
Before running the tests the module ClassMC has to be properly compiled by running the ``make`` command in the
``./source`` sub-directory.
The numdiff package is used for comparison purposes and should be made available before running the tests, 
otherwise the diff command will be used automatically instead but the user is warned that the test might fail 
due to numerical differences. 
Tests can be executed automatically by running the command ``./test.sh`` in the ``./tests`` sub-directory
for all three systems, or separately for each system by running the command ``./test.sh`` within the corresponding
system ``CLASSICAL`` sub-directory.
Tests are by default executed on two processor cores and this can be changed by setting the value of required 
cores as an integer number after the command ``./test.sh`` (example ``./test.sh 20``, for the use of 20 processor
cores in the test). The number of processor cores should not exceed 50. 
Due to small numerical discrepancies between generated outputs and reference values which can cause the tests to fail, 
the user is advise to manually examine the numerical differences between generated output and the corresponding 
reference values in case the tests fail.


Source Code
___________

The ClassMC module source code is located at: https://gitlab.e-cam2020.eu/Quantum-Dynamics/PIM/tree/ClassMC/source.


Source Code Documentation
_________________________

The source code documentation is given at https://gitlab.e-cam2020.eu/Quantum-Dynamics/PIM/tree/ClassMC/doc.
The documentation files (html and latex format) are obtained by executing the ``make`` command in the ./doc directory.
