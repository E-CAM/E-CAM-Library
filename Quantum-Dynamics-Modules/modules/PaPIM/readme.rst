.. _PaPIM:

####################
PaPIM
####################

.. sidebar:: Software Technical Information

  Language
    Fortran 90/95

  Licence
    MIT license (MIT)

  Documentation Tool
    Doxygen

.. contents:: :local:


Purpose of Module
_________________

**PaPIM** is a code for computing time-dependent correlation functions and sampling of the phase space. 
It samples the phase space either classically or quantum mechanically. 
For the classical sampling of the phase space a Monte Carlo algorithm samples the Boltzmann distribution function, 
while for the quantum sampling a Phase Integration Method (PIM) [PMon1]_ [PMon2]_ is utilized for an exact sampling of the quantum 
Wigner density distribution. 
From the sampled phase space points trajectories are propagated in time using classical molecular dynamics 
in order to obtain the appropriate time-dependent correlation functions. 
The code is designed so the user can easily couple it with its own external potential energy code/library 
and/or correlation functions subroutines. 
Example external subroutines are provided for the :math:`\text{OH}` and :math:`\text{CH}_{4}` systems, respectively, 
with potential energies described by the harmonic potential, 
and the electric dipole moments by point charge approximation. 
An external subroutine for calculation of 
:math:`\text{CH}_{5}^{+}` system potential energy and electric dipole moment, based on fitted values, [PJin]_ is also given. 
The electric dipole moment operator is currently implemented into the code for calculation of the 
electric dipole moment autocorrelation function from which system IR spectra can be directly obtained.


Phase Integration Method (PIM)
______________________________

The Phase Integration Method (PIM) is a novel approximate quantum dynamical technique developed for computing 
systems time dependent observables. [PMon1]_ [PMon2]_ [PBeu]_ 
PIM employs an algorithm in which the exact sampling of the quantum thermal Wigner density is combined 
with a linearized approximation of the quantum time propagators represented in the path integral formalism 
that reduces the evolution to classical dynamics. 
The quantities of interest can then be computed by combining 
classical molecular dynamics algorithms with a generalized Monte Carlo sampling scheme for sampling of the 
quantum initial conditions. 

.. image:: ./PaPIM.png
   :width: 80 %
   :align: center


Applications of the Module
__________________________

The PaPIM code has been extensively used for the calculation of the :math:`\text{CH}_{5}^{+}` system infrared absorption 
spectrum in the gas phase. 
These calculations also provided the benchmark of the PIM method as well as for the code performance analysis. 
The results obtained on the :math:`\text{CH}_{5}^{+}` system are currently under preparation for publication. 
One master thesis was completed by applying the code. 
Investigations of the processes shaping the infrared spectrum of small water cluster systems and a protoneted 
water dimer system are currently being investigated using the PaPIM code. 


Compiling
_________

Fortran compiler with a MPI wrapper together with lapack libraries have to be available to successfully compile the code. 
The user is advised to examine the ``Makefile`` in the ``./source`` sub-directory prior to code compilation in order to
select an appropriate compiler and to check or adapt the compiler options to his local environment, or to generally
modify the compiler options to his requirements.

::

	cd source

	make

Upon adapting the ``Makefile``, the code compilation is executed by command ``make`` in the ``./source`` sub-directory.
An executable ``PaPIM.exe`` is created upon successful compilation.


Testing
_______

For PaPIM test purposes the ``numdiff`` package is used for automatic comparison purposes and should be made 
available before running the tests, otherwise the ``diff`` command will be used automatically instead but the user 
is warned that the test might fail due to numerical differences. 
The user is advised to download and install ``numdiff`` from `here <http://www.nongnu.org/numdiff/>`_.
Tests and corresponding reference values are located in sub-directories ``./tests/``. The tests are performed on 
three systems, the :math:`\text{OH}`, :math:`\text{CH}_{4}` and :math:`\text{CH}_{5}^{+}`. They are located in their 
corresponding sub-directories, 
``oh``, ``ch4`` and ``ch5``, 
where each sub-directory contains corresponding classical and quantum input files located in ``CLASSICAL`` and ``QUANTUM`` 
sub-directories, respectively. 
Before running the tests the code has to be properly compiled by running the ``make`` command in the 
``./source`` sub-directory. 
The tests are performed automatically by executing the command ``./test.sh`` in the ``./tests`` sub-directory 
for all three systems:

::

	cd tests

	./test.sh [number of cores]

Tests are by default performed using two processor cores.
This can be changed by setting the value of required 
cores as an integer number after the command ``./test.sh`` (example ``./test.sh 20``, for the use of 20 processor 
cores in the test). 
The number of processor cores should not exceed 20. 
Due to small numerical discrepancies between generated outputs and reference values which can cause the tests to fail, 
the user is advised to manually examine the numerical differences between generated output and the corresponding 
reference values in case the tests fail. 


Performance and benchmarking
----------------------------

PaPIM is designed as a highly scalable code. Its performance was extensively tested. 

.. toctree::
    :glob:
    :maxdepth: 1

    ./performance


Source Code
___________

.. The PaPIM module source code can be obtained from: https://gitlab.e-cam2020.eu:10443/Quantum-Dynamics/PIM/tree/master/source.

The PaPIM module source code can be obtained from: https://gitlab.e-cam2020.eu/Quantum-Dynamics/PIM/tree/PaPIM.


Source Code Documentation
_________________________

The source code documentation is given at this `link <https://gitlab.e-cam2020.eu:10443/Quantum-Dynamics/PIM/tree/master/doc>`_.
The documentation files (html and latex format) are obtained by executing the ``make`` command in the ``./doc`` directory:

::

	cd doc

	make


References
__________

.. [PMon1] M. Monteferrante, S. Bonella, G. Ciccotti *Mol. Phys.* **109** (2011) 3015 `DOI: 10.1080/00268976.2011.619506
          <http://dx.doi.org/10.1080/00268976.2011.619506>`_
.. [PMon2] M. Monteferrante, S. Bonella, G. Ciccotti *J. Chem. Phys.* **138** (2013) 054118 `DOI: 10.1063/1.4789760
          <http://dx.doi.org/10.1063/1.4789760>`_
.. [PBeu] J. Beutier, M. Monteferrante, S. Bonella, R. Vuilleumier, G. Ciccotti *Mol. Sim.* **40** (2014) 196 `DOI: 
         10.1080/08927022.2013.843776 <http://dx.doi.org/10.1080/08927022.2013.843776>`_
.. [PJin] Z. Jin, B. Braams, J. Bowman *J. Phys. Chem. A* **110** (2006) 1569 `DOI: 10.1021/jp053848o 
         <https://pubs.acs.org/doi/abs/10.1021/jp053848o>`_


