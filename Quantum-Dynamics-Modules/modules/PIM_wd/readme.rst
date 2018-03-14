.. _PIM_wd:

####################
PIM_wd
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

Module PIM_wd implements the Phase Integration Method (PIM) [Mon1]_ [Mon2]_ for the exact sampling of the quantum Wigner 
distribution in phase space representation. 
The PIM samples the thermal Wigner density through the use of a generalised Monte Carlo scheme for sampling of 
phase space points, which combines the Penalty [Pen]_ and Kennedy [Ken]_ algorithms in order to sample the noisy 
probability densities. 
This is necessary because the estimator of the quantum thermal density is not known analytically but must be 
computed via a statistical average acted by uncertainty. 
Through the action of certain operators on the sampled phase space corresponding system properties can be obtained, 
or the sampled points used as initial conditions in subsequent dynamics from which certain time dependent properties 
can be obtained. 
The latter is the basis of calculation of time-dependent correlation function with the PIM algorithm via  
the module PaPIM_. 
The user is required to provide the potential energy of the system by incorporating an external potential energy 
subroutine into the PotMod_ potential energy library. 

.. _PaPIM: ./modules/PaPIM/readme
.. _PotMod: ./modules/PotMod/readme


Applications of the Module
__________________________

This module forms the basis for computing the time-dependent cross- and auto-correlation functions with the PIM algorithm. 
It has been used in the calculation of :math:`\text{CH}_{5}^{+}` infrared spectrum and in the gas phase as well as for the 
computation of infrared spectrum of small water molecule clusters and protonated water dimer system.


Compiling
_________

Fortran compiler with a MPI wrapper together with lapack libraries have to be available to successfully compile the code. 
The user is advise to examine the Makefile in the ``./source``` sub-directory prior to code compilation in order to
select an appropriate compiler and to check or adapt the compiler options to his local environment, or to generally
modify the compiler options to his requirements.

::

        cd source

        make

Upon adapting the ``Makefile``, the code compilation is executed by command ``make`` in the ``./source`` sub-directory.
An executable ``PaPIM.exe`` is created upon successful compilation.
For PaPIM test purposes the ``numdiff`` package should be made available before running the tests. 
In case the numdiff is not available on the system the ``diff`` command will be automatically used instead. 
The user is advise to download and install numdiff from here_ .
The PaPIM documentation is obtained by executing the ``make`` command in the ./doc sub-directory.

.. _here: http://www.nongnu.org/numdiff/


Testing
_______

Tests and corresponding reference values are located in sub-directories ``./tests/``. The tests are performed over 
three systems, the :math:`\text{OH}`, :math:`\text{CH}_{4}` and :math:`\text{CH}_{5}^{+}`. They are located in their corresponding 
``oh``, ``ch4`` and ``ch5``, 
where each sub-directory contains corresponding classical and quantum input files located in ``CLASSICAL`` and ``QUANTUM`` 
sub-directories, respectively. 
Before running the tests the code has to be properly compiled by running the ``make`` command in the 
``./source`` sub-directory. 
The numdiff package is used for automatic comparison purposes and should be made available before running the tests, 
otherwise the diff command will be used automatically instead but the user is warned that the test might fail 
due to numerical differences. 
The tests are performed automatically by executing the command ``./test.sh`` in the ``./tests`` sub-directory 
for all three systems:

::

        cd tests

        ./test.sh [number of cores]

Tests are by default performed using two processor cores, which can be changed by setting the value of required 
cores as an integer number after the command ``./test.sh`` (example ``./test.sh 20``, for the use of 20 processor 
cores in the test). The number of processor cores should not exceed 20. 
Due to small numerical discrepancies between generated outputs and reference values which can cause the tests to fail, 
the user is advise to manually examine the numerical differences between generated output and the corresponding 
reference values in case the tests fail. 


Source Code
___________

The PIM_qcf module source code is located at: https://gitlab.e-cam2020.eu:10443/Quantum-Dynamics/PIM/tree/PIMwd.


Source Code Documentation
_________________________

The source code documentation can be generated automatically in ./doc sub-directory, 
html and latex format, by executing the ``doxygen PIMwd_doxygen_settings`` command in the ./doc directory:

::

        doxygen PIMwd_doxygen_settings


References
__________

.. [Pen] Ceperley, Dewing `The penalty method for random walks with uncertain energies`_ *J. Chem. Phys.* **110** (1999) 9812
.. [Ken] Kennedy, Kuti `Noise without Noise - A New Monte Carlo Method`_ *Phys. Rev. Lett.* **54** (1985) 2473

.. _Linearized symmetrized quantum time correlation functions calculation via phase pre-averaging: http://dx.doi.org/10.1080/00268976.2011.619506
.. _Quantum dynamical structure factor of liquid neon via a quasiclassical symmetrized method: http://dx.doi.org/10.1063/1.4789760
.. _The penalty method for random walks with uncertain energies: http://dx.doi.org/10.1063/1.478034
.. _Noise without Noise - A New Monte Carlo Method: https://doi.org/10.1103/PhysRevLett.54.2473


