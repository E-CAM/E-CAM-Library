:orphan:

.. _\EFAC2019:

#####
EFAC
#####

.. sidebar:: Software Technical Information

  Language
    Fortran 90

  License
    GNU Lesser General Public License (LGPL)

  Software Module Developed by
     Hugo Bessone, Lea-Maria Ibele, Emanuele Marsili, Francesco Talotta, David Lauvergnat, Basile F. E. Curchod, Federica Agostini

.. contents:: :local:


Purpose of Module
_________________

This module is an analysis tool to be employed for excited-state, nonadiabatic dynamics simulations. The physical situation to be studied is, for instance, the sub-picosecond response of a molecule to an UV/visible ultrashort laser pulse, that excites the molecule electronically. The photo-excited molecule can relax via, so-called, radiationless channels, i.e., internal conversion processes, towards the electronic ground state. To describe such ultrafast processes, it is essential to account for (i) electronic transitions, thus changes of electronic states, that are induced by nuclear motion, and (ii) the quantum mechanical nature of the nuclei. Various numerical approaches exist nowadays to perform simulations of nonadiabatic processes, based on the -- standard, and thus widely used -- Born-Huang representation, and on the Exact Factorization. The ultimate goal here is to provide the quantum dynamics community with an easy-to-use analysis tool able to make the link between Born-Huang and Exact Factorization.


Short description
___________________________________________________

The Exact Factorization Analysis Code provides a post processing tool to transform the result of a molecular quantum dynamics simulation from the Born-Huang representation to the one of the Exact Factorization. 
Using the output provided by the grid-based quantum dynamics ElVibRot code [ElVibRot]_, this module calculates the two key quantities of the Exact Factorization: the *time-dependent potential energy surface* and *time-dependent vector potential* that have been used to offer new perspectives on numerous nonadiabatic processes (see Refs [Gross_PRL2010]_ , [Gross_JCP2012]_ , [Agostini_JPCL2017]_ , [Gross_JCP2015]_ , [Gross_MP2013]_ ).

The purpose of **EFAC** is to familiarize the user with the framework of the Exact Factorization and to connect it with the more commonly used quantum dynamics methods. 
Hence, the central purpose of this module is to make the Exact Factorization of the electron-nuclear wavefunction easily accessible to the broad quantum dynamics community. 

The two time-dependent potentials of the Exact Factorization can be easily recovered by expressing them in a diabatic or adiabatic basis. This connection offers a bridge between quantum dynamics simulation conducted in the Born-Huang representation and the Exact Factorization; a bridge exploited in this module.

In the framework of the Exact Factorization, the nuclear and electronic wavefunctions are unique up to gauge transformation. While this gauge can in principle be chosen arbitrarily, the current implementation enforces two specific gauges that have proved useful in previous studies (see references above). For one-dimensional simulations, the gauge is fixed by making the time-dependent vector potential equal to zero. While being convenient, this gauge cannot be generalized to higher dimensions. As such, the tool uses a different gauge for problems in higher dimensions where the phase of the nuclear wavefunction is zero, i.e., the nuclear wavefunction will be real and non-negative at all times.

The module uses the result of a grid-based quantum dynamics calculation -- (timedependent) nuclear wavefunctions in a diabatic basis and corresponding diagonal and off-diagonal potential surfaces -- to construct the time-dependent potential energy surface (*TDPES*). The TDPES can be split into three components, two gauge-independent and one gauge-dependent.  It outputs separately the two gauge-independent contributions as well as the gauge-dependent one. Additionally, the time dependent vector potential is obtained (*TDVP*). 


Practical application and exploitation of the code
___________________________________________________

This code is intended to provide an easy access to the time-dependent potential energy surface and vector potential of the Exact Factorization from the result of a quantum dynamics simulation in any arbitrary number of dimensions and electronic states. This allows the user to study simple nonadiabatic model systems in low dimensions, but also the more complex nonadiabatic dynamics of molecules through conical intersections.

Installation
____________

The **EFAC** is a fortran90 based code. Compilation of the code requires the ``gfortran`` compiler. 

Once the tarball ``EFAC.tar.gz`` from the `EFAC repository <https://gitlab.e-cam2020.eu/marsili/efac/>`_ has been
downloaded, create a new directory and untar the file typing 

::

	tar -zxvf EFAC.tar.gz

Compile the source code and generate the executable *EF.x*.

::

        make


Testing
_______

Go in the directory test/1d and copy there the executable *EF.x*. Run the compiled code. 

::

	./EF.x


The executable reads and analyses two files already present there: ``EF_parameter_dpsi`` and ``EF_parameter_gV``.
It will generate, for each time-step, three files: ``Epsilon``, ``Density`` and ``potential.dat``. In the ``Epsilon``
file is printed, for each grid point value, the ``TDPES`` and ``TDVP``. In the ``Density`` file is printed, for
each grid point value, the modulus of the wavefunction and the diabatic densities.  

``processing.x`` is an additional tool provided whenever the number of degrees of freedom are more than 1. Copy
the executable in the test directory and run the code. It reads the output files, created with the ``EF.x`` executable,
and it prints the results along a specific degree of freedom. Therefore, it will generate two additional output files
called ``Epsilon-cut.out`` and ``Density-cut.out``. In the current version, the program makes the one-dimensional
cuts only. In addition, when ``processing.x`` is run, it requires some parameters that have to be prompted in the
following order:

* the time at which the cut has to be performed,
* the number of states,
* the degree of freedom treated as the independent variable and
* the values at which all the other degrees of freedom are fixed. 

::

	./processing.x 1 2 2 0.2

In this 2d case, the 2nd degrees of freedom is the independent variable while the first degrees of freedom is
fixed at the value of 0.2. The ``processing.x`` will use ``Density001.dat`` and ``Epsilon001.dat`` files,
containing the information after 1 fs of propagation.
 	

Source Code
___________

The EFAC source code and test files can be found at `EFAC <https://gitlab.e-cam2020.eu/marsili/efac/>`_.


References
__________

.. [Gross_PRL2010] Abedi,  A., Maitra, N. T., Gross, E. K. U. *Phys. Rev. Lett.* 
	**105** (2010) 123002 Exact factorization of the time-dependent electron-nuclear wave
  function. 

.. [Gross_JCP2012] Abedi,  A., Maitra, N. T., Gross, E. K. U. *Phys. Rev. Lett.* 
	**137** (2012) 22A530 Correlated electron-nuclear dynamics: Exact factorization of the
  molecular wave-function.

.. [Gross_MP2013] Agostini, F., Abedi, A., Suzuki, Y., and Gross, E. K. U. *Mol. Phys.*
	**111** (2013) 3625--3640  Mixed quantum-classical dynamics on the exact time-dependent
  potential energy surfaces: A fresh look at non-adiabatic processes.

.. [Gross_JCP2015] Agostini, F., Abedi, A., Suzuki, Y., Min, S. K., Maitra, N. T., and Gross, E. K. U. *J. Chem. Phys.* 
	**142** (2015) 084303 The exact forces on classical nuclei in non-adiabatic charge transfer.

.. [Agostini_JPCL2017]  Curchod, B. F. E., and Agostini, F. *J. Phys. Chem. Lett.*
	**105** (2017) 831--837 On the dynamics through a conical intersection. 
 
.. [ElVibRot] Lauvergnat, D. *J. Chem. Phys.* 
	Elvibrot: Quantum dynamics code. 
	<https://github.com/lauvergn/ElVibRot-TnumTana>`_


 

