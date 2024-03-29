.. E-CAM documentation master file, created by
   sphinx-quickstart on Thu Sep 15 17:56:17 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. sidebar:: General Information

    .. contents:: :depth: 2

    * :ref:`contributing`
    * :ref:`search`

.. _readme_quantum_dynamics:

************************
Quantum Dynamics Modules
************************

Introduction
============

.. image:: ./images/AltLogo.png
   :width: 30 %
   :align: left

This is a collection of the modules that have been created by the E-CAM community within the area of Quantum Dynamics.
This documentation is created using ReStructured Text and the git repository for the documentation. Source files can be
found at https://gitlab.e-cam2020.eu/e-cam/E-CAM-Library which are open to contributions from E-CAM members.

In the context of E-CAM, the definition of a software module is any piece of software that could be of use to the E-CAM
community and that encapsulates some additional functionality, enhanced performance or improved usability for people
performing computational simulations in the domain areas of interest to the project.

This definition is deliberately broader than the traditional concept of a module as 
defined in the semantics of most high-level programming languages and is intended 
to capture internal workflow scripts, analysis tools and test suites
as well as traditional subroutines and functions. Because such E-CAM modules 
will form a heterogeneous collection we prefer to refer to this as an E-CAM 
software repository rather than a library (since the word library carries a 
particular meaning in the programming world). The modules do however share with 
the traditional computer science definition the concept of hiding the internal 
workings of a module behind simple and well-defined interfaces. 
It is probable that in many cases the modules will result from the abstraction 
and refactoring of useful ideas from existing codes rather than being written entirely de novo.

Perhaps more important than exactly what a module is, is how it is written and used. 
A final E-CAM module adheres to current best-practice programming style conventions, 
is well documented and comes with either regression or unit tests
(and any necessary associated data). E-CAM modules should be written in such a way 
that they can potentially take advantage of anticipated hardware developments in the 
near future (this is one of the training objectives of E-CAM).



Objectives of E-CAM WP3 Quantum Dynamics
========================================

Software development in quantum dynamics has so far been less systematic than in other fields of modelling,
such as classical molecular dynamics or electronic structure. Although some packages have been developed to
implement specific methods, e.g. `Quantics <http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/index.html>`_
for wave packet dynamics, or subroutines added to electronic structure
packages, e.g. Surface Hopping and Ehrenfest in CPMD_, these efforts are not the standard.

One of the goals of E-CAM's WP3 is then to provide an environment to stimulate the transition from in-house
codes, often developed and used by single groups, to the development of modular, well documented community-based
software packages capable of multiple functionalities and adopting a common set of standards and benchmarks.

To foster this development, we have initiated five parallel activities:

*  Creating software for benchmarking and testing based on exact integration schemes for low dimensional systems and
   standard potentials.

*  Creating an environment to transform in-house software to modules that adhere to the E-CAM best practices.

*  Disseminating this initiative to attract coding efforts from leading groups in the field to the E-CAM repository.

*  Interact with industrial partners to enrich our repository with software targeted at their needs.

*  Training young code developers.

.. _CPMD: http://www.cpmd.org/



Pilot Projects
==============

One of primary activity of E-CAM is to engage with pilot projects with industrial partners. These projects are conceived
together with the partner and typically are to facilitate or improve the scope of computational simulation within the
partner. The related code development for the pilot projects are open source (where the licence of the underlying
software allows this) and are described in the modules associated with the pilot projects.

The `pilot project <https://www.e-cam2020.eu/pilot-project-ibm/>`_ of the WP3 in collaboration with IBM_ is
related to quantum computing and improvements of the quantum computer technology.
One of our main topic was development of software for construction of control pulses necessary for operating quantum
logical gates between qubits in a universal quantum computer using the Local Control Theory. [Curc]_
More information can be found on the `pilot project <https://www.e-cam2020.eu/pilot-project-ibm/>`_ web site.
Below are listed the pilot project modules created so far:


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/LocConQubit/readme
    ./modules/Pergauss/readme
..    ./modules/module_faster/readme

**LocConQubit** is a code for the construction of controlled pulses on isolated qubit systems using the Local Control
Theory.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/OpenQubit/readme

**OpenQubit** is an extension to the LocConQubit code for the construction of controlled pulses in a more realistic
environment with dissipating effects.



Extended Software Development Workshops
=======================================


ESDW Maison de la Simulation (Paris 2016)
-----------------------------------------

The first Quantum Dynamics ESDW was held in June-July 2016 at the `Maison de la Simulation`_ near Paris. 10 students
and 6 tutors, including Dr. Ivano Tavernelli representing the industrial partner of the WP3, IBM_, worked to develop
software modules in the following areas:

- Exact quantum propagation methods for low dimensional systems to be used to provide benchmarks for approximate schemes

- Development of a library of single and multi surface potentials for benchmark systems

- Calculation of approximate quantum time correlation functions

Work was performed by teams of 2-4 students, assisted by the senior participants and by E-CAM's Software Manager,
Dr. Alan O'Cais, and the Software Developer associated to WP3, Dr. Liang Liang.

In addition to the software development activities, the Workshop enjoyed lively scientific discussions
centered on presentations made by the students and the senior participants. The on-line E-CAM tools for software
development, including the Git repository, and tools for the documentation (Doxygen) and performance analysis
were presented by E-CAM staff members and participants were instructed on their use via tutorials.
The program was further enriched by the interactions with experts on software and hardware development working
at Maison de la Simulation who gave talks on topics such as architectures and programming paradigms and the
use of advanced visualization tools such as the Image wall hosted by the Maison de la Simulation.

.. _Maison de la Simulation: http://www.maisondelasimulation.fr/en/index.php?a


ESDW University College Dublin (2017)
-------------------------------------

The second Quantum Dynamics ESDW was held in July 2017 (first part) and March 2018 (wrap up meeting) at
`University College Dublin <http://www.ucd.ie/>`_. 21 participants, including the representative of WP3’s
current industrial partner IBM_, worked to develop and upload on the E-CAM repositories software
modules in the following areas:

- Calculation of approximate quantum time correlation functions via the PaPIM code;

- Mixed quantum-classical algorithms, with specific reference to Surface Hopping and Wigner-Liouville methods;

- Implementation of the factorization scheme for quantum dynamics in CPMD_;

- Interfacing of quantum codes with electronic structure codes;

- Grid based exact propagation schemes;

- Design and optimization of qubit control pulses.

Teams of coders assisted by senior tutors, E-CAM’s Software Manager, Dr. Alan O’Cais, and WP3 Software
Developer, Dr. Liang Liang, performed the work.
Specific discussions on optimal parallelization strategies for the E-CAM’s quantum dynamical codes
(PaPIM and Quantics) were also initiated and implemented.
The coding work was accompanied by scientific presentations on the themes of the workshops and by
the instruction from E-CAM personnel on the CoE’s tools for software production, testing, documentation
and maintaining.
The participants benefitted also from the proximity of software and hardware experts from the
`ICHEC <https://www.ichec.ie/>`_ supercomputing center that offered, in particular, a set of
lectures and tutorials on OpenMP parallelization.

.. _IBM: https://www.zurich.ibm.com/

Modules developed in this workshop not included in other subheadings are:

.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/Correlated-Sampling/readme


ESDW Durham University (Durham 2019)
------------------------------------

This modules have been developed at Durham ESDW

.. toctree::
    :glob:
    :maxdepth: 1

..    ./modules/CTMQC/readme

List of available Modules
=========================

Below are listed all the modules from the E-CAM ESDWs in Quantum Dynamic developed up-to-date:


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/CTMQC/readme
    ./modules/EF_module/readme

The **CTMQC** module allows to simulate excited-state dynamics in model systems of one to three spatial (nuclear)
dimensions, with an arbitrary number of electronic states. The algorithm is based on the quantum-classical approximation
of the equations of motion derived in the framework of the exact factorization of the electron-nuclear wavefunction. In
practice, trajectories are used to mimic the nuclear evolution, that is, in turn, coupled to the quantum evolution of
the electronic degrees of freedom.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/SINGLEPATH/readme.rst

The **SinglePath** module uses combined quantum and classical descriptions of the dynamics to compute quantum rate
processes in condensed phase systems. The main purpose of this module is to act as the core of additional software
modules aimed at addressing important issues such as improving the speed of convergence of estimates using correlated
sampling, and much more realistic treatment of the classical bath, and connecting to other problems such as constant pH
simulation through an effective Hamiltonian.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/G-CTMQC/readme

The **G-CTMQC** module extends the previous **CTMQC** module, introducing new methodological and 
technical features. G-CTMQC is interfaced with the **QuantumModelLib** library of potentials, 
which gives more flexibility in the choice of systems that can be studied. The present 
implementation allows to perform surface hopping calculations, also with inclusion of energy 
decoherence corrections, and Ehrenfest dynamics, as well as CT-MQC calculations. Finally, 
spin-orbit coupling is included in CT-MQC (G-CT-MQC algorithm).

.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/PhysConst_module/readme

The **PhysConst** enables the use of physical constants and the correct isotopic masses.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/QuantumModelLib/readme

The **QuantumModelLib** use potential energy surfaces extracted from the literature and can be linked to quantum
dynamics codes.

.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/FBTS_MPI/FBTS_MPI_readme

The **FBTS_MPI_** module implements the Forward-Backward Trajectory Solution (FBTS) to the
quantum-classical Liouville equation developed by Hsieh and Kapral.


PaPIM
-----

PaPIM is a code for calculation of equilibrated system properties (observables). Some properties can be directly
obtained from the distribution function of the system, while properties that depends on the exact dynamics of the
system, such as the structure factor, [Mon2]_ infrared spectrum [Beu]_ or reaction rates, can be obtained from the
evolution of appropriate time correlation functions. PaPIM samples either the quantum (Wigner) or classical (Boltzmann)
density functions and computes approximate quantum and classical correlation functions.

The code is highly parallelized and suitable for use on large HPC machines. The code's modular structure enables an
easy update/change of any of its modules. Furthermore the coded functionalities can be used independently of each other.
The code is specifically design with simplicity and readability in mind to enable any user to easily implement its own
functionalities. The code has been extensively used for the calculation of the infrared spectrum of the
:math:`\text{CH}_{5}^{+}` cation in gas phase, while recently new calculations on the water dimer, and protonated water
dimer systems were started.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/PaPIM/readme

**PaPIM** is the current version of the code, including all available functionalities.

The following modules make up the PaPIM code and can be used as stand-alone software libraries for e.g.
sampling of the Wigner distribution, sampling of the classical Boltzmann distribution, or building MPI
parallelized Fortran codes.
Such libraries are rarely available to the community in a Fortran program format.
Some of the functionalities within the code are specifically designed for computation of infrared spectra, and serve as
a template for the user to implement its own functionalities.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/PIM_wd/readme

**PIM_wd** samples, via the Phase Integration Method, [Mon1]_ the system's quantum Wigner density function.
The function is given in the phase-space representation and is the basis for any further calculation of system's quantum
observables.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/PIM_qcf/readme

**PIM_qcf** is a library of quantum correlation functions for computing system's time-dependent properties.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/PIM_qtb/readme

**PIM_qtb**  implements different methods based on Langevin dynamics. 
The trajectories generated can be exploited directly or used to sample initial conditions for 
Linearized Semi-Classical Initial Value Representation (LSC-IVR) calculations.
The methods implemented are: classical Langevin dynamics, Quantum Thermal Bath (QTB)
and two variants of adaptive QTB (adQTB-r and adQTB-f).


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/ClassMC/readme

**ClassMC** samples, via Metropolis Monte Carlo algorithm, the system's classical Boltzmann distribution function and
calculates the classical time-dependent correlation functions from the sampled phase space.
Results obtained from classical sampling can be used to assess the relevance of quantum effects for a given system.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/PotMod/readme

**PotMod** is a library of potential energy functions and interfaces for external potential energy calculation codes.
Currently available in the library are the harmonic and Morse potentials (different molecular systems can be simulated
depending on parameters provided by the user);  empirical potential of the ground state of :math:`\text{CH}_{5}^{+}`
based on high level electronic structure calculations [ZJin]_; and the call to the ab initio
`CP2K code <https://www.cp2k.org/>`_ using the **PaPIM-CP2K_Interface** module.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/PaPIM-CP2K_Interface/readme

**PaPIM-CP2K_Interface** module links the PaPIM code with the `CP2K program package <https://www.cp2k.org/>`_
as an internal library for calculation of system's electronic structure properties.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/AuxMod/readme

**AuxMod** is a library of subroutines which enables any user to easily construct its own Fortran input parser.
It also contains a library of adapted MPI subroutines for easier programming of Fortran MPI parallel codes.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/Openmpbeads/readme

**Openmpbeads** is a patch to the PaPIM code which enables parallelization of the sampling of the
polymer chains within the PIM algorithm, improving efficiency in sampling of the Wigner density.

**PerGauss** is an implementation of periodic boundary conditions for gaussian basis functions
to be used within the quantics program package.

.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/Pergauss/readme


Quantics
--------

`Quantics <http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/index.html>`_ is suite of programs
for molecular quantum dynamics simulations.
The package is able to set up and propagate a wavepacket using the MCTDH method [Beck]_.
Numerically exact propagation is also possible for small systems using a variety of standard integration schemes
[Lefo]_, as is the solution of the time-independent Schrödinger equation using Lanczos diagonalisation.
The program can also be used to generate a ground state wavefunction using energy relaxation (i.e. propagation
in imaginary time) and with the "improved relaxation" it is even possible to generate (low lying) excited states.
Within the Quantics package there are also programs to propagate density operators (by solving the
Liouville-von Neumann equation for open or closed system) [Mey]_,
a program for fitting complicated multi-dimensional potential energy function, programs for determining bound
or resonance energies by filter-diagonalisation,
parameters of a vibronic coupling Hamiltonian, and many more.
Recent developments include the use of Gaussian wavepacket based methods (G-MCTDH) and interfaces to quantum chemistry
programs such as Gaussian and Molpro allow direct dynamics calculations using the vMCG method [Ric]_.
The following modules are extension of Quantics functionalities developed at E-CAM Extended Software Development
Workshops.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/SODLIB/sod_readme

The **SodLib** module provides exact wavefunction propagation using the second-order differencing (SOD) integrator
scheme to solve the time-dependent Schrödinger equation. This routine has been implemented and tested as an added
functionality within the Quantics_ quantum dynamics package.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/cheb_doc/cheb_readme

The **ChebLib** module implements the Chebyshev integration scheme for exact wavefunction propagation on the grid. This
routine  has been implemented and tested as an added functionality within the Quantics_ quantum dynamics package.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/QQ-Interface/qq-interface

The **Quantics-QChem-Interface** is an interface between Quantics and `QChem <http://www.q-chem.com/>`_. The DFT
algorithm implemented in QChem can be used to provide electronic structure information for direct dynamics simulations
using the Quantics program package.


.. toctree::
        :glob:
        :maxdepth: 1

        ./modules/zagreb_sh/zagrebsh_readme

The **Zagreb_sh** module is an interface between between Quantics package and the Tully's surface hoping code provided
by the group of Nadja Doslic in Zagreb.


.. toctree::
        :glob:
        :maxdepth: 1

        ./modules/Quantics_Openmp_Improvements_Module/Quantics_omp_module

The **Quantics_openmp** module is an initial effort at OpenMP parallelisation improvements to Quantics.

.. toctree::
        :glob:
        :maxdepth: 1

        ./modules/Quantics_DD_MPIOMP/readme

The **Quantics_DD_MPIOMP** module is a further improvement on the parallel version of DD-vMCG in 
Quantics by adding an extra layer of MPI parallelization to the existing OpenMP parallelization.

.. toctree::
        :glob:
        :maxdepth: 1

        ./modules/sharc_gym/readme

The **SHARC-gym** module uses the surface hopping code
SHARC and enables the use of a more accurate set of quantum methods implemented in QUANTICS.

CLstunfti
---------

`CLstunfti <https://gitlab.com/axelschild/CLstunfti>`_ is an extendable Python 
toolbox to compute scattering of electrons with a given kinetic energy in 
liquids and amorphous solids. It uses a continuum trajectory model with 
differential ionization and scattering cross sections as input to simulate the 
motion of the electrons through the medium. 

.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/CLstunfti/readme

The module **CLstunfti** makes CLstunfti available to the world by providing 
a documentation of the toolbox and inline documentations of the source code, 
as well as a set of examples that can also be used for testing.

.. toctree::
        :glob:
        :maxdepth: 1

        ./modules/guessoc/guessoc_readme

The **Spin orbit coupling smoothing** module is to smooth spin orbit couplings along internuclear distance.

.. toctree::
        :glob:
        :maxdepth: 1

        ./modules/Direct_Dynamics_Database/readme

The **Direct Dynamics Database** The Direct Dynamics Database module is an improved, 
more efficient version of the database used to provide the potential energy surfaces 
in the Direct Dynamics variational multi-configuration Gaussian wavepacket (DD-vMCG) 
method [Wor1]_ which is included in the powerful and flexible Quantics_ package 
program [Wor2]_.


ElVibRot
--------

`ElVibRot <https://github.com/lauvergn/ElVibRot-TnumTana>`_ is a package for general
quantum dynamics simulation using curvilinear coordinates. 
The code has no built-in limitation in terms of the number of degrees of freedom. 
It applied a numerical but exact kinetic energy operator with Tnum (Automatic
differentiation), 
which enables much flexibility in the choice of the curvilinear coordinates [Tnum]_. 
Moreover, the Smolyak algorithm [Smo]_ is employed to avoid the conventional direct-product
basis sets and grids, 
which allows the simulation of larger systems. Typically, the package could be used for

1) Vibrational levels, intensities for floppy molecular systems; 
2) Wave-packet propagation with or without time dependent Hamiltonian; 
3) Quantum gate and optimal control; 
4) Optimization with the given set of curvilinear coordinates.

.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/ElVibRot_TID_MPI/readme

The **ElVibRot-TID-MPI** (ElVibRot Time-independent MPI) module is a parallelized 
time-independent quantum simulation program. The Davidson algorithm is the main
method employed 
for getting the Eigen levels of the Hamiltonian.

.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/ElVibRot_TD_MPI/readme

The **ElVibRot-TD-MPI** (ElVibRot Time-dependent MPI) module is a parallelized 
time-dependent quantum simulation program. The available propagation methods 
include Chebyshev, Runge-Kunta, short iterative Lanczos and Taylor expansion, etc.

References
----------


.. [Curc] B. F. E. Curchod, T. J. Penfold, U. Rothlisberger, I. Tavernelli *Phys. Rev. A*
          **84** (2012) 042507 `DOI: 10.1103/PhysRevA.84.042507
          <https://journals.aps.org/pra/abstract/10.1103/PhysRevA.84.042507>`_
.. [Mon1] M. Monteferrante, S. Bonella, G. Ciccotti *Mol. Phys.* **109** (2011) 3015 `DOI: 10.1080/00268976.2011.619506
          <http://dx.doi.org/10.1080/00268976.2011.619506>`_
.. [Mon2] M. Monteferrante, S. Bonella, G. Ciccotti *J. Chem. Phys.* **138** (2013) 054118 `DOI: 10.1063/1.4789760
          <http://dx.doi.org/10.1063/1.4789760>`_
.. [Beu]  J. Beutier, M. Monteferrante, S. Bonella, R. Vuilleumier, G. Ciccotti *Mol. Sim.* **40** (2014) 196 `DOI:
          10.1080/08927022.2013.843776 <http://dx.doi.org/10.1080/08927022.2013.843776>`_
.. [ZJin] Z. Jin, B. Braams, J. Bowman *J. Phys. Chem. A* **110** (2006) 1569 `DOI: 10.1021/jp053848o
          <https://pubs.acs.org/doi/abs/10.1021/jp053848o>`_
.. [Beck] M. Beck, A. Jäckle, G.A. Worth, and H.-D. Meyer *Phys. Rep.* **324** (2000) 1–106
          `DOI: 10.1016/S0370-1573(99)00047-2 <https://doi.org/10.1016/S0370-1573(99)00047-2>`_
.. [Lefo]  C. Leforestier, R. H. Bisseling, C. Cerjan, M. D. Feit, R. Friesner, A. Guldberg, A. Hammerich,
          G. Jolicard, W. Karrlein, H.-D. Meyer, N. Lipkin, O. Roncero, R. Kosloff *J. Comp. Phys.* **94** (1991) 59
          `DOI: 10.1016/0021-9991(91)90137-A <https://doi.org/10.1016/0021-9991(91)90137-A>`_
.. [Mey]  H.-D. Meyer, G. A. Worth *Theor. Chem. Acc.* **109** (2003) 251 `DOI: 10.1007/s00214-003-0439-1 <https://doi.org/10.1007/s00214-003-0439-1>`_
.. [Ric]  G. W. Richings, I. Polyak, K. E. Spinlove, G. A. Worth, I. Burghardt, B. Lasorne
          *Int. Rev. Phys. Chem.* **34** (2015) 269 `DOI: 10.1080/0144235X.2015.1051354 <https://doi.org/10.1080/0144235X.2015.1051354>`_
.. [Wor1]  G. A. Worth, M. A. Robb, B. L. Lasorne
          *Mol. Phys.* **106** (2008) 2077–2091 `DOI: 10.1080/00268970802172503 <https://doi.org/10.1080/00268970802172503>`_
.. [Wor2]  G. A. Worth, K. Giri, G. W. Richings, M. H. Beck, A. Jäckle, H.-D. Meyer
            Quantics package, version 1.1, (2015)
.. [Tnum] D. Lauvergnat, A. Nauts, *Phys. Chem. Chem. Phys.* **12** (2010) 8405-8412 `DOI: 10.1039/C001944E <http://dx.doi.org/10.1039/C001944E>`_
.. [Smo]  S. A. Smolyak, *Dokl. Akad. Nauk SSSR* **148** (1963) 1042–1045 `<http://mi.mathnet.ru/eng/dan27586>`_
