.. E-CAM documentation master file, created by
   sphinx-quickstart on Thu Sep 15 17:56:17 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _readme_quantum_dynamics:

************************
Quantum Dynamics Modules
************************

Introduction
============

.. sidebar:: General Information

    .. contents:: :depth: 2

    * :ref:`contributing`
    * :ref:`search`

.. image:: ./images/AltLogo.png
   :width: 30 %
   :align: left

This is a collection of the modules that have been created by E-CAM community 
within the area of Quantum Dynamics. This documentation is created using ReStructured Text and the git repository for the documentation 
source files can be found at 
https://gitlab.e-cam2020.eu/e-cam/E-CAM-Library which are open to contributions from E-CAM members.

In the context of E-CAM, the definition of a software module is any piece of software that could be of use to the E-CAM
community and that encapsulates some additional functionality, enhanced performance or improved usability for people
performing computational simulations in the domain areas of interest to us.

This definition is deliberately broader than the traditional concept of a module as defined in the semantics of most
high-level programming languages and is intended to capture inter alia workflow scripts, analysis tools and test suites
as well as traditional subroutines and functions. Because such E-CAM modules will form a heterogeneous collection we
prefer to refer to this as an E-CAM software repository rather than a library (since the word library carries a
particular meaning in the programming world). The modules do however share with the traditional computer science
definition the concept of hiding the internal workings of a module behind simple and well-defined interfaces. It is
probable that in many cases the modules will result from the abstraction and refactoring of useful ideas from existing
codes rather than being written entirely de novo.

Perhaps more important than exactly what a module is, is how it is written and used. A final E-CAM module adheres to
current best-practice programming style conventions, is well documented and comes with either regression or unit tests
(and any necessary associated data). E-CAM modules should be written in such a way that they can potentially take
advantage of anticipated hardware developments in the near future (and this is one of the training objectives of E-CAM).

Objectives of E-CAM WP3 Quantum Dynamics
========================================

Software development in quantum dynamics has so far been less systematic than in other fields of modelling, 
such as classical molecular dynamics or electronic structure. Although some packages have been developed to 
implement specific methods, e.g. Quantics_ for dynamics with MCTDH, or subroutines added to electronic structure 
packages, e.g. Surface Hopping and Ehrenfest in CPMD_, these efforts are not the standard. 

One of the goals of E-CAM's WP3 is then to provide an environment to stimulate the transition from in-house
codes, often developed and used by single groups, to the development of modular, well documented community-based
software packages capable of multiple functionalities and adopting the common set of standards and benchmarks.

To foster this development, we have initiated five parallel activities: 

*  Creating software for benchmarking and testing based on exact integration schemes for low dimensional systems and standard potentials.

*  Creating an environment to transform in-house software to modules that adhere to the E-CAM best practices.

*  Disseminating this initiative to attract coding efforts from leading groups in the field to the E-CAM repository.

*  Interact with industrial partners to enrich our repository with software targeted at their needs.

*  Training young code developers.

.. _Quantics: http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/index.html
.. _CPMD: http://www.cpmd.org/

Pilot Projects
==============

One of primary activity of E-CAM is to engage with pilot projects with industrial partners. These projects are conceived
together with the partner and typically are to facilitate or improve the scope of computational simulation within the
partner. The related code development for the pilot projects are open source (where the licence of the underlying
software allows this) and are described in the modules associated with the pilot projects.

Extended Software Development Workshops
=======================================

ESDW Maison de la Simulation (Paris 2016)
-----------------------------------------

The first Quantum Dynamics ESDW was held in June-July 2016 at the `Maison de la Simulation`_ near Paris. 10 students
and 6 tutors, including Dr. Ivano Tavernelli representing the industrial partner of the WP3, IBM, worked to develop
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
at La Maison de la Simulation who gave talks on topics such as architectures and programming paradigms and the 
use of advanced visualization tools such as the Image wall hosted by the Maison de la Simulation.

.. _Maison de la Simulation: http://www.maisondelasimulation.fr/en/index.php?a

ESDW University College Dublin (2017)
-------------------------------------

The second Quantum Dynamics ESDW was held in July 2017 at the University College Dublin.

List of available Modules
=========================

.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/SODLIB/sod_readme

The **SodLib** module provides exact wavefunction propagation using the second-order differencing (SOD) integrator
scheme to solve the time-dependent Schroedinger equation. This routine has been implemented and tested as an added
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

    ./modules/PhysConst_module/readme

The **PhysConst** enables the use of physical constants and the correct isotopic masses.

PaPIM
-----

PaPIM is a code for calculation of equilibrated system properties (observables). Some properties can be directly obtained from
the sampled system distribution function, while properties that depends on the exact dynamics of the system, such as the structure
factor, [Mon2]_ infrared spectrum [Beu]_ or reaction rates, can be easily obtained from corresponding correlation functions.
PaPIM code samples either the quantum (Wigner) or classical (Boltzmann) density functions and calculates the corresponding correlation
functions. The code is highly parallelized which makes it suitable for use on large HPC machines.
The code's modular structure enables an easy update/change of any of its modules as well as that the coded functionalities
can be used independently of each other.
The code is specifically design with simplicity and readability in mind to enable any user to easily implement its own functionalities.
The code has been extensively used for the calculation of the infrared spectrum of the :math:`\text{CH}_{5}^{+}` cation in gas phase,
while recently new calculations on water dimer, and protonated water dimer systems were started.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/PaPIM/readme

**PaPIM** is the current version of the code with all included functionalities.

The following modules make up the PaPIM code and can be used as stand-alone software libraries for e.g. sampling of the Wigner distribution,
sampling of the classical Boltzmann distribution, or building MPI parallelized Fortran codes. Such libraries are rarely available to the community in a Fortran program format.
Some of the functionalities coded within the code are specifically design for computation of infrared spectra, and serve as a template
for the user to implement its own functionalities.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/PIM_wd/readme

**PIM_wd** samples, via the Phase Integration Method, [Mon1]_ the system's quantum Wigner density distribution function. The distribution is
given in the phase-space representation and is the basis for any further calculation of system's quantum observables.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/PIM_qcf/readme

**PIM_qcf** is a library of quantum correlation functions for computing system's time-dependent properties.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/ClassMC/readme

**ClassMC** samples, via Metropolis Monte Carlo algorithm, the system's classical Boltzmann distribution function and calculates
the classical time-dependent correlation functions from the sampled phase space.
Results obtained from classical sampling can be used to assess the relevance of quantum effects for a given system.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/PotMod/readme

**PotMod** is a library of potential energy functions and interfaces for external potential energy calculation codes.
Currently available in the library are the harmonic and Morse potentials (different molecular systems can be simulated depending on parameters
provided by the user);  empirical potential of the ground state of :math:`\text{CH}_{5}^{+}` based on high level
electronic structure calculations [Jin]_, and interface to the ab initio CP2K_ code.

.. _CP2K: https://www.cp2k.org/


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/AuxMod/readme

**AuxMod** is a library of subroutines which enables any user to easily construct its own Fortran input parser.
It also contains a library of adapted MPI subroutines for easier programming of Fortran MPI parallel codes.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/PIM_qcf/readme


References
==========

.. [Mon1] M. Monteferrante, S. Bonella, G. Ciccotti `Linearized symmetrized quantum time correlation functions calculation via phase pre-averaging`_ *Mol. Phys.* **109** (2011) 3015
.. [Mon2] M. Monteferrante, S. Bonella, G. Ciccotti `Quantum dynamical structure factor of liquid neon via a quasiclassical symmetrized method`_ *J. Chem. Phys.* **138** (2013) 054118
.. [Beu] J. Beutier, M. Monteferrante, S. Bonella, R. Vuilleumier, G. Ciccotti `Gas phase infrared spectra via the phase integration quasi-classical method`_ *Mol. Sim.* **40** (2014) 196
.. [Jin] Jin, Braams, Bowman `An ab Initio Based Global Potential Energy Surface Describing :math:'\text{CH}_{5}^{+} \rightarrow \text{CH}_{3}^{+} + \text{H}_{2}'`_ *J. Phys. Chem. A* **110** (2006) 1569

.. _Linearized symmetrized quantum time correlation functions calculation via phase pre-averaging: http://dx.doi.org/10.1080/00268976.2011.619506
.. _Quantum dynamical structure factor of liquid neon via a quasiclassical symmetrized method: http://dx.doi.org/10.1063/1.4789760
.. _Gas phase infrared spectra via the phase integration quasi-classical method: http://dx.doi.org/10.1080/08927022.2013.843776
.. _An ab Initio Based Global Potential Energy Surface Describing :math:'\text{CH}_{5}^{+} \rightarrow \text{CH}_{3}^{+} + \text{H}_{2}': pubs.acs.org/doi/abs/10.1021/jp053848o
.. Jin, Braams, Bowman `An ab Initio Based Global Potential Energy Surface Describing :math:'\text{CH}_{5}^{+} \rightarrow \text{CH}_{3}^{+} + \text{H}_{2}'`_ *J. Phys. Chem. A* **110** (2006) 15
.. _E-CAM: https://www.e-cam2020.eu/
