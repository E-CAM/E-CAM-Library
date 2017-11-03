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
https://gitlab.e-cam2020.eu/e-cam/Quantum-Dynamics-Modules which are open to contributions from E-CAM members.

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

.. _Quantics: http://stchem.bham.ac.uk/~quantics/
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


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/PotMod/readme

**PotMod** is structured as a library in which users can store new potentials. Potentials currently available in the
module are:  harmonic and Morse potentials (different molecular systems can be simulated depending on parameters
provided by the user);  empirical potential of the ground state of CH\ :sub:`5`:sup:`+` \ based on high level
electronic structure calculations [1]_.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/AuxMod/readme

The **AuxMod** module contains a set of subroutines which enables any user to easily construct a Fortran input parser.
It also contains a library of adapted MPI subroutines for easier programming of Fortran MPI parallel codes.


.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/ClassMC/readme

**ClassMC** samples, via Metropolis Monte Carlo, the classical Boltzmann distribution function and calculates classical
time correlation functions from the sampled initial conditions. This module provides a classical reference result to
assess the relevance of quantum effects for a given system.



References
==========

.. [1] Jin, Braams, Bowman *J. Phys. Chem. A* **110** (2006) 1569

.. _E-CAM: https://www.e-cam2020.eu/
