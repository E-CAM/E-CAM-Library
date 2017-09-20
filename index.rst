.. E-CAM documentation master file, created by
   sphinx-quickstart on Thu Sep 15 17:56:17 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _readme:

************************
The E-CAM Module Library
************************

.. sidebar:: Scientific Areas

    * :ref:`readme_classical_md`
    * :ref:`readme_electronic_structure`
    * :ref:`readme_quantum_dynamics`
    * :ref:`readme_meso_multi`   

    **Quicklinks**

    .. contents:: :local:

    * :ref:`search`

This is a collection of the modules that have been created by E-CAM community 
within the four initial target areas of E-CAM:

.. toctree::
    :maxdepth: 1

    ./Classical-MD-Modules/index
    ./Electronic-Structure-Modules/index
    ./Quantum-Dynamics-Modules/index
    ./Meso-Multi-Scale-Modelling-Modules/index
 

This documentation is created using ReStructured Text and the git repository for the documentation source files can be
found at https://gitlab.e-cam2020.eu/e-cam/E-CAM-Library which are open to contributions from anyone in the E-CAM
community.

What is a module?
=================

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

E-CAM Activities
================

Pilot Projects
--------------

One of primary activity of E-CAM is to engage with pilot projects with industrial partners. These projects are conceived
together with the partner and typically are to facilitate or improve the scope of computational simulation within the
partner. The related code development for the pilot projects are open source (where the licence of the underlying
software allows this) and are described in the modules associated with the pilot projects.

Extended Software Development Workshops
---------------------------------------

E-CAM carries out 2 week software development workshops.

Contributing to this documentation
==================================

This documentation is completely open and we welcome both internal and external contributions. If you would like to
contribute to this effort then please follow the steps below to allow us to include your contribution.

.. toctree::
    :glob:
    :maxdepth: 1

    ./contributing

.. _E-CAM: https://www.e-cam2020.eu/

