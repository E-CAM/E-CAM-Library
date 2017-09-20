.. E-CAM documentation master file, created by
   sphinx-quickstart on Thu Sep 15 17:56:17 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. _readme_meso_multi:

**********************************************
The E-CAM Meso- and Multi-scale Module Library
**********************************************

Introduction
============

.. sidebar:: General Information

    .. contents:: :local:

    * :ref:`search`

.. image:: ./images/DPD1.jpg
   :width: 10 %
   :align: left

This is a collection of the modules that have been created by E-CAM community 
within the area of Meso- and Multi-scale Modelling. This documentation is 
created using ReStructured Text and the git repository for the documentation 
source files can be found at 
https://gitlab.e-cam2020.eu/e-cam/Meso-Multi-Scale-Modelling-Modules which are
public and open to contributions.

In the context of E-CAM, the definition of a software module is any piece of software that could be of use to the E-CAM community and that encapsulates some additional functionality, enhanced performance or improved usability for people performing computational simulations in the domain areas of interest to us. 

This definition is deliberately broader than the traditional concept of a module as defined in the semantics of most high-level programming languages and is intended to capture inter alia workflow scripts, analysis tools and test suites as well as traditional subroutines and functions. Because such E-CAM modules will form a heterogeneous collection we prefer to refer to this as an E-CAM software repository rather than a library (since the word library carries a particular meaning in the programming world). The modules do however share with the traditional computer science definition the concept of hiding the internal workings of a module behind simple and well-defined interfaces. It is probable that in many cases the modules will result from the abstraction and refactoring of useful ideas from existing codes rather than being written entirely de novo.

Perhaps more important than exactly what a module is, is how it is written and used. A final E-CAM module adheres to current best-practice programming style conventions, is well documented and comes with either regression or unit tests (and any necessary associated data). E-CAM modules should be written in such a way that they can potentially take advantage of anticipated hardware developments in the near future (and this is one of the training objectives of E-CAM). 

Pilot Projects
==============

One of primary activity of E-CAM is to engage with pilot projects with industrial partners. These projects are conceived
together with the partner and typically are to facilitate or improve the scope of computational simulation within the
partner. The related code development for the pilot projects are open source (where the licence of the underlying
software allows this) and are described in the modules associated with the pilot projects.

Extended Software Development Workshops
=======================================

DL_MESO_DPD
-----------

The following modules connected to the DL_MESO_DPD code have been produce so far:

.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/DL_MESO_DPD/dipole_dlmeso_dpd/readme
    ./modules/DL_MESO_DPD/format_dlmeso_dpd/readme
    ./modules/DL_MESO_DPD/dipole_af_dlmeso_dpd/readme
    ./modules/DL_MESO_DPD/moldip_af_dlmeso_dpd/readme
    ./modules/DL_MESO_DPD_onGPU/add_gpu_version/readme

ESPRESSO++
----------

The following modules connected to the ESPRESSO++ code have been produce so far:

.. toctree::
    :glob:
    :maxdepth: 1

    ./modules/hierarchical-strategy/components/fixed-local-tuple/readme
    ./modules/hierarchical-strategy/components/md-softblob/readme
    ./modules/hierarchical-strategy/components/minimize-energy/readme

ESDW Barcelona 2017
-------------------

The first Meso- and Multi-scale ESDW in Barcelona in 2017 will be the starting point for the modules to be included here.

.. _E-CAM: https://www.e-cam2020.eu/
