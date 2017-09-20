.. _ops_direct_rate_flux:

####################################################
Direct MD (on-the-fly) flux/rate in OpenPathSampling
####################################################

.. sidebar:: Software Technical Information

  The information in this section describes OpenPathSampling as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Language
    Python (2.7)

  Documentation Tool
    Sphinx, numpydoc format (ReST)

  Application Documentation
    http://openpathsampling.org

  Relevant Training Material
    http://openpathsampling.org/latest/examples/

  Licence
    LGPL, v. 2.1 or later

.. contents:: :local:

Authors: David W.H. Swenson

This module contains code to implement the direct (on-the-fly) calculation
of flux and rate in OpenPathSampling.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

This calculates the flux out of a state and through an interface, or the
rate of the transition between two states, while running a
trajectory. This means that you don't have to save all the frames of the
trajectory. This is especially useful for small (toy) systems, where you can
easily run very long trajectories to get very accurate results, and would
rather re-run than save the full trajectory.
A separate module calculates the rate and flux from an existing trajectory.

The primary object implemented in this module is the ``DirectSimulation``
subclass of ``PathSimulator``, which performs on-the-fly analysis of flux
and rate.

Background Information
______________________

This module builds on OpenPathSampling, a Python package for path sampling
simulations. To learn more about OpenPathSampling, you might be interested in
reading:

* OPS documentation: http://openpathsampling.org
* OPS source code: http://github.com/openpathsampling/openpathsampling


Testing
_______

Tests in OpenPathSampling use the `nose`_ package.

.. IF YOUR MODULE IS IN OPS CORE:

This module has been included in the OpenPathSampling core. Its tests can
be run by setting up a developer install of OpenPathSampling and running
the command ``nosetests`` from the root directory of the repository.

.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY

.. The tests for this module can be run by downloading its source code, 
.. installing its requirements, and running the command ``nosetests`` from the
.. root directory of the repository.

Examples
________

Flux for MISTIS: [`GitHub
<https://github.com/openpathsampling/openpathsampling/blob/master/examples/toy_model_mistis/toy_mistis_2_flux.ipynb>`_ | `Docs
<http://openpathsampling.org/latest/examples/mistis.html>`_]

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

This module has been merged into OpenPathSampling. It is composed of the
following pull requests:

.. * link PRs

* https://github.com/openpathsampling/openpathsampling/pull/495

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

