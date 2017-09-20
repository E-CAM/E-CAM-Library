.. _ops_piggybacker:

#######################################
OPS Piggybacker (legacy file converter)
#######################################

.. sidebar:: Software Technical Information

  This module is based on OpenPathSampling. This section includes
  information both for the specific module and for OpenPathSampling as a
  whole.

  Language
    Python (2.7)

  Documentation Tool
    Sphinx, numpydoc format (ReST)

  Application Documentation
    * http://openpathsampling.org
    * http://opspiggybacker.readthedocs.io (in progress)

  Installation instructions
    * OpenPathSampling: http://openpathsampling.org/latest/install.html
    * OPSPiggybacker: https://github.com/dwhswenson/OPSPiggybacker

  Relevant Training Material
    * http://openpathsampling.org/latest/examples/
    * https://github.com/dwhswenson/OPSPiggybacker/tree/master/examples

  Licence
    LGPL, v. 2.1 or later

.. contents:: :local:

Authors: David W.H. Swenson

This module provides a library for converting path sampling simulations from
legacy codes into a format that can be analyzed by OpenPathSampling. The
implementation in this module works with flexible-length transition path
sampling (TPS) with one-way shooting and uniform shooting point selection.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

OpenPathSampling contains excellent tools for analyzing simulations, as well
as excellent tools for sampling. However, since OpenPathSampling is a new
software package, users may already have simulations that they have run with
other packages. The purpose of this module is to provide tools that allow
the user to easily convert legacy script output into a format that can be
analyzed by OpenPathSampling.

The OPSPiggybacker essentially fakes a simulation, based on data from
another source. In this module, it has the ability to read in data from
one-way TPS. The user must create the appropriate OPS ``TransitionNetwork``
object (including defining the correct collective variables and state
volumes). Then the code creates a ``MoveScheme``, but instead of actually
running the simulation, it reads in the results of an existing simulation
and provides the same output that the ``MoveScheme`` *would* have provided.
We call this a "pseudo-simulation."

Classes implemented in this module include:

* ``ShootingPseudoSimulator``, subclass of
  ``openpathsampling.PathSimulator``. This acts like the OPS simulator, and
  runs the pseudo-simulation. Instead of taking an integer saying how many
  steps to run, it takes a list of data that describes each shooting move.
* ``ShootingStub``, subclass of ``openpathsampling.pathmovers.PathMover``.
  This acts like the ``openpathsampling.OneWayShootingMover``. It reads in
  the data and creates the appropriate output that can be analyzed by OPS.
* ``OneWayTPSConverter``, subclass of ``ShootingPseudoSimulator``. This
  pseudo-simulator is designed to read a specific type of input file, which
  can be prepared based on the output from legacy simulation tools.
  Depending on the nature of the input trajectory files, several options can
  be set to ensure that the resulting OPS trajectories are correct. This is
  an abstract superclass, subclasses must define how to read trajectory
  files of the appropriate format.
* ``GromacsOneWayTPSConverter``, subclass of ``OneWayTPSConverter``.
  Specialized for reading in GROMACS files (using MDTraj).

Background Information
______________________

This module builds on OpenPathSampling, a Python package for path sampling
simulations. To learn more about OpenPathSampling, you might be interested in
reading:

* OPS documentation: http://openpathsampling.org
* OPS source code: http://github.com/openpathsampling/openpathsampling


Testing
_______

Tests use the `nose`_ package.

.. IF YOUR MODULE IS IN OPS CORE:

.. This module has been included in the OpenPathSampling core. Its tests can
.. be run by setting up a developer install of OpenPathSampling and running
.. the command ``nosetests`` from the root directory of the repository.

.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY

To test this module, first install its requirements (namely
OpenPathSampling). Next, download OPSPiggybacker code, either by git or by
downloading the ``.tar`` or ``.zip`` of this release and decompressing it.
Change into the root directory of the OPSPiggybacker, and run ``python
ops_piggybacker/tests/common_test_data.py`` to prepare some test data. After
that, use the ``nosetests`` command to run the actual tests (this can be
done from the same directory).

Installation of this package can be performed with ``python setup.py
install``. Installation can be done before or after testing.

Examples
________

Several examples are in the `examples/` directory of the code:

* |partial_traj|_
* |prejoined_traj|_

.. * |gromacs_converter|_

.. |partial_traj| replace:: Using the ``ShootingPseudoSimulator`` with partial (one-way) trajectories
.. _partial_traj: https://github.com/dwhswenson/OPSPiggybacker/blob/master/examples/example_one_way_shooting.ipynb

.. |prejoined_traj| replace:: Using the ``ShootingPseudoSimulator`` with full (pre-joined) trajectories
.. _prejoined_traj: https://github.com/dwhswenson/OPSPiggybacker/blob/master/examples/example_prejoined.ipynb

.. .. |gromacs_converter| replace:: TODO: Using the ``GromacsOneWayTPSConverter`` on real data
.. .. _gromacs_converter: 

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

.. This module has been merged into OpenPathSampling. It is composed of the
.. following pull requests:

.. * link PRs

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

The module is for the 0.1 release of the OPSPiggybacker project. This
includes all the work on that project through pull request #15 (merged on
28 December, 2016).

The source code for this module can be found in:
https://github.com/dwhswenson/OPSPiggybacker/releases/tag/v0.1.0

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

