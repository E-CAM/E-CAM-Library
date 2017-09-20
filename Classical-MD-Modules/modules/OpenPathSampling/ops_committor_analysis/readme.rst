.. _ost_committor:

######################################
Committor Analysis in OpenPathSampling
######################################

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

This module adds a simulator to perform committor analysis in
OpenPathSampling, given a set of initial points to shoot from.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

Frequently, we try to launch several trajectories from a point that
is not in any state, to see which state they land in. In a transition
A->B, the fraction that land in B is called the "committor" at that
point. Configurations which lead to a committor of approximately 50%
are said to make up the "transition state ensemble." This code
provides a straightforward way of calculating the committor for a
given set of initial conditions.

While this direct approach is not necessarily the best way to
calculate the committor, it is still a very useful tool for
obtaining initial trajectories for path sampling simulations.

The implementation in this module includes:

* ``SnapshotModifier`` abstract class to change a snapshot, along with
  concrete subclasses ``NoModification`` (used in testing) and
  ``RandomVelocities`` (used for committor analysis). This same class of
  object will be reused for two-way shooting.
* A ``CommittorSimulation`` subclass of ``PathSimulator`` to run the
  committor simulation.
* A generic ``TransformedDict`` object which acts as a dictionary, but
  applies an arbitrary key-altering function before accessing the keys.
* ``SnapshotByCoordinateDict``, a subclass of ``TransformedDict``, which
  uses the coordinates of a snapshot as the internal keys. Thus multiple
  snapshots with the same coordinates can map to the same values, regardless
  of their velocities.
* ``ShootingPointAnalysis``, a subclass of ``SnapshotByCoordinateDict``,
  which performs the analysis of shooting points. This includes calculating
  the committor and making 1D and 2D histograms of the committor (mapped
  with arbitrary axes).

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

* OPS docs committor example [`GitHub
  <https://github.com/openpathsampling/openpathsampling/blob/master/examples/misc/committors.ipynb>`_ | `Docs
  <http://openpathsampling.org/latest/examples/miscellaneous/committors.html>`_]
* Alanine dipeptide committor example [`GitHub
  <https://github.com/openpathsampling/openpathsampling/tree/master/examples/misc/alanine_dipeptide_committor>`_ 
  | `Docs
  <http://openpathsampling.org/latest/examples/miscellaneous/committors_alanine_dipeptide.html>`_]

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

This module has been merged into OpenPathSampling. It is composed of the
following pull requests:

.. * link PRs

* https://github.com/openpathsampling/openpathsampling/pull/450
* https://github.com/openpathsampling/openpathsampling/pull/454
* https://github.com/openpathsampling/openpathsampling/pull/466
* https://github.com/openpathsampling/openpathsampling/pull/601
* https://github.com/openpathsampling/openpathsampling/pull/618
* https://github.com/openpathsampling/openpathsampling/pull/647

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

