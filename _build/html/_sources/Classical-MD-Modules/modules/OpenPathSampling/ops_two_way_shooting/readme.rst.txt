.. _ops_two_way_shooting:

####################################
Two-Way Shooting in OpenPathSampling
####################################

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

This module adds the ability to do two-way shooting to OpenPathSampling.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

Different types of dynamics are better suited for different kinds of Monte
Carlo moves in path sampling. "One-way shooting," which was already
implemented in OpenPathSampling, is an efficient approach for sampling paths
when the dynamics are chaotic or diffusive. However, it requires stochastic
dynamics, and therefore isn't appropriate for deterministic dynamics, as
should be used with ballistic processes. For ballistic processes and
deterministic dynamics, the "two-way shooting" move, which is implemented in
this module, should be used. These moves differ in that one-way shooting
selects a frame as a shooting point and evolves the trajectory *either*
forward or backward, keeping part of the original trajectory, whereas
two-way shooting selects a shooting point, modified it (usually by changing
the velocities) and evolves *both* forward and backward. In one-way
shooting, the stochastic dynamics obviates the need to modify the shooting
point.

The shooting point selection methods used by one-way shooting can be re-used
for two-way shooting. However, two-way shooting requires a
``SnapshotModifier``, which one-way shooting does not. The basics of the
``SnapshotModifier``, as well as an implementation which completely
randomizes the velocities according to the Boltzmann distribution, were
included in a module to do committor simulations.

This module implements the movers and move strategies necessary to support
two-way shooting:

* ``AbstractTwoWayShootingMover``, subclass of ``EngineMover``. There's a
  possibility that generalized ensembles may behave differently if you shoot
  the forward part of the path first, versus the backward part of the path.
  This isn't the case for common TIS and TPS ensembles, but in case it is
  important for other ensembles, we create separate forward-first and
  backward-first two way shooting movers. This class is a common abstract
  base class for both.
* ``ForwardFirstTwoWayShootingMover``, subclass of
  ``AbstractTwoWayShootingMover``, which implements two-way shooting where
  the forward half-trajectory is generated first.
* ``BackwardFirstTwoWayShootingMOver``, subclass of
  ``AbstractTwoWayShootingMover``, which implements two-way shooting where
  the backward half-trajectory is generated first.
* ``TwoWayShootingMover``, subclass of ``RandomChoiceMover``, which randomly
  selects to either shoot forward-first or backward-first.
* ``TwoWayShootingStrategy``, subclass of ``MoveStrategy``, which generates
  a ``TwoWayShootingMover`` for every ensemble of interest.

In addition, this module implements new snapshot modifiers to change the
velocities of the shooting point:

* ``GeneralizedDirectionModifier``, subclass of ``SnapshotModifier``, which
  acts as an abstract base class for the specific velocity direction
  modifiers.
* ``VelocityDirectionModifier``, subclass of
  ``GeneralizedDirectionModifier``, which modifies all the velocities of the
  selected subset of atoms.
* ``SingleAtomVelocityDirectionModifier``, subclass of
  ``GeneralizedDirectionModifier``, which modifies the velocities on one of
  the selected subset of atoms.

Beyond these, the previously existing ``RandomVelocities`` snapshot modifier
is also likely to be used with two-way shooting.


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

An example of this can be found at:

* https://gitlab.e-cam2020.eu/dwhswenson/ops_additional_examples/blob/master/two_way_shooting.ipynb

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

This module has been merged into OpenPathSampling. It is composed of the
following pull requests:

.. * link PRs

* https://github.com/openpathsampling/openpathsampling/pull/600
* https://github.com/openpathsampling/openpathsampling/pull/650
* https://github.com/openpathsampling/openpathsampling/pull/652

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

