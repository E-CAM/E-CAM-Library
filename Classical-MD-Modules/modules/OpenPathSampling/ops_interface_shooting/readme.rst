.. _ops_interface_shooting:

:orphan:

##################################################
Interface-Constrained Shooting in OpenPathSampling
##################################################

.. sidebar:: Software Technical Information

  The information in this section describes OpenPathSampling as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Language
    Python

  Documentation Tool
    Sphinx, numpydoc format (ReST)

  Application Documentation
    http://openpathsampling.org

  Relevant Training Material
    http://openpathsampling.org/latest/examples/

  Licence
    LGPL, v. 2.1 or later

  Software module developed by
    Peter G. Bolhuis and David W.H. Swenson


.. contents:: :local:

This module adds interface-constrained shooting to OpenPathSampling.
Interface-constrained shooting is a technique that can improve the
efficiency of transition interface sampling.

Purpose of Module
_________________

In transition interface sampling (TIS), one defines stable states (volumes in
phase space) and interfaces (surfaces in phase space). For a trajectory to
be accepted in TIS, it must begin with exactly one frame in a given initial
state, cross the interface, and end with exactly one frame in any state
volume (including the initial state).

New trajectories are generated with the shooting move, which selects a point
along an initial trajectory from which new frames can be made. In one-way
shooting, the dynamics only needs to run in one direction (with the
stochastic nature of the dynamics ensuring that a new trajectory is
generated).

However, if the interfaces are far from the initial state and if all frames
are equally likely to be used for shooting, it can be very likely for the
shooting point to come before the trajectory has crossed the interface. This
can then lead to shooting moves that usually generate trajectories that
don't cross the interface, and therefore must be rejected. This uses a lot
of simulation effort without generating useful new trajectories.

Interface-constrained shooting [1]_ is an approach to solve
this problem. Instead of selecting from anywhere along the trajectory, only
the first point after crossing the interface is allowed as a shooting point.
This ensures that every trajectory that is generated will be valid (will
cross the interface). In addition, because the first crossing is still the
first crossing in the new trajectory, this leads to the Metropolis
acceptance probability also being 1. Therefore, every trial trajectory is
accepted.

In practice, this must be combined with the path reversal move in order to
sample all of trajectory space. The result is an approach with very high
acceptance, although decorrelation of the trajectory is a little slower.


This module implements interface-constrained shooting using:

* ``ForwardShootingStrategy``: An OPS ``MoveStrategy`` to do forward-only
  shooting. The interface-constrained shooting approach uses forward-only
  stochastic dynamics, counting on path reversal to handle the backward-time
  dynamics.
* ``InterfaceConstrainedSelector``: A ``ShootingPointSelector`` that selects
  the first point outside the given volume (the boundary of which defines
  the interface).

.. [1] TODO: Add citation here


Background Information
______________________

This module builds on OpenPathSampling, a Python package for path sampling
simulations. To learn more about OpenPathSampling, you might be interested in
reading:

* OPS documentation: http://openpathsampling.org
* OPS source code: http://github.com/openpathsampling/openpathsampling


Testing
_______

Tests in OpenPathSampling use `pytest`_.

.. IF YOUR MODULE IS IN OPS CORE:

This module has been included in the OpenPathSampling core. Its tests can
be run by installing pytest and OPS (with commit ``c340818``, which will be
part of release 0.9.6 and later), and running the command ``py.test
--pyargs  openpathsampling``.

.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY

.. The tests for this module can be run by downloading its source code, 
.. installing its requirements, and running the command ``py.test`` from the
.. root directory of the repository.

Examples
________

An example of this is in the following notebook:

* https://github.com/openpathsampling/openpathsampling/blob/7e157661dd8633690ebfcae4a8265fc14e31c5b9/examples/toy_model_mstis/toy_mstis_A4_constrained_shooting.ipynb

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

This module has been merged into OpenPathSampling. It is composed of the
following pull requests:

* https://github.com/openpathsampling/openpathsampling/pull/788
* https://github.com/openpathsampling/openpathsampling/pull/790
* https://github.com/openpathsampling/openpathsampling/pull/800

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _pytest: http://pytest.org/

