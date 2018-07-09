.. _ops_flux_rate_analysis:

######################################
Flux/Rate Analysis in OpenPathSampling
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

This module adds the ability to use existing trajectories to calculate the
flux through an interface or the rate of a transition in OpenPathSampling. 

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

Calculating the flux out of a state and through a given interface is an
important step in transition interface sampling (TIS). It is required for
the rate calculation, and can be used to determine the best location for the
innermost interface before performing the main TIS sampling.

In many cases, the main TIS sampling is only performed after a significant
amount of direct MD has been performed. For example, MD trajectories may
have already been used to identify and test the stability of stable states.
This module calculates the flux (as well as the rate, a related quantity)
from existing trajectories. These can be trajectories generated with OPS or
loaded into OPS from other simulation packages, such as Gromacs.

The flux calculation is used as part of obtaining the rate in TIS. While
some variants of TIS calculate the flux as part of their primary sampling,
others need an auxiliary calculation. In addition, the flux calculation is
always useful for confirming a good definition of the innermost interface in
TIS, and using an existing trajectory to select the location of the
innermost interface does not require re-running the dynamics.

The rate calculation will probably not be used very much, because rates
require extremely long trajectories.  Both flux and rate are included as a
single "module" because the code is very closely related.

This module analyses previously existing trajectories. Another module will
calculate these things on-the-fly with an OPS engine.

The primary new objects in this module are:

* ``TrajectoryTransitionAnalysis``: Contains all the methods to perform the
  analysis. The overall approach is based on identifying subtrajectory
  segments that correspond to various conditions, e.g., time in one volume
  before entering another volume (lifetimes, which are then related to the
  rate). By keeping these subtrajectories, this module also enables
  visualization on time traces of the trajectory.
* ``TrajectorySegmentContainer``: Container class for a list of trajectory
  segments, created as results of the ``TrajectoryTransitionAnalysis``
  object. Also includes conveniences for reporting by either number of
  frames or time: each is reported as a numpy array, so that numpy's built
  in statistics can be used for, e.g., mean or standard deviation.

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

Examples for this have been provided in the ``ops_additional_examples``
repository. In particular, the Jupyter notebooks:

* https://gitlab.e-cam2020.eu/dwhswenson/ops_additional_examples/blob/master/transition_analysis_Abl.ipynb
* https://gitlab.e-cam2020.eu/dwhswenson/ops_additional_examples/blob/master/DNA_flux_example.ipynb

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

This module has been merged into OpenPathSampling. It is composed of the
following pull requests:

* https://github.com/openpathsampling/openpathsampling/pull/435
* https://github.com/openpathsampling/openpathsampling/pull/448
* https://github.com/openpathsampling/openpathsampling/pull/451
* https://github.com/openpathsampling/openpathsampling/pull/654

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

