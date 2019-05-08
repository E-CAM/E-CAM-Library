.. _ops_visit_all_states:

#############################
OPS Visit All States Ensemble
#############################

.. sidebar:: Software Technical Information

  The information in this section describes OpenPathSampling as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Language
    Python (2.7, 3.5, 3.6)

  Documentation Tool
    Sphinx, numpydoc format (ReST)

  Application Documentation
    http://openpathsampling.org

  Relevant Training Material
    http://openpathsampling.org/latest/examples/

  Licence
    LGPL, v. 2.1 or later

  Software module developed by
    David W.H. Swenson

.. contents:: :local:

This module adds a convenient new OpenPathSampling ensemble that allows
trajectories to continue until they have visited all the states in the
system. In addition, it provides real-time reporting about the progress.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

One of the ways to get initial trajectories for path sampling is to use
dynamics that aren't physical for the ensemble of interest, such as using an
increased temperature. If a trajectory has a frame in every state, then in
must have subtrajectories that connect from each state to another one, and
therefore it has all the information to start a MSTIS simulation. The
ensemble definition tools in OPS make it easy to create such 

However, users often want to do simulation setup in an interactive mode,
such as in a Jupyter notebook, or at a minimum want to have a sense of the
progress made on a long trajectory such as this. The default OPS ensemble
gives no output and therefore no sense of how much progress has been made.

This module provides a custom OPS ensemble that gives such output during its
simulation. It outputs the length of the trajectory so far, as well as the
states that have and have not already been visited. This gives a much better
sense of how long the simulation will take to run.

This module includes:

* ``default_state_progress_report``: A function to create the progress
  report string. This can be replaced by another function to customize the
  output.
* ``VisitAllStatesEnsemble``: A class that wraps an OPS
  ``SequentialEnsemble``. The ``can_append`` method, called while generating
  the dynamics, can output information about the progress. The behavior of
  this output can be set with the initialization variable ``progress``,
  which can take the values of ``default`` for the default output,
  ``silent`` for no output, or can take a 2-tuple of callables where the
  first determines what to write, and the second determined how to emit the
  information.


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
be run by installing pytest and OPS (with commit 8e767872, which will be
part of release 1.0 and later), and running the command ``py.test --pyargs
openpathsampling``.

.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY

.. The tests for this module can be run by downloading its source code, 
.. installing its requirements, and running the command ``py.test`` from the
.. root directory of the repository.

Examples
________

This module is used in the OPS `alanine dipeptide MSTIS example <https://github.com/openpathsampling/openpathsampling/blob/master/examples/alanine_dipeptide_mstis/AD_mstis_1_setup.ipynb>`_, during the creation of initial trajectories.

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

This module has been merged into OpenPathSampling. It is composed of the
following pull requests:

* https://github.com/openpathsampling/openpathsampling/pull/826

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _pytest: http://pytest.org/

