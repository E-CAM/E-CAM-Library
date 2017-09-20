.. _tse_module:

#############################################
Transition State Ensemble in OpenPathSampling
#############################################

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


Authors: Sander Roet

This module is an addition to OpenPathSampling to calculate the snapshots that
correspond to the transition state ensemble from a list of trajectories.

Purpose of Module
_________________

.. Give a brief overview of the transition state ensemble module.

Often in transition path sampling we want to get an idea about the features of
the transition. This is done by generating an ensemble of snapshots that
correspond to a committor of approximately 50%. This ensemble gives information
about the transition state and the shape of the barrier. This code provides a
straightforward way of calculating this ensemble for a given list of
trajectories.

This module tries to efficiently find a single transition state frame from each
trajectory. This is done by bisection of the trajectory, depending on the
current committor. For example, if the current committor is to high (to much
ends up in state B) the next index is selected halfway towards the left edge
and the current index is set as the new right edge. This is repeated untill a
committor within a given range is reached or no new frame can be selected.

In the end this module returns a dictionary of shape ``{snapshot: comittor
value}`` which then can be used for analysis.

The implementation in this module includes:

* A ``TransitionStateEnsemble`` subclass of ``PathSimulator`` to run the
  transition state ensemble simulation.

Background Information
______________________

This module builds on OpenPathSampling, a Python package for path sampling
simulations. To learn more about OpenPathSampling, you might be interested in
reading:

* OPS documentation: http://openpathsampling.org
* OPS source code: http://github.com/openpathsampling/openpathsampling


Testing
_______

To test this module you need to download the source files package (see the ``Source Code`` section below) and install it using
``pip install -e .`` from the root directory of the package. 
In the ``ops_tse/tests`` folder type ``nosetests test_ops_tse.py`` to test the module using the `nose`_ package.

.. IF YOUR MODULE IS IN OPS CORE:

.. This module has been included in the OpenPathSampling core. Its tests can
.. be run by setting up a developer install of OpenPathSampling and running
.. the command ``nosetests`` from the root directory of the repository.

.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY


Examples
________

* An IPython 2-D toy example can be found in the ``examples`` directory of the the
  source files (see the ``Source Code`` section below). Open it using ``jupyter notebook simple_tse_example.ipynb`` 
  (see ``Jupyter notebook`` documentation at http://jupyter.org/ for more details)


Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

.. This module has been merged into OpenPathSampling. It is composed of the
.. following pull requests:

.. * link PRs

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

The source code for this module can be found in: https://gitlab.e-cam2020.eu/Classical-MD_openpathsampling/TSE/tree/master

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

