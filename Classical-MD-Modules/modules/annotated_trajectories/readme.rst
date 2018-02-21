.. _annotated_trajectories:

######################
Annotated Trajectories
######################

.. sidebar:: Software Technical Information

  This module is based on OpenPathSampling. This section includes
  information both for the specific module and for OpenPathSampling as a
  whole.

  Language
    Python (2.7)

  Documentation Tool
    Sphinx, numpydoc format (ReST)

  Application Documentation
    http://openpathsampling.org

  Relevant Training Material
    http://openpathsampling.org/latest/examples/
    https://github.com/dwhswenson/annotated_trajectories/tree/master/examples

  Licence
    LGPL, v. 2.1 or later

.. contents:: :local:

Authors: David W.H. Swenson

This module provides a data structure that adds annotations to frames of a
trajectory, intended to label those frames as being, for example, in a
given metastable state. It also provides some tools to analysis whether a
proposed state definition is compatible with those annotations.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

When dealing with biomolecular systems, one of the common challenges is to
define the (meta)stable states. The existence of metastable states is easily
determined by visually inspecting the trajectory. However, identifying
geometric criteria to characterize the states remains difficult.

This module provides a data structure that allows the user to easily
annotate a trajectory with the visually identified states, and to compare
those annotations to proposed state definitions. It also includes tools to
visualize where the proposed state definition matches the annotations.

This implementation includes:

* An ``Annotation`` class, which is essentially a structure to connect the
  state label and a range of frames (marked with their beginning and ending
  frames) that the user identifies as in the given state.
* Another data structure, ``ValidationResults``, which contains the
  correctly identified frames, as well as false positives and false
  negatives, for a given proposed state definition.
* The ``AnnotatedTrajectory`` class, which associates the annotations with
  an OpenPathSampling ``Trajectory`` object and performs the analysis to
  compare the proposed states to those annotations.
* A method ``plot_annotated``, which plots the annotations and the proposed
  state definition in order to visually inspect the quality of the proposed
  state.

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

.. This module has been included in the OpenPathSampling core. Its tests can
.. be run by setting up a developer install of OpenPathSampling and running
.. the command ``nosetests`` from the root directory of the repository.

.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY

The tests for this module can be run by downloading its source code, 
installing its requirements (namely, OpenPathSampling), and running the
command ``nosetests`` from the root directory of the repository.

Once the requirements are installed, a standard installation of this package
can be done with ``python setup.py install``.

Examples
________

The features in this code, including the ability to save the annotations
associated with a trajectory, are highlighted in a Jupyter notebook in its
``examples/`` directory. `It can be viewed here.
<https://github.com/dwhswenson/annotated_trajectories/blob/master/examples/annotation_example.ipynb>`_

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

.. This module has been merged into OpenPathSampling. It is composed of the
.. following pull requests:

.. * link PRs

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

This module is for the 0.1 release of ``annotated_trajectories``. The source
code for this module can be found in:
https://github.com/dwhswenson/annotated_trajectories/releases/tag/v0.1.0

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

