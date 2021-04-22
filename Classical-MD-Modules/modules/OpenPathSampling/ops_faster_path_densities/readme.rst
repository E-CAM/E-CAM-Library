
.. _ops_faster_path_densities:

###################################
Faster Path Density Analysis in OPS
###################################

.. sidebar:: Software Technical Information

  The information in this section describes OpenPathSampling as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Language
    Python (2.7, 3.7, 3.8, 3.9)

  Documentation Tool
    Sphinx, numpydoc format (ReST)

  Application Documentation
    http://openpathsampling.org

  Relevant Training Material
    http://openpathsampling.org/latest/examples/

  Licence
    MIT

  Software module developed by
    David W.H. Swenson

.. contents:: :local:

A previous module introduced a path density analysis in OpenPathSampling.
However, the interpolation scheme used in that was rather slow. This module
makes it so that users can change interpolation schemes, and introduces two
faster options.


Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

As discussed in the module :ref:`ops_path_density`, the path density is a
useful tool for analyzing mechanisms in transition path sampling. One of the
features of the path density, which distiguishes if from a frame-based
density, is that it uses interpolation over the trajectory. That is,
histogram bins that are traversed are included, even if no snapshot falls in
them.

The interpolation approach introduced in the previous module worked by
subdividing intervals to find all bins that are crossed. This approach is
exact, but slow. This module refactors the path density so that the
interpolation algorithm can be provided by the user, and also provides two
new (and faster) interpolation approaches:

* ``BresenhamInterpolation``: Interpolation using the `Bresenham line
  drawing algorithm
  <https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm>`_
* ``BresenhamLikeInterpolation``: An interpolation scheme similar to
  Bresenham, but using floats instead of integers.


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

This module has been included in the OpenPathSampling core as of version
1.1. Its tests can be run by installing pytest and OPS, and running the
command ``py.test --pyargs  openpathsampling``.

.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY

.. The tests for this module can be run by downloading its source code, 
.. installing its requirements, and running the command ``py.test`` from the
.. root directory of the repository.

Examples
________

An example for path densities can be found at:

* https://github.com/openpathsampling/openpathsampling/blob/master/examples/misc/tutorial_path_histogram.ipynb

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

This module has been merged into OpenPathSampling. It is composed of the
following pull request:

* https://github.com/openpathsampling/openpathsampling/pull/875

.. * link PRs

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _pytest: http://pytest.org/

