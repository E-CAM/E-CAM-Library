.. _ops_resampling_statistics:

#####################
Resampling Statistics
#####################

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

The module provides tools for resampling (e.g., statistical bootstrapping)
in the context of ``pandas`` DataFrames in general and specifically
OpenPathSampling. This provides tools for estimating statistical error on
multiple quantities simultaneously.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

Providing an estimate of uncertainty is essential when presenting scientific
results. This module provides the ability to perform statistical analysis of
a large simulation from OpenPathSampling by using the sampled trajectories
to create subsamples, which are then assumed to be independent. The
subsamples are analyzed separately, and this module makes it easy to obtain
mean, standard deviation, or percentile values. In particular, this module
provides the tools to do such an analysis on functions that return a table
of data using a ``pandas.DataFrame`` object, as the OpenPathSampling rate
matrix calculation does.

Most of the code is generic, and could be used for any function that
produces a ``pandas.DataFrame`` as its output. Therefore this module may
be useful for many projects other than OpenPathSampling. Within
OpenPathSampling, this can be used to obtain statistics on rates, fluxes,
and other such quantities.

These tools are implemented in two main classes. The first is
``BlockResampling``, which organizes the input (MC steps in OPS) into blocks
to be passed to a function that does the analysis. This allows us to obtain
several results for the analysis. The second is ``ResamplingStatistics``,
which takes those blocks and a function (that returns a
``pandas.DataFrame``) as input. It then applies that function to each of
those blocks, and then makes it easy to access properties such as the mean,
standard deviation, or percentile values for each frame element.

While ``BlockResampling`` is the only resampling method implemented in the
module (as it is the one needed for TIS rate calculations), it would be
straightforward to extend this framework with other resampling methods, such
as variants of bootstrapping.

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

* https://gitlab.e-cam2020.eu/dwhswenson/ops_additional_examples/blob/master/resampling_statistics.ipynb


Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

This module has been merged into OpenPathSampling. It is composed of the
following pull requests:

* https://github.com/openpathsampling/openpathsampling/pull/684

.. * link PRs

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

