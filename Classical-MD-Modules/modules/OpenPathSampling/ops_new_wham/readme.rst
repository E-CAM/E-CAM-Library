.. _ops_new_wham:

#############
New WHAM code
#############

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

This module includes a re-write of the OpenPathSampling reweighted histogram
analysis code. This fixes limitations and is more readable than the previous
version.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

The weighted histogram analysis method (WHAM) is one of the ways that the
results from the multiple ensembles in TIS can be combined to provide a
total result. The existing WHAM code in OPS was poorly documented,
untested, and required certain assumptions that are not guaranteed for many
kinds of path sampling, in particular that all ensembles had the same
number of samples. This module fixed all of that.

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

An example of how to use this code can be found at:

* https://gitlab.e-cam2020.eu/dwhswenson/ops_additional_examples/blob/master/wham.ipynb

Further cases where this has been used are implicit in the analysis
notebooks in OpenPathSampling.

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

This module has been merged into OpenPathSampling. It is composed of the
following pull requests:

.. * link PRs

* https://github.com/openpathsampling/openpathsampling/pull/541

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

