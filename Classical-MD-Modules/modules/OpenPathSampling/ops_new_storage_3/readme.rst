:orphan:

.. _ops_new_storage_3:

#########################
OPS New Storage Subsystem
#########################

.. sidebar:: Software Technical Information

  The information in this section describes OpenPathSampling as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Language
    Python (3.6+)

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

This module interfaces OpenPathSampling with the underlying storage
subsystem designed for SimStore.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

The motivation for this is described in depth in the module
:ref:`ops_new_storage`. That module focused on the underlying, reusable
library SimStore that was developed to meet the storage needs of
OpenPathSampling. This module represents the software that integrates the
generic code with OPS.

In particular, this includes:

* Design of the OPS serialization schema
* Custom handling of some objects, such as snapshots, for which the
  dimensionality is only known at runtime.
* Workarounds so that some objects could be stored.
* Custom subclass of the storable functions introduced in
  :ref:`ops_new_storage_2` that meet the requirements of OpenPathSampling.
  For example, when a collective variable is calculated on a trajectory, it
  should return a list with the value for each snapshot within the
  trajectory.


Background Information
______________________

This module builds on OpenPathSampling, a Python package for path sampling
simulations. To learn more about OpenPathSampling, you might be interested in
reading:

* OPS documentation: http://openpathsampling.org
* OPS source code: http://github.com/openpathsampling/openpathsampling


Installation and Testing
________________________

This was included in the version 1.4 release of OpenPathSampling. 
It can be installed via the ``conda`` package manager with:

.. code:: bash

    conda install -c conda-forge openpathsampling

In addition to previous OPS requirements, this module requires SQLAlchemy,
and other parts of the new storage require Dill.
These can be installed with, e.g., ``conda install -c conda-forge
sqlalchemy dill``.

The tests for this module are split between unit tests included in the
OpenPathSampling repository and integration tests in a separate repository.
The easiest way to run both sets of tests is to download or clone the
integration test repository at
https://github.com/dwhswenson/ops-storage-notebooks. Install the required
testing software, e.g., with:

.. code:: bash

    conda install -c conda-forge pytest pytest-cov nbval

Then just run the ``test-storage.sh`` script in that repository. Note:
although the module will work with Python 3.6+, some of the notebook tests
are not compatible with more recent versions of Python, so the tests should
be run with Python 3.7.

Examples
________

An example for this module can be found at:

* https://github.com/dwhswenson/ops-storage-notebooks/blob/master/examples/02_load_old_cvs.ipynb

Source Code
___________

This module includes the general SimStore components of the pull request at: https://github.com/openpathsampling/openpathsampling/pull/928.
In particular, this module is for the files in the
``openpathsampling.experimental.storage`` subpackage within that
pull request.

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

.. This module has been merged into OpenPathSampling. It is composed of the
.. following pull requests:

.. * link PRs

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

