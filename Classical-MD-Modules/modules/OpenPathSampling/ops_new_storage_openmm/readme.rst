:orphan:

.. _ops_new_storage_openmm:

######################################
SimStore: Support for OpenMM Snapshots
######################################

.. sidebar:: Software Technical Information

  The information in this section describes OpenPathSampling as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Language
    Python (3.7, 3.8, 3.9)

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

This module adds support for OpenMM snapshots in SimStore, the new storage
subsystem used by OpenPathSampling.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

Previous modules have provided the core of the SimStore storage interface
for OPS, as well as integration for OPS simulations using either the
internal toy engine or using the Gromacs engine. However, one of the most
commonly used engines for OPS is OpenMM. Because OpenMM data carries
explicit units, it requires special techniques for storing. Additionally,
OpenMM snapshots in OPS are split such that the configurational components
can be reused for multiple initial velocities, which also requires special
treatment. This module adds those techniques, thus adding support for OpenMM
simulations in the new SimStore storage subsystem in OPS. SimStore is faster
than the current OPS storage, and is essential for the parallelization of
OPS.

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

.. This module has been included in the OpenPathSampling core. Its tests can
.. be run by installing pytest and OPS (with commit ????????, which will be
.. part of release ??? and later), and running the command ``py.test
.. --pyargs  openpathsampling``.

.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY

.. The tests for this module can be run by downloading its source code, 
.. installing its requirements, and running the command ``py.test`` from the
.. root directory of the repository.

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

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

This module has been merged into OpenPathSampling. It is composed of the
following pull request:

* https://github.com/openpathsampling/openpathsampling/pull/949

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _pytest: http://pytest.org/

