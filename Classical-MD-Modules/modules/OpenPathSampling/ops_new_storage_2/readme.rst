:orphan:

.. _ops_new_storage_2:

#########################
SimStore: New 
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
    MIT

.. contents:: :local:

Authors: David W.H. Swenson

This module adds "storable functions" to SimStore, the new storage subsystem
for OpenPathSampling. Storable functions cache the results of previous
calculations to disk. This new implementation will support future
parallelization approaches.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

Trajectory-based methods to study rare events, such as transition path
sampling (TPS), frequently require calculation of some collective variables
during the simulation. In some cases, these collective variables can be
relatively expensive to calculate, and my be calculated hundreds of
thousands of times during simulation.

For some types of simulations, such as the one-way shooting variable in TPS,
parts of trajectories can be reused, making it advantageous to store the
results of collective variables in memory. Furthermore, those same
collective variables are frequently used in analysis, make it advantageous
to store the results to disk.

This module introduces the parts of SimStore that manage that storage. This
includes the ``StorableFunction`` class itself, which wraps around a
user-defined function and handles caching results in memory, and looking up
results cached to disk. The user-defined function must take a data object
(such as a snapshot or a trajectory), which has a unique universal
identifier (UUID), and must return the same value every time it operates on
the same input (i.e., it must be a "pure" function).

A ``StorableFunction`` can be used in different modes: in ``'analysis'``
mode, it first searches the memory cache, then the disk storage, then
finally evaluates the internal function. In ``'production'`` mode, it first
searches the memory cache, then evaluates the function. Finally, in
``'no-caching'`` mode, it always evaluates the internal function.

One of the challenges in designing the new storable function infrastructure
was to ensure that it would be compatible with parallelization. This module
includes functionality so that the memory caches from different remote
workers can be returned with the other results, and combined into a master
memory cache of the process that also stores results to disk.

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

