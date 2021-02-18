:orphan:

.. _ops_new_storage:

############################################
SimStore: OPS New Storage Subsystem (part 1)
############################################

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

This module provides the core of SimStore, a new storage subsystem for
OpenPathSampling, which is more flexible and has better performance than the
older storage system. This module is also necessary to serialize data for
transfer over a network, as is needed for parallelization across multiple
nodes.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

OpenPathSampling has the following needs from a storage subsystem:

1. In addition to data objects (data created by the simulation), the
   simulation objects (i.e., the details of how the simulation was run)
   should be stored. This helps track provenance and enhances
   reproducibility.
2. All objects should have unique universal identifiers (UUIDs). References
   in data objects to the UUIDs of the simulation objects that generated
   them are important for provenance and reproducibility.
3. Because users may add new simulation objects, storing simulation objects
   should be very general, and should place minimal burden on the users who
   create new simulations objects.
4. The results from functions that are calculated during a simulation should
   be stored in such a way that they can be retrieved again, instead of
   recalculating them.
5. Path sampling generates large quantities of data. Because of this, and
   because analysis is frequently done in layers (e.g., one can perform the
   TIS rate analysis without reloading any coordinate information), it
   should be possible to reload some information without reloading the
   entire object.
6. Storage of simulation objects should be (nearly) human readable. While
   there are some exceptions to human readability, it is important that, for
   the most part, simulation parameters can be read and interpreted if a
   user wishes to load OPS data in, e.g, another programming language.

This set of requirements was met by the previous storage subsystem,
``netcdfplus``. However, ``netcdfplus`` was written in a recursive style,
which means that every load from disk was a separate request. This made
``netcdfplus`` very slow. Additionally, the storable function results (#4
above) were written in a way that was not compatible with parallelization.
Finally, the base storage class of ``netcdfplus`` inherits from ``netcdf4``,
meaning that it was tied to a single backend.

With this module, we introduce SimStore, which is being added as an
experimental module in OpenPathSampling, with the intent of replacing
``netcdfplus`` in OPS 2.0. SimStore will have all the same features, with
better performance, more flexibility for users and developers, and a design
that is prepared for parallelization. Until version 2.0, both storage
subsystems will coexist in the OPS library.

This module, in particular, provides the core storage capabilities (#1, #3,
and #6 above) and the proxy-based lazy loading (#5). The UUIDs (#2) and the
are still provided by ``netcdfplus``. A future module will address the
problem of storing function results (#4). Importantly, the API for flexible
storage of general simulation objects (#3) remains the same as in
``netcdfplus``, facilitating the transition to SimStore.

Some of the specific functionality covered includes:

* **SQL backend**: The first backend for SimStore is SQL, defaulting to
  sqlite3. However, in principle, it should be nearly trivial to use a MySQL
  or PostgreSQL instance instead, which would be suitable for parallel
  usage.
* **Schema-based storage**: The description of data objects, which does not
  vary much for a given application, is provided by a human-readable schema,
  including specification of what objects should be loaded as lazy proxies.
  This makes it easy for users or developers to see and understand the
  overall data model.
* **Dynamic registration of new tables**: Some aspects of the data model do
  not vary for a certain application (e.g., in OpenPathSampling,
  trajectories are always lists of snapshots). However, some aspects do
  depend on the specific use case (e.g., in OpenPathSampling, the size of
  the coordinates array depends on the specific molecular system being
  studied). SimStore allows dynamic registration of tables in order to
  create new tables of the correct size for, e.g., snapshots coordinates.
* **Extensible JSON-based simulation object serialization**: In order to
  create a (mostly) human readable description of simulation objects,
  SimStore (like ``netcdfplus``) uses JSON. However, some simulation objects
  from outside packages (e.g., instances of ``simtk.Quantity``, which pair a
  value with a unit, and are used in OpenMM) require custom serialization.
  In ``netcdfplus``, that custom serialization was inside a ``netcdfplus``
  function, and extension by the user required editing the ``netcdfplus``
  code. SimStore uses a simple registration protocol so that new custom JSON
  serialization can be provided by the user without digging into the
  internals.

This module only deals with the generic and reusable aspects of SimStore.
Integration of SimStore and OpenPathSampling will be the subject of a future
module.

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

Then just run the ``test-storage.sh`` script in that repository.

Examples
________

An example for this module can be found at:

* https://github.com/dwhswenson/ops-storage-notebooks/blob/master/examples/02_load_old_cvs.ipynb

Source Code
___________

This module includes the general SimStore components of the pull request at: https://github.com/openpathsampling/openpathsampling/pull/928.
In particular, this module is for the files in the
``openpathsampling.experimental.storage.simstore`` subpackage within that
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

