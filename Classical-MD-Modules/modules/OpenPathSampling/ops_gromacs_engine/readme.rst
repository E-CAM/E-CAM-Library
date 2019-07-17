.. _ops_gromacs_engine:

##################################
Gromacs engine in OpenPathSampling
##################################

.. sidebar:: Software Technical Information

  The information in this section describes OpenPathSampling as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Language
    Python (2.7, 3.6, 3.7)

  Documentation Tool
    Sphinx, numpydoc format (ReST)

  Application Documentation
    http://openpathsampling.org

  Relevant Training Material
    http://openpathsampling.org/latest/examples/

  Licence
    LGPL, v. 2.1 or later

  Authors
    David W.H. Swenson

.. contents:: :local:

This module adds support for Gromacs as an engine for OpenPathSampling.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

Different molecular dynamics (MD) codes have developed to serve different
communities. Gromacs is one of the major MD codes for the biomolecular
community, and even though much of its functionality can be reproduced by
other MD codes, such as OpenMM, there are still some extensions that are
built on top of Gromacs that haven't been ported to other codes. For
example, the MARTINI coarse-grained model is not available other codes such
as OpenMM.

Additionally, people who are familiar with a given MD package will prefer to
continue to work with that. Therefore codes that wrap around MD packages, as
OpenPathSampling does, can expand their reach by adding ways to interface
with other MD packages.

This module adds the Gromacs engine for OpenPathSampling. It is the first
practical test of the external engine API of OPS.

Specific functionality in this module includes:

* ``GromacsEngine``: the OPS dynamics engine, based on the
  ``ExternalEngine``, that runs Gromacs as an external tool. Option on
  initialization allow the user to customize the path to the Gromacs
  executable.
* ``ExternalMDSnapshot``: an OPS snapshot for external MD engines, which
  contains coordinates, velocities, and box vectors. Requires that the
  engine implement a ``read_frame_data`` method to load from a specific MD
  trajectory.
* ``snapshot_from_gro``: a function that creates an OPS snapshot from a
  Gromacs ``.gro`` file.


Background Information
______________________

This module builds on OpenPathSampling, a Python package for path sampling
simulations. To learn more about OpenPathSampling, you might be interested in
reading:

* OPS documentation: http://openpathsampling.org
* OPS source code: http://github.com/openpathsampling/openpathsampling


Testing
_______

.. IF YOUR MODULE IS IN OPS CORE:

.. This module has been included in the OpenPathSampling core. Its tests can
.. be run by setting up a developer install of OpenPathSampling and running
.. the command ``py.test`` from the root directory of the repository.

This module is in a development branch of OpenPathSampling. If you have
conda installed, this branch of OPS can be installed by downloading the
``conda_ops_dev_install.sh`` script and running it with the command:

.. code:: bash

    source conda_ops_dev_install.sh dwhswenson gromacs_engine

This will download a new copy of the OPS git repository, select the
``gromacs_engine`` branch from the ``dwhswenson`` fork, install the
requirements, and create an editable install of OPS. If you would like to do
this in a new conda environment, set the environment variable ``OPS_ENV``,
and it will install in a new environment with the name ``$OPS_ENV``.

To run tests, you may need ``pytest``, which can be installed with ``conda
install pytest``.

The entire OPS test suite can be run with run with ``py.test --pyargs
openpathsampling``. Tests specific to the Gromacs engine can be run with
``py.test --pyargs openpathsampling.tests.test_gromacs_engine``.

.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY

.. The tests for this module can be run by downloading its source code, 
.. installing its requirements, and running the command ``nosetests`` from the
.. root directory of the repository.

Examples
________

* An example can be found here: https://github.com/dwhswenson/openpathsampling/tree/gromacs_engine/examples/gromacs

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

.. This module has been merged into OpenPathSampling. It is composed of the
.. following pull requests:

This module is contained in the following pull request:

* https://github.com/openpathsampling/openpathsampling/pull/819

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

