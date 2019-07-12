.. _ost_example:

#################
OPS-based modules
#################

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
communities. For example, although 


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

This will download a new copy of the git repository, select the
``gromacs_engine`` branch from the ``dwhswenson`` fork, install the
requirements, and create an editable install of OPS. If you would like to do
this in a new conda environment, set the environment variable ``OPS_ENV``,
and it will install in a new environment with that the name ``$OPS_ENV``.

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

