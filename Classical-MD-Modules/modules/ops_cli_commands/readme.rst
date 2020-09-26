.. _ops_cli_commands:

#############################
OpenPathSampling CLI Commands
#############################

.. sidebar:: Software Technical Information

  The information in this section describes OpenPathSampling as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Language
    Python (3.6+)

  Documentation Tool
    Sphinx, numpydoc format (ReST)

  Application Documentation
    http://openpathsampling-cli.readthedocs.io/

  Relevant Training Material
    http://openpathsampling.org/

  Licence
    MIT

  Software module developed by
    David W.H. Swenson

.. contents:: :local:

This module provides the commands available to the OpenPathSampling command
line interface as of version 0.1.

Purpose of Module
_________________

As discussed in the :ref:`ops_cli_core` module, a command line interface
will make path sampling easier for a broader range of users. The OPS CLI is
initially designed to support a few functions that will be most useful to
users. More commands will be added over time, but the initial commands are
outlined below.

Simulation commands
~~~~~~~~~~~~~~~~~~~

* ``visit-all``: create initial trajectories by running MD until all states
  have been visited (works for MSTIS or any 2-state system); must provide
  states, engine, and initial snapshot on command line
* ``equilibrate``: run equilibration for path sampling (until first
  decorrelated trajectory); must provide move scheme and initial conditions
  on the command line
* ``pathsampling``: run path sampling with a given move scheme (suitable for
  custom TPS schemes as well as TIS/RETIS); must provide move scheme,
  iniital conditions,  and number of MC steps on command line

Commands for dealing with files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* ``contents``: list all the named objects in an OPS storage, organized by
  store (type); this is extremely useful to get the name of an object to use
* ``append`` : add an object from once OPS storage into another one; this is
  useful for getting everything into a single file before running a
  simulation

Background Information
______________________

This module builds on OpenPathSampling, a Python package for path sampling
simulations. To learn more about OpenPathSampling, you might be interested in
reading:

* OPS documentation: http://openpathsampling.org
* OPS source code: http://github.com/openpathsampling/openpathsampling

Detailed documentation on the CLI can be found at
https://openpathsampling-cli.readthedocs.io/

Testing
_______

The OPS CLI can be installed with either pip or conda:

.. code:: bash

   pip install openpathsampling-cli
   # or
   conda install openpathsampling-cli

Tests in the OpenPathSampling CLI use `pytest`_. The requirements for
testing are ``pytest`` and ``nose``, both of which can also be installed
with either ``pip`` or ``conda``.

With the package and its testing tools installed, tests can be run with:

.. code:: bash

   py.test --pyargs paths_cli

Examples
________


Source Code
___________

The source code for the OpenPathSampling CLI can be found in the its
GitHub repository:  http://github.com/openpathsampling/openpathsampling-cli.

This module covers the "core" code for the OpenPathSampling CLI as of
release version 0.1.

Specifically, it includes the following files, and their associated test
suites:

* ``commands/append.py``
* ``commands/contents.py``
* ``commands/equilibrate.py``
* ``commands/pathsampling.py``
* ``commands/visit_all.py``


.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

.. This module has been merged into OpenPathSampling. It is composed of the
.. following pull requests:

.. * link PRs

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _pytest: http://pytest.org/

