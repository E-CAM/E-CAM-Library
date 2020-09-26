.. _ops_cli_core:

#########################
OpenPathSampling CLI Core
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
    http://openpathsampling-cli.readthedocs.io/

  Relevant Training Material
    http://openpathsampling.org/

  Licence
    MIT

  Software module developed by
    David W.H. Swenson

.. contents:: :local:

This module provides the core infrastructure for the OpenPathSampling
command line interface.

Purpose of Module
_________________

OpenPathSampling (OPS) is a powerful Python library for path sampling
simulations.  However, using it requires expertise in Python programming. In
order to make OPS more accessible to a broader audience, we have begun
development of a command line interface (CLI) to work with OPS files and to
run OPS simulations.

The usage model is that there is a single command, ``openpathsampling``,
which takes subcommands and delegates them to different routines. This is
the same model as, e.g., ``git``, which has subcommands like ``git commit``
and ``git clone``.

This module provides the underlying infrastructure/platform that the
specific CLI subcommands will be built upon. A subsequent module will
provide several specific subcommands.

Key functionality included here:

* **Parameter decorators**: Different subcommands will frequently use the
  same options and arguments. In order to maintain consistency in parameter
  usage and help statements, common parameters have been implemented as a
  set of reusable decorators.
* **Plug-in infrastructure**: To promote customizability, the OPS CLI uses a
  plug-in infrastructure. This allows us to ship a small set of default
  subcommands, but to allow users to easily create and share custom
  subcommands for their own projects or workflows. This module introduces
  two mechanisms by which a plug-in can register with the CLI. One is based
  on `native namespace packages`_, which makes it easy for developers to
  create and distribute plug-ins that automatically register with OPS when
  installed with standard Python tools.  The other approach is file-based,
  and simply involves placing the plug-in Python file in the user's
  ``~/.openpathsampling/cli-plugins/`` directory.  This approach is useful
  for quick development of plug-ins that may be intended to be shared within
  a specific research group or used to simplify reproducibility of workflows
  for a single project, but are not intended to be widely distributed and
  maintained for the long term.

.. _native namespace packages: https://packaging.python.org/guides/packaging-namespace-packages/#native-namespace-packages


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

This module deals with the underlying platform on which the rest of the OPS
CLI is built. As such, there are no direct examples. The concrete
implementation of specific commands in the OPS CLI will be the subject of a
second module.

Source Code
___________

The source code for the OpenPathSampling CLI can be found in the its
GitHub repository:  http://github.com/openpathsampling/openpathsampling-cli.

This module covers the "core" code for the OpenPathSampling CLI as of
release version 0.1.

Specifically, it includes the following files, and their associated test
suites:

* ``cli.py``
* ``param_core.py``
* ``parameters.py``
* ``plugin_management.py``

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

