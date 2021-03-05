.. _ops_progress_meters:

###############################
Progress meters in OPS analysis
###############################

.. sidebar:: Software Technical Information

  The information in this section describes OpenPathSampling as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Language
    Python (2.7, 3.6, 3.7, 3.8)

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

This module adds a general framework for progress meters within the analysis
tools of OpenPathSampling, as well as adding progress meters for several
specific analysis functions.

Purpose of Module
_________________

Some analysis in OpenPathSampling can take a significant amount of time to
perform. Previously, users had no feedback on how long such an analysis
would take, which can be a frustrating experience.

This module adds a standardized and straightforward approach to progress
meters in OPS analysis functions. In addition, it includes these progress
meters in several OPS analysis tools.

The general and reusable approach
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The primary goals of the OPS progress meter approach are to make it easy
for contributors to add progress meters to their analysis tools and to make
it flexible with regards to the progress meter used, especially making it
easy to silence the output if desired.

Therefore, the approach used here is to create a mix-in class that provides
a ``progress`` property, which wraps around an iterable to provide a
progress bar on that iterable.  The progress meter was designed to wrap
around the widely-used package |tqdm|_, and much of the API mimics the
``tqdm`` API.

.. |tqdm| replace:: ``tqdm``
.. _tqdm: https://github.com/tqdm/tqdm

The ``progress`` property can be set with the string ``'tqdm'`` to use
``tqdm`` or ``'silent'`` to not output anything. In addition, it can be
further customized by the user by setting it to a function that takes
keyword ``desc`` (a string description) and ``leave`` (a boolean indicating
whether the progress bar should remain after completion) and returns a
closure that takes an iterable and yields the elements of that iterable.

In order to use the progress meter in an analysis tool, a developer must
simply inherit from the mix-in ``openpathsampling.progress.SimpleProgress``
and wrap the appropriate iterable with ``self.progress``, i.e., a loop
``for step in steps`` becomes ``for step in self.progress(steps)``.

If ``tqdm`` is present in the user's environment, the default behavior is to
use ``tqdm``. If it is not present, the progress meter is silent.

Specific implementations
~~~~~~~~~~~~~~~~~~~~~~~~

This has been implemented for several OPS analysis classes. Straightforward
implementation have been performed for:

* ``ShootingPointAnalysis``
* ``PathHistogram``
* ``MoveAcceptanceAnalysis``

It has also been implemented as part of the OPS TIS analysis subpackage.
This implementation was less straightforward, as progress in this analysis
contains nested loops. However, it has been implemented for:

* ``MultiEnsembleSamplingAnalyzer``, the base class for many TIS analysis
  classes
* ``StandardTISAnalysis`` has significant customized work to use the new
  progress bars


Background Information
______________________

This module builds on OpenPathSampling, a Python package for path sampling
simulations. To learn more about OpenPathSampling, you might be interested in
reading:

* OPS documentation: http://openpathsampling.org
* OPS source code: http://github.com/openpathsampling/openpathsampling


Testing
_______

OpenPathSampling can be installed with either pip or conda:

.. code:: bash

    pip install openpathsampling
    # or
    conda install -c conda-forge openpathsampling

Tests in OpenPathSampling use `pytest`_. The requirements for testing are
``pytest`` and ``nose``, both of which can be installed with either ``pip``
or ``conda``.

With the package and its testing tools installed, tests can be run with:

.. code:: bash

    py.test --pyargs openpathsampling

.. IF YOUR MODULE IS IN OPS CORE:

.. This module has been included in the OpenPathSampling core. Its tests can
.. be run by installing pytest and OPS (with commit ????????, which will be
.. part of release ??? and later), and running the command ``py.test
.. --pyargs  openpathsampling``.

.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY

.. The tests for this module can be run by downloading its source code, 
.. installing its requirements, and running the command ``py.test`` from the
.. root directory of the repository.

Examples
________

This will affect many existing analysis examples. OPS examples can be found:

* In the documentation: http://openpathsampling.org/latest/examples/index.html
* On GitHub: https://github.com/openpathsampling/openpathsampling/tree/master/examples

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

This module has been merged into OpenPathSampling. It is composed of the
following pull requests:

* https://github.com/openpathsampling/openpathsampling/pull/882
* https://github.com/openpathsampling/openpathsampling/pull/895
* https://github.com/openpathsampling/openpathsampling/pull/902
* https://github.com/openpathsampling/openpathsampling/pull/906

All of the functionality in this module will be included in OpenPathSampling
1.3.

.. * link PRs

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _pytest: http://pytest.org/

