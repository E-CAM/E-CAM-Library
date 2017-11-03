.. _ost_example:

#################
OPS-based modules
#################

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

Authors: Alan O'Cais 

This is the template for an E-CAM *module* based on OpenPathSampling (OPS). Several
sections are already pre-filled with the details of OPS. Please fill out the
rest in order to make your module. Feel free to add additional information as
well. This section should provide a very brief description of your module.

To add your module, please follow the instructions in ":ref:`contributing`" but create your module 
in a subdirectory of ``modules/OpenPathSampling`` and use a copy of 
:download:`this file <./ops_module_template.rst>` as a starting point.
Push your changes
back to GitLab and immediately open a merge request from your feature branch
against our repository. We can discuss your module in the merge request and
help you get it accepted.

For more details and the syntax for further functionality, see the generic example module in the
``modules/example_module/`` directory (:ref:`example`).

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.
Interface optimization is an important issue in TIS simulations. In general TIS procedure developed to perform TPS calculations on a separate ensembles, each is represented by its interface. With TIS algorithm it is possible obtain larger crossing probabilities and then collect the results. For more accurate results it is better to use a lot of interface, but for better performance the interfaces has to be optimized.


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

.. This module has been included in the OpenPathSampling core. Its tests can
.. be run by setting up a developer install of OpenPathSampling and running
.. the command ``nosetests`` from the root directory of the repository.

.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY

.. The tests for this module can be run by downloading its source code, 
.. installing its requirements, and running the command ``nosetests`` from the
.. root directory of the repository.

Examples
________

Link to examples, if you have one. May be a file within your source code.

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

