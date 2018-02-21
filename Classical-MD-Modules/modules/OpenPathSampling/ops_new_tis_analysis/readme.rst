.. _ops_new_tis_analysis:

####################
OPS New TIS Analysis
####################

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

This module provides a new framework for analyzing transition interface
sampling simulations using OpenPathSampling. The previous analysis tools
gave no flexibility to the user, were not easily extendable, and had no unit
tests. This module fixes all of that.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

Transition interface sampling (TIS) is a powerful rare events method with a
particular focus on calculating the rates of reactions. The core idea starts
by splitting the rate :math:`k_{AB}` into a product:

.. math::

   k_{AB} = \phi_{A_0} P_A(B|\lambda_0)

where :math:`k_{AB}` is the rate from state :math:`A` to state :math:`B`,
:math:`\phi_{A_0}` is the flux out of state :math:`A` and through an
interface :math:`\lambda_0`, and :math:`P_A(B|\lambda_0)` is the transition
probability of that a trajectory enters :math:`B` before any other state
given that has exited the interface :math:`\lambda_0`, starting in state
:math:`A`.

TIS further splits the transition probability into several conditional
probabilities, by adding a set of :math:`m` interfaces (surfaces in phase
space) :math:`\{\lambda_i\}`, with :math:`\lambda_0` as the innermost.
Mathematically, this gives us:

.. math::

   P_A(B|\lambda_0) = P_A(B|\lambda_m) \prod_{i=0}^{m-1}
                      P_A(\lambda_{i+1}|\lambda_i)

By sampling trajectories that necessarily cross each given interface
:math:`\lambda_i`, TIS provides the information that can be used to
determine :math:`P_A(\lambda_{i+1}|\lambda_i)`. However, there are several
approaches have been developed/proposed to efficiently turn the sampling data
into a best estimate of the transition probability.

The previous analysis in OPS took one of those method, and provided very
little room to customize the procedure. This module makes it so that it is
easier to customize the analysis or to use different approaches to calculate
the various terms that make up the TIS rate expression.

A much more detailed description of the TIS analysis as implemented here is
given in the core OPS documentation, which was also contributed as part of
this module. That section of the documentation is online at
http://openpathsampling.org/latest/topics/tis_analysis.html

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

This module has been included in the OpenPathSampling core. Its tests can
be run by setting up a developer install of OpenPathSampling and running
the command ``nosetests`` from the root directory of the repository.

.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY

.. The tests for this module can be run by downloading its source code, 
.. installing its requirements, and running the command ``nosetests`` from the
.. root directory of the repository.

Examples
________

Example in the OPS repository on using this: 
  
* https://github.com/openpathsampling/openpathsampling/blob/master/examples/toy_model_mstis/toy_mstis_A3_new_analysis.ipynb

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

This module has been merged into OpenPathSampling. It is composed of the
following pull request:

* https://github.com/openpathsampling/openpathsampling/pull/686

.. * link PRs

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

