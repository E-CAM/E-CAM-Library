.. _ops_network_input:

###############################
Improved input for OPS networks
###############################

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

Authors: David W.H. Swenson

This module includes modifications to OpenPathSampling that simplify the
setup of transition networks, including providing a method so that extra
information about interfaces can (optionally) be provided on setup in order
to simplify analysis.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

In OpenPathSampling, "reactions" (which are often conformational changes)
are represented by objects called ``Transitions``, and the set of all
reactions of interest is represented by a ``TransitionNetwork``. The
``TransitionNetwork`` knows about all the reactions being sampled, as well
as the path ensembles used to sample them. In general, a full reaction
network might involve hundreds of path ensembles, so the
``TransitionNetwork`` is a factory that creates the ensembles so the user
doesn't have to, and also provides conveniences for analysis, such as
grouping the path ensembles according to the reaction they sample.

This module deals with two aspects of transition interface sampling methods.
The first is the interface set, which is the group of interfaces associated
with a given transition. These interfaces are associated with volumes in
phase space. Those volumes are typically defined by a maximum value of some
order parameter, :math:`\lambda`. Knowing this edge value is essential for
calculating rates.

The second aspect is the multiple state outer (MS-outer) interface ensemble.
This is an ensemble used in some variants of multiple state transition
interface sampling to facilitate replica exchange between paths with
different initial states. In practice, this approach is likely to become
less frequently used (there are more efficient approaches to achieve the
same goals), so requiring that the MS-outer interface exist is not very
forward-thinking, although removing entirely is also not the best approach.

Prior to the improvements made in this module, OpenPathSampling suffered from
the following problems:

* An interface had no way of knowing what its "edge" value was, only whether
  a given snapshot was inside it or outside it. This made it difficult to
  automatically determined the value at the outermost interface for
  calculating the rate. The previous code relied on a hack that assumed that
  trajectories in the interface had a relatively low probability of crossing
  to another state.
* All networks required a multiple state outer interface, even if it wasn't
  used. This also meant that the outermost interface a user defined was
  converted to an MS-outer interface, which could lead to confusion. This
  module makes usage much easier to understand.

This module changes the setup of interface sets, such that they can identify
their edge values (if it is uniquely identified; the code still works if it
is not unique). This obviates the need for an ugly hack to guess where the
boundary was.

This module also changes the way that the multiple state outer interface is
set up. Now the user must explicitly make a multiple state outer interface
object, which will then make the appropriate MS-outer ensemble. We wanted to
keep the ability to have an MS-outer ensemble. However, we did not want to
require it, because there are, in general, better approaches to accomplish
the same things.

The primary new objects in this module are:

* ``InterfaceSet``: replace the previous list of ``Volume`` objects with a
  proper set of interfaces, which can be associated with a list of
  ``lambdas``. 
* ``VolumeInterfaceSet``: subclass of ``InterfaceSet``, intended to directly
  replace the old functions to create several volumes at once. Also has the
  ability to automatically create a new interface based on the new value of
  the maximum lambda. This also makes it potentially useful in methods where
  the interfaces should be treated parametrically, such as adaptive
  multilevel splitting.
* ``MSOuterTISInterface``: object to create the multiple state outer
  interface for transition interface sampling. Mainly allows us to smoothly
  transition away from using this sort of object, since there are better
  approaches to solve the same problems.

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

The first example demonstrates how to use the new classes that have been
incorporated in OpenPathSampling through this module. This includes some of
the new features of interface sets, such as identifying values of lambda and
creating new interfaces based on a desired "edge" value, as well as a couple
approaches to building an MS-outer interface, and how to build a
``TransitionNetwork`` with or without MS-outer interfaces.

* https://gitlab.e-cam2020.eu/dwhswenson/ops_additional_examples/blob/master/network_input.ipynb [`HTML <https://nbviewer.jupyter.org/urls/gitlab.e-cam2020.eu/dwhswenson/ops_additional_examples/raw/master/network_input.ipynb>`_]

The example below links to the official OpenPathSampling documentation. The
notebooks that make up that example can also be found in the
OpenPathSampling GitHub repository.  Note that this example represents the
most recent version of code, and may not be identical to what was included
in the module. The original example is included in the source code, below.
In this second example, usage of this module is illustrated in the context
of a larger example of MSTIS.
 
* `Multiple State TIS on a Toy Model
  <http://openpathsampling.org/latest/examples/mstis.html>`_ 

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

This module has been merged into OpenPathSampling. It is composed of the
following pull requests:

* https://github.com/openpathsampling/openpathsampling/pull/528
* https://github.com/openpathsampling/openpathsampling/pull/530
* https://github.com/openpathsampling/openpathsampling/pull/538
* https://github.com/openpathsampling/openpathsampling/pull/553

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

