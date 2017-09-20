.. _ops_snapshot_features_1:

##################################
OpenPathSampling Snapshot Features
##################################

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

This module includes several new OpenPathSampling snaphost "features,"
which make attributes directly accessible from the snapshot object. In
particular, this includes support for ``masses``, ``n_degrees_of_freedom``,
and ``instantaneous_temperature`` in both the toy and OpenMM engines.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

In OpenPathSampling, certain quantities can be accessed directly from each
snapshot. The standard examples of such "features" are things like
coordinates and velocities, which are stored for each snapshot. However,
additional features can also be added, which may not require per-snapshot
storage. This module makes several of those features available for snapshots
from the OpenMM engine and from the toy engine.

Included in this implementation are:

* ``masses``: was already available in toy, but now also available in
  OpenMM. Only stored once (in the ``engine``), but accessible from any
  snapshot.
* ``n_degrees_of_freedom``: added for both OpenMM and toy engines.
  Calculated on the fly.
* ``instantaneous_temperature``: added for both OpenMM and toy engines.
  Calculated on the fly (requires calculation of ``n_degrees_of_freedom``
  and of kinetic energy).

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

An example of these features in use can be found at:

* https://gitlab.e-cam2020.eu/dwhswenson/ops_additional_examples/blob/master/snapshot_features_1.ipynb
  [`HTML <https://nbviewer.jupyter.org/urls/gitlab.e-cam2020.eu/dwhswenson/ops_additional_examples/raw/master/snapshot_features_1.ipynb>`_]

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

This module has been merged into OpenPathSampling. It is composed of the
following pull requests:

.. * link PRs

* https://github.com/openpathsampling/openpathsampling/pull/579
* https://github.com/openpathsampling/openpathsampling/pull/589
* https://github.com/openpathsampling/openpathsampling/pull/649

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

