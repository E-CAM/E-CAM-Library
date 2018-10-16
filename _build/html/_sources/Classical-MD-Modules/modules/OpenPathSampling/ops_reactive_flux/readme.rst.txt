.. _ops_rf:

#################################
Reactive flux in OpenPathSampling
#################################

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

Authors: Andreas Singraber

This module implements the reactive flux method in OpenPathSampling.

Purpose of Module
_________________

The reactive flux method in combination with a free energy calculation allows
to derive the rate constant of a rare event. This is accomplished by a shooting
algorithm similar to a committor analysis where fleeting trajectories starting
from the dividing surface are generated and statistics about their state with
respect to a collective variable is collected. There are many flavors of the
reactive flux method, this module implements the effective positive flux method
as described by van Erp and Bolhuis (see e.g.
http://dx.doi.org/10.1016/j.jcp.2004.11.003).

The implementation introduces the following new classes:

- ``ReactiveFluxSimulation`` inherits from ``ShootFromSnapshotsSimulation``
  and implements the shooting algorithm similar to ``CommittorSimulation``.
  First, backward trajectories from the initial snapshots are started and
  followed until they either hit state A or recross the dividing surface. In
  the latter case the trajectory is rejected. If instead the trajectory reaches
  A, a forward shot is performed until the trajectory reaches either A
  (rejected) or B (accepted). The forward trajectory is allowed to recross the
  barrier any number of times but must end up in B without reaching A. To
  implement this behaviour of a forward shot depending on the final state of
  the backward trajectory a ``NonCanonicalConditionalSequentialMover`` and the
  ``NonCanonicalConditionalSequentialMoveChange`` were derived from existing
  classes available in OpenPathSampling. The stable states, the dividing
  surface and other regions are identified via a user-defined reaction
  coordinate and resulting trajectories are saved in a ``Storage`` object.

- The class ``ReactiveFluxAnalysis`` provides functionality to analyze
  previously generated and stored trajectories similar to its parent class
  ``ShootingPointAnalysis``. In addition to trajectories the user needs to
  provide the gradient of the reaction coordinate at the dividing surface. With
  the stored velocities at the trajectory starting points it is possible to
  compute the time derivate of the reaction coordinate and therefore (together
  with results from a free energy calculation) derive the total flux and the
  flux for each initial snapshot. Methods to visualize e.g. per-snapshot
  results in 1D- and 2D-histograms are also provided.


Background Information
______________________

This module builds on OpenPathSampling, a Python package for path sampling
simulations. To learn more about OpenPathSampling, you might be interested in
reading:

* OPS documentation: http://openpathsampling.org
* OPS source code: http://github.com/openpathsampling/openpathsampling


Testing
_______

To test this module you need to download the source files package (see the ``Source Code`` section below) and install it using
``python setup.py install`` from the root directory of the package.
In the ``ops_rf/tests`` folder type ``nosetests testrfanalysis.py`` to test the module using the `nose`_ package.


.. IF YOUR MODULE IS IN OPS CORE:

.. This module has been included in the OpenPathSampling core. Its tests can
.. be run by setting up a developer install of OpenPathSampling and running
.. the command ``nosetests`` from the root directory of the repository.

.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY


Examples
________

See the ``rf-example.ipynb`` IPython notebook in the source directory, here is the direct link: https://gitlab.e-cam2020.eu/Classical-MD_openpathsampling/RF/blob/master/ops_rf/rf-example.ipynb
To run the example execute ``jupyter notebook rf-example.ipynb`` in your terminal.

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

.. This module has been merged into OpenPathSampling. It is composed of the
.. following pull requests:

.. * link PRs

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

The source code for this module can be found in: https://gitlab.e-cam2020.eu/Classical-MD_openpathsampling/RF/tree/master

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

