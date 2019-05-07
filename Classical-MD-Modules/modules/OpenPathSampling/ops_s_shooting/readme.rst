.. _ost_s_shooting:

##############################
S-shooting in OpenPathSampling
##############################

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

This module implements the S-shooting method [1]_ in OpenPathSampling.

Purpose of Module
_________________

S-shooting [1]_ is a recently developed method to determine rate constants of
rare events. It is similar in spirit to the reactive flux method but its
relaxed requirements help to overcome practical problems. The method is based
on a simple shooting algorithm where trajectories are propagated forward and
backward in time for a fixed number of timesteps. The starting points need to
be provided and must lie in the saddle point region. This so-called S region
(hence the name S-shooting) is defined via a suitable reaction coordinate and
must to separate the stable states A and B in such a way that no trajectory can
connect A with B without visiting S. In contrast to the reactive flux method
the time derivative of the reaction coordinate is not required, which makes
this approach applicable to systems exhibiting diffusive dynamics along the
reaction coordinate. The S-shooting method can also be applied if the initial
shooting points are taken from a biased simulation. Thus, it is a natural
follow-up to free energy calculations like umbrella sampling and, in
combination with free energy curves, allows the computation of rate constants.

The implementation of the S-shooting method in OpenPathSampling (OPS) is split
into two main parts:

- Forward and backward trajectories started from initial snapshots are
  harvested  and glued together calling the ``SShootingSimulation`` class. The
  user needs to provide the initial snapshots, a suitable definition of the
  S region and the desired trajectory length.

- The S-shooting analysis is performed upon calling the ``SShootingAnalysis``
  class. Mandatory arguments include the definition of the stable states (A and
  B) and of the S region. In case the initial snapshots are taken from a biased
  simulation a bias function may be provided as an optional argument.

This module comes also with an IPython example notebook demonstrating the
method by applying it to a one-dimensional system (a brownian walker in a
double-well potential).

.. [1] Menzl, G., Singraber, A. & Dellago, C. S-shooting: a Bennett–Chandler-like method for the computation of rate constants from committor trajectories. Faraday Discuss. 195, 345–364 (2017), https://doi.org/10.1039/C6FD00124F

Background Information
______________________

This module builds on OpenPathSampling, a Python package for path sampling
simulations. To learn more about OpenPathSampling, you might be interested in
reading:

* OPS documentation: http://openpathsampling.org
* OPS source code: http://github.com/openpathsampling/openpathsampling


Testing
_______

Follow these steps to test the module:

1. Download and install OpenPathSampling (see http://openpathsampling.org/latest/install.html).

   .. caution::

      This module has been developed alongside a specific OPS version available at
      that time. If incompatibilities arise as OPS is further enhanced, please use
      version 0.9.5 available here:
      https://github.com/openpathsampling/openpathsampling/releases/tag/v0.9.5 .

2. Install the `nose`_ package.

3. Download the source files of the module (see the `Source Code`_ section below).

4. Install the module: change to the ``S-Shooting`` directory and run ``python setup.py install``.

5. Run the tests: execute ``nosetests`` in the ``S-Shooting`` directory.

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

See the ``sshooting-example.ipynb`` IPython notebook in the source directory, here is the direct link: https://gitlab.e-cam2020.eu/singraber/S-Shooting/blob/master/ops_s_shooting/sshooting-example.ipynb
To run the example execute ``jupyter notebook sshooting-example.ipynb`` in your terminal.

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

.. This module has been merged into OpenPathSampling. It is composed of the
.. following pull requests:

.. * link PRs

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

The source code for this module is located here:
https://gitlab.e-cam2020.eu/singraber/S-Shooting

.. tip::

   Ultimately, this module will be merged into the official OPS code. Check
   the status of the corresponding pull request here:
   https://github.com/openpathsampling/openpathsampling/pull/787 .

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

