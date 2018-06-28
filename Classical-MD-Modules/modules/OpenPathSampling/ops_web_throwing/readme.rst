.. _ops_web_throwing:

################################
Web Throwing in OpenPathSampling
################################

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

Authors: Sander Roet, Anders Lervik, Enrico Riccardi 

This module implements the web throwing method in OpenPathSampling

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.
Web throwing is a Monte Carlo move in path space designed to improve the
efficiency of transition interface sampling (TIS). It consists in a smart selection of 
shooting points and shooting moves in respect with the super detailed balance. 
Thanks to web throwing, the new path generated, even if more computationally
expensive in respect with standard shooting, has a much lower correlation with
the source paths. The strategy thus can significantly reduce the computational
time required to study transition events and to quantify their rates. 
The web throwing algorithm is described by Riccardi, Dahlen, and van Erp
(http://dx.doi.org/10.1021/acs.jpclett.7b01617)

In summary, the web throwing method is an shooting method for transition
interface sampling, that decorrelates the trajectory between an interface
:math:`\lambda` and an associated surface of unlikely return 
:math:`\lambda_{\text{SOUR}}`. It does this by doing ``n_cycles`` of
Transition Path Sampling (TPS) from :math:`\lambda_{\text{SOUR}}` to :math:`\lambda`.
The first frame in this volume is selected for a forwards shot and the last
frame in this volume is selected for a backwards shot. After the ``n_cycles`` 
the new sub-trajectory is extended in both ways to satisfy the TIS ensemble.
The sampling efficiency is increased significantly if :math:`\lambda_{\text{SOUR}}`
and :math:`\lambda` are positioned before and after the barrier in the potential,
respectively.

The implementation introduces the following new classes:
* ``SecondFrameSelector`` inherits from ``ShootingPointSelector`` and selects
  the second frame (``index = 1``) from a trajectory.

* ``SecondToLastFrameSelector`` inherits from ``ShootingPointSelector`` and
  selects the second to last frame of a trajectory 
  (``index = len(trajectory)-2``).

* ``WebEnsemble`` inherits from ``SequentialEnsemble`` and implements the
  sampling ensemble between the ``Volume`` :math:`\lambda_{\text{SOUR}}` and 
  the ``Volume``  :math:`\lambda` in which the web throwing occurs.

* ``WebShootingMover`` inherits from ``RandomChoiceMover`` and implements the
  TPS shooting algorithm in the ``WebEnsemble`` using the
  ``SecondFrameSelector`` for the ``ForwardShootMover`` and the 
  ``SecondToLastFrameSelector`` for the ``BackwardShootMover``.

* ``WebCycleMover`` inherits from ``SequentialMover`` and implements 
  ``n_cycles`` sequential ``WebShootingMover`` in the ``WebEnsmeble``.

* ``WebExtendMover`` inherits from ``ConditionalSequentialMover`` and extend the
  sub trajectory after web throwing. It first simulates backwards to reach the
  initial TIS state. If this reaches the interface, the whole move is rejected.
  If this indeed finds the correct state the sub trajectory is extended in the
  forward direction to complete the TIS ensemble.

* ``WebThrowingMover`` inherits from ``SubPathMover`` and is the canonical mover
  for the web throwing algorithm. From a TIS ``ensemble`` it first selects a
  random sub-trajectory in the ``web_ensemble``. Then it calls the
  ``WebCycleMover`` with ``n_cycles``. Finally it calls the ``WebExtendMover`` to
  extend the sub-trajectory back into the TIS ``ensemble``. 

* ``WebThrowingStrategy`` inherits from ``MoveStrategy`` and it builds the
  ``WebThrowingMover`` for every ``{interface: lambda_sour}`` in the ``sours``
  dictionary. Every interface should have only 1 ``lambda_sour`` associated with
  it.

* ``WebThrowingScheme`` inherits from ``MoveScheme``. This is a ``MoveScheme``
  that only does web throwing. This is mostly a convenience class used in 
  examples and testing.


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
The tests for this module can be run by downloading its source code (see the
``Source Code`` section below), installing its requirements and installing it
by running ``python setup.py install`` from the root directory of the package.
Test this module by running the command ``nosetests`` from the root directory of
the repository.


Examples
________

All examples can be found in the ``examples`` directory
(https://gitlab.e-cam2020.eu/sroet/web_throwing/tree/master/examples).
Open the ipython notebook (files with the extension ``.ipynb``) of interest by
running ``jupyter notebook file_of_interest.ipynb`` in this directory, replacing
``file_of_interest.ipynb`` by the file you want to open. (see
``Jupyter notebook`` documentation at http://jupyter.org/ for more details)

The examples are

* An example on how to set up a multiple interface set TIS (MISTIS) simulation
  with only web throwing and the analysis of the web throwing moves can be found
  in ``mistis_using_only_webthrowing.ipynb``.

* An example on how to add the web throwing to a default MISTIS simulation can 
  be found in ``adding_webthrowing_to_mistis.ipynb``. This example also shows 
  how to change the selection weight of this move and some analysis.

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

.. This module has been merged into OpenPathSampling. It is composed of the
.. following pull requests:

.. * link PRs

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

The source code for this module can be found in:
https://gitlab.e-cam2020.eu/sroet/web_throwing/tree/master.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

