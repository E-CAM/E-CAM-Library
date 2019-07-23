.. _ops_spring_shooting:

###################################
Spring Shooting in OpenPathSampling 
###################################

.. sidebar:: Software Technical Information

  The information in this section describes OpenPathSampling as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Language
    Python (2.7, 3.6, 3.7)

  Documentation Tool
    Sphinx, numpydoc format (ReST)

  Application Documentation
    http://openpathsampling.org

  Relevant Training Material
    http://openpathsampling.org/latest/examples/

  Licence
    LGPL, v. 2.1 or later

.. contents:: :local:

Authors: Sander Roet

This module implements the spring shooting method in OpenPathSampling

Purpose of Module
_________________
.. ideal case, non-ideal situation, with the reason

Transition path sampling is most efficient when paths are generated from 
the top of the free energy barrier. However, complex (biomolecular) activated
processes, such as nucleation or protein binding/unbinding, can have asymmetric
and peaked barriers. Using uniform selection on these type of processes will not
be efficient, as it, on average, results in selected points that are not on the
top of the barrier. Paths generated from these points have a low acceptance
probability and accepted transition paths decorrelate slowly, resulting in a
low overall efficiency. 
Spring shooting was developed to increase the efficiency of path sampling of
these types of barriers,  without any prior knowledge of the barrier shape. The
spring shooting algorithm uses a shooting point selector that is biased with a
spring potential. This bias pulls the selection of points towards the transition
state at the top of the barrier. The paths that are generated from points
selected by this biased selector therefore have an increased acceptance
probability and decorrelation between accepted transition paths is also
increased. This results in a higher overall efficiency. The spring shooting
algorithm is described by Brotzakis and Bolhuis 
(http://dx.doi.org/10.1063/1.4965882).

In summary, the spring shooting selection algorithm is a selector for the  
one-way shooting method for transition path sampling, which uses a bias. This
bias is of the shape :math:`\min[1, e^{s\kappa\Delta\tau}]`. Where :math:`s = -1` for
forward shooting and :math:`s = 1` for backward shooting, :math:`\kappa` is the
given spring constant and :math:`\Delta\tau = \tau^{\prime} - \tau` is the
number of shifted frames of the new shooting point :math:`\tau^{\prime}`
compared to the previous accepted shooting point :math:`\tau`. The choice of
:math:`\tau^{\prime}` is limited to the interval 
:math:`[-\Delta\tau_{max}, \Delta\tau_{max}]`. The shooting move is rejected if
a :math:`\tau^{\prime}` is selected that is outside of the current path and is
accepted if the trajectory satisfies the path ensemble.

The main difference of this module compared to the paper is that instead of
using a rejection algorithm to sample from the correct distribution, the correct
distribution is sampled directly.

The implementation introduces the following new classes:

* ``SpringShootingSelector`` inherits from ``ShootingPointSelector`` and
  implements the shooting point selection, using a bias of the shape 
  :math:`\min[1, e^{s\kappa\Delta\tau}]`. At initialization it also takes an
  ``initial_guess`` as the initial reference point. This will default to 
  ``floor(len(trajectory)/2)``. To correctly keep track of the 
  history the selector has to be the same instance for the forward and 
  backward mover. 
  The ``pick`` function has to be called with a direction in 
  order to be able to use the correct value for :math:`s`. The selector then 
  draws a random number in the range :math:`[0,1)`, multiplies it with the
  sum of the biases. It the sums the biases from :math:`-\Delta\tau_{max}` to 
  :math:`\Delta\tau_{max}` and returns the index when this sum is bigger than the 
  random number.
  The ``restart_from_step`` function makes it possible to restart the selector
  at a specific step. It takes an ``MCStep`` and reconstructs the correct
  history from the ``MoveDetails``.

* ``SpringMover`` inherits from ``EngineMover`` and is the parent class for
  the ``ForwardSpringMover`` and ``BackwardSpringMover`` classes. It calls the 
  ``selector.pick`` function with ``self.direction`` to get the correct shooting
  point. It also build adds the needed ``MoveDetails`` in order to be able to 
  restart the selector. If an invalid snapshot has been chosen it will not
  run dynamics but will build a ``Sample`` with an acceptance probability of 
  0.0.

* ``SpringShootingMover`` inherits from ``RandomChoiceMover`` and behaves 
  similar, except it takes the extra arguments: ``delta_max``, ``k_spring`` and
  ``initial_guess``. These are then used to make the ``SpringShootingSelector``
  and this selector is given to both the forward and backward mover.

* ``SpringShootingStrategy`` inherits from ``SingleEnsembleMoveStrategy`` and
  will make the ``SpringShootingMover`` for every ensemble, with the given 
  values of ``delta_max``, ``k_spring`` and ``initial_guess``. 

* ``SpringShootingMoveScheme`` inherits from ``MoveScheme`` and it will use
  the ``SpringShootingStrategy`` to build all the necessary movers for the 
  given ``network``, with the given values of ``delta_max``, ``k_spring`` and
  ``initial_guess``. Also the functions ``to_dict`` and ``from_dict`` have been
  adapted to save and load with all the data


Background Information
______________________

This module builds on OpenPathSampling, a Python package for path sampling
simulations. To learn more about OpenPathSampling, you might be interested in
reading:

* OPS documentation: http://openpathsampling.org
* OPS source code: http://github.com/openpathsampling/openpathsampling


Testing
_______

.. Tests in OpenPathSampling use the `nose`_ package.

.. IF YOUR MODULE IS IN OPS CORE:

.. This module has been included in the OpenPathSampling core. Its tests can
.. be run by setting up a developer install of OpenPathSampling and running
.. the command ``nosetests`` from the root directory of the repository.

.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY

The tests for this module can be run by downloading its source code (see the 
``Source Code`` section below), installing its requirements and installing it
by running ``python setup.py install`` from the root directory of the package.
Test this module with the `nose`_ package, by running the command ``nosetests``
from the root directory of the repository.

Examples
________

* An example on how to set up a simulation using the ``SpringShootingMoveScheme``
  and the comparison with the ``UniformSelector`` can be found in 
  ``spring_shooting_example.ipynb`` in the ``examples`` directory 
  (https://gitlab.e-cam2020.eu/sroet/spring_shooting/tree/master/examples).
  Open it using ``jupyter notebook spring_shooting_example.ipynb`` (see 
  ``Jupyter notebook`` documentation at http://jupyter.org/ for more details)

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

.. This module has been merged into OpenPathSampling. It is composed of the
.. following pull requests:

.. * link PRs

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

The source code for this module can be found in: 
https://gitlab.e-cam2020.eu/sroet/spring_shooting/tree/master.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

