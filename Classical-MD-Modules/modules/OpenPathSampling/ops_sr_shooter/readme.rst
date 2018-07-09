.. _ops_sr_shooter:

########################################
OPS-based module: Shooting range shooter
########################################

.. sidebar:: Software Technical Information

  The information in this section describes OpenPathSampling as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Language
    Python (2.7)
    [+Python (3.6)]

  Documentation Tool
    Sphinx, numpydoc format (ReST)

  Application Documentation
    http://openpathsampling.org

  Relevant Training Material
    http://openpathsampling.org/latest/examples/

  Licence
    LGPL, v. 2.1 or later

.. contents:: :local:

This module implements the "shooting from the top" algorithm as detailed in `the paper "Transition path sampling of rare events by shooting from the top" <http://dx.doi.org/10.1063/1.4997378>`_.

Purpose of Module
_________________

The purpose of this algorithm is to increase the number of generated transitions in a transition path sampling simulation by exclusively shooting from the transition state ensemble(TSE)/ the top of the barrier (hence the name). Naturally this only works if the approximate location of the TSE is already known and can be given as a function of one or more collective variables. In this module any `openpathsampling.Volume`_ object can be used by the user to define the shooting range volume.

The implementation in this module includes:

* A ``ShootingRangeSelector`` subclass of ``openpathsampling.ShootingPointSelector`` to pick shooting points only in the predefined shooting range volume.


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


.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY

The tests for this module can be run by downloading its source code, 
installing its requirements, and running the command ``nosetests`` from the
root directory of the repository.

Examples
________


| There are two `example`_ jupyter notebooks in the example directory of the repository:
| `One <https://gitlab.e-cam2020.eu:10443/hejung/sr_shooter/blob/master/examples/toy_example.ipynb>`_ shows the general setup of a two way shooting transition path sampling with a shooting range on a toy system.
| The `other <https://gitlab.e-cam2020.eu:10443/hejung/sr_shooter/blob/master/examples/OneWayShooting_vs_TwoWayShooting.ipynb>`_ is a comparisson between one way shooting and two way shooting from the shooting range and shows that path space is explored faster with two way shooting when using a (well placed) shooting range. The reason beeing that the shots initiated at the barrier top have a high probability of success and two way shooting decorrelates faster (if using randomized velocities even faster).

Source Code
___________

The source code for this module can be found in the `ecam gitlab repository`_.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/
.. _openpathsampling.Volume: http://openpathsampling.org/latest/volume.html
.. _example: https://gitlab.e-cam2020.eu:10443/hejung/sr_shooter/tree/master/examples
.. _ecam gitlab repository: https://gitlab.e-cam2020.eu:10443/hejung/sr_shooter
