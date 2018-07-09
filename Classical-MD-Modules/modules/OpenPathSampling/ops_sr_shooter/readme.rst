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

The purpose of this algorithm is to increase the number of generated transitions in a transition path sampling simulation by exclusively shooting from the transition state ensemble (TSE)/the top of the barrier (hence the name). Naturally this only works if the approximate location of the TSE is already known and can be given as a function of one or more collective variables. In this module any `openpathsampling.Volume`_ object can be used by the user to define the shooting range volume. See also the `TSE module`_ for finding the TSE.

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

To test this module you need to download the source files package (see the ``Source Code`` section below) and install it using
``python setup.py install`` or ``pip install -e .`` from the root directory of the package. 
In the root folder then type ``nosetests`` to test the module using the `nose`_ package.


Examples
________


| There are two `example jupyter notebooks <https://gitlab.e-cam2020.eu/hejung/sr_shooter/tree/master/examples>`_ in the example directory of the repository:
| One shows the `general setup of a two way shooting transition path sampling with a shooting range on a toy system <https://gitlab.e-cam2020.eu/hejung/sr_shooter/blob/master/examples/toy_example.ipynb>`_.
| The other is a `comparison between one way shooting and two way shooting from the shooting range <https://gitlab.e-cam2020.eu/hejung/sr_shooter/blob/master/examples/OneWayShooting_vs_TwoWayShooting.ipynb>`_ and shows that path space is explored faster with two way shooting when using a (well placed) shooting range. The reason beeing that the shots initiated at the barrier top have a high probability of success and two way shooting decorrelates faster (if using randomized velocities even faster).

Source Code
___________

The source code for this module can be found in https://gitlab.e-cam2020.eu/hejung/sr_shooter.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/
.. _openpathsampling.Volume: http://openpathsampling.org/latest/volume.html
.. _example: 
.. _TSE module: ../ops_tse/readme
