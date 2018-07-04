.. _ops_plumed_wrapper:

###################################
PLUMED Wrapper for OpenPathSampling
###################################

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

Authors: Alberto Pérez de Alba Ortíz

This module interfaces OpenPathSampling (OPS) with PLUMED, an open source
library with a rich catalogue of Collective Variables (CVs).

* G.A. Tribello, M. Bonomi, D. Branduardi, C. Camilloni, G. Bussi,
  PLUMED2: New feathers for an old bird, Comp. Phys. Comm. 185, 604 (2014)

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

Transition path sampling simulations and analysis rely on accurate state
definitions. Such states are typically defined as volumes in a CV-space. OPS
already supports a number of CVs, including the ones defined in the MDTraj
python library. PLUMED offers a wide variety of extra CVs, which are enabled
in OPS by this module.

The class ``PLUMEDInterface`` is a subclass of the cython wrapper class
``Plumed`` contained in the PLUMED installation. For initialization,
``PLUMEDInterface`` requires an ``MDTrajTopology`` and accepts additional
PLUMED keywords. The initialized ``PLUMEDInterface`` can be subsequently
used to make functions that calculate CVs for a given ``Trajectory``. This
is done via the `PLUMEDCV`` class, a subclass of ``CoordinateFunctionCV``.

In PLUMED, the syntax for all commands is: ``label: keywords``. The class
``PLUMEDCV`` takes ``name`` and ``definition`` as arguments, which are
respectively equivalent to ``label`` and ``keywords``. The ``PLUMEDCV``
class also takes the ``PLUMEDInterface`` as argument. This allows for a
single ``PLUMEDInterface`` to contain the ``MDTrajTopology``, additional
PLUMED keywords and previously defined CVs that can be reused for the same
system. Both ``PLUMEDInterface`` and ``PLUMEDCV`` are storable.

This module supports (as listed in PLUMED documentation):

* Groups and Virtual Atoms: are directly set in the ``PLUMEDInterface`` via
  the ``PLUMEDInterface.set(name, definition)`` function. The
  ``PLUMEDInterface.get()`` function allows to consult the commands that
  have been already set.

* CV Documentation: all CVs are created by calling ``PLUMEDCV(name,
  PLUMEDInterface, definition)``. The returned function can be appied to a
  ``Trajectory``. CVs with components should specify the ``components=["c1",
  "c2", "c3"]`` keyword and the corresponding PLUMED keyword in the
  ``definition``.

* Distances from reference configurations: are also created by calling
``PLUMEDCV(name, PLUMEDInterface, definition)``. Most of them require
  external files with the reference configurations.

* Functions: are also created by calling ``PLUMEDCV(name, PLUMEDInterface,
  definition)``. They should be created using the same ``PLUMEDInterface``
  that contains the previously defined CVs that are to be part of the
  function.

* Multicolvar and Exploiting contact matrices are not tested.

For examples see the ``Examples`` section below.

For further PLUMED details see:
http://plumed.github.io/doc-master/user-doc/html/index.html

Background Information
______________________

This module builds on OpenPathSampling, a Python package for path sampling
simulations. To learn more about OpenPathSampling, you might be interested
in reading:

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
``Source Code`` section below), installing its requirements, and running the
command ``nosetests`` from the root directory of the repository.

Examples
________

* Examples on how create and calculate PLUMED CVs can be found in
  ``plumed_wrapper_example.ipynb`` in the ``examples`` directory
  (https://gitlab.e-cam2020.eu/apdealbao/plumed_wrapper/tree/
  master/plumed_wrapper/examples).
  Open it using ``jupyter notebook plumed_wrapper_example.ipynb``
  (see http://jupyter.org/ for more details).

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

.. This module has been merged into OpenPathSampling. It is composed of the
.. following pull requests:

.. * link PRs

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

The source code for this module can be found in:
https://gitlab.e-cam2020.eu/apdealbao/plumed_wrapper/tree/master

It can be installed by running ``pip install -e .`` from the root directory
of the package.

It requires to have the PLUMED development version with the cython wrapper
installed from: https://github.com/plumed/plumed2; and to source the file
'/path/to/plumed2/sourceme.sh'

For more details on PLUMED, see:
http://plumed.github.io/doc-master/user-doc/html/_installation.html

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

