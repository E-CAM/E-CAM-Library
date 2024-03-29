.. _SQARE-ODE:

#########
SQARE ODE
#########

.. sidebar:: Software Technical Information

 Language
   C with Fortran 2003 bindings.


 Documentation Tool
   Doxygen,Sphinx,ReStructuredText


 Application Documentation
   `ESL wiki <http://esl.cecam.org/SQARE>`_ 

 Licence
   L-GPL v3

.. contents:: :local:


SQARE (Solvers for quantum atomic radial equations) is a library of
utilities intended for dealing with functions discretized on radial
meshes, wave-equations with spherical symmetry and their corresponding
quantum states. The utilities are segregated into three levels: radial
grids and functions, ODE solvers, and states.

Purpose of Module
_________________

This module provides functions and structures to solve ordinary
differential equations on a radial mesh.

Background Information
______________________

If the modifications are to an existing code base then this would be the place to describe that codebase and how to get
access to it.

Software Technical Information
______________________________

License
 LGLP v3

Language
  C with Fortran 2003 bindings.

Documentation Tool
  Doxygen,Sphinx,ReStructuredText

Application Documentation
`The ESL wiki <http://esl.cecam.org/SQARE>`_

Installation
____________ 

A release can be download from `This link <https://gitlab.e-cam2020.eu/ESL/sqare/tags/v0.0.0>`_
Current installation and testing are done with gcc compiler. Check (version>=0.9.4) is required for installation and testing.

Here are the commands for installation::

 $ tar xfvz libsqare-0.0.0.tar.gz
 $ ./configure
 $ make


Testing
_______

SQARE contains several unit tests that can be used to check the
compilation and to perform regression testing. These tests can be
executed by doing:

   $ make check

Source Code
___________


The source code is available from the `E-CAM Gitlab`__ under the
  `sqare`__ project. The SQARE ODE-solvers directory can be found `here`__.

.. __: https://gitlab.e-cam2020.eu/
.. __: https://gitlab.e-cam2020.eu/ESL/sqare/
.. __: https://gitlab.e-cam2020.eu/ESL/sqare/tree/src_split/ode-solvers
