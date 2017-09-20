.. _physconst:

###############################
E-CAM Physical Constant module
###############################

.. sidebar:: Software Technical Information

  Language
    Fortran 95

  Compiler
    gfortran, ifort

  Licence
    GNU Lesser General Public License (LGPL)

  Documentation Tool
    Doxygen

.. contents:: :local:

Purpose of Module
_________________

This module enables to use fundamental physical constants (speed of light in vacuum, Planck constant ... and isotopic
masses). Two versions can be selected:

- The CODATA 2006 ones, downloaded from `NIST <http://physics.nist.gov/cuu/Constants/archive2006.html>`_ and the NIST
  `masses <https://www.nist.gov/pml/atomic-weights-and-isotopic-compositions-relative-atomic-masses>`_ downloaded in
  2012 (default).

- Constants and masses from the 70th edition of the Handbook of Chemistry and Physics.

From these fundamental constants, some conversion factors are calculated automatically and can be used easily.

Remark: the actual mass values of the NIST web page differ slightly from the module ones.

Background Information
______________________

This module has been extracted and modified from the ElVibRot-Tnum-Tnum code (ElVibRot_f90-v80.13-Tnum28.9-Tana5.1).
This pre-E-CAM version can be downloaded
`here <http://pagesperso.lcp.u-psud.fr/lauvergnat/ElVibRot/preECAM-E.80.13.28.9.5.1.tar.gz>`_.

Installing
__________

Dependencies: this module needs the fortran modules in the ``Source_Lib/sub_system`` directory.

Build the module (with dependencies):

::

    make PhysConst

Build the module documentation (with doxygen):

::

    make doxy

Testing
_______

Two example data/script files:

::

    Examples/exa_PhysicalConstants/dat_PhysConst
    Examples/exa_PhysicalConstants/dat_PhysConst_HandBook70ed

To test the installation, you can run both test examples.

::

     cd Examples/exa_PhysicalConstants ; ./run_tests

The results will be compared to previous results in ```Examples/exa_PhysicalConstants/output_17dec2016``


Source Code
___________

The source code can be downloaded from the `E-CAM GitLab service <https://gitlab.e-cam2020.eu/lauvergn/ElVibRot>`_.
