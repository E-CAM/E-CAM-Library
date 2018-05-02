.. _cheblib:

######################### 
The Chebyshev Scheme (CH)
######################### 

.. sidebar:: Software Technical Information

  Language
    Fortran 90

  Licence
    None

  Documentation Tool
    Documentation provided as in-line comments within the source code

  Application Documentation
    Useful documentation can be found `here <http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/index.html>`_ 

  Relevant Training Material
    Training material is available through the test examples

.. contents:: :local:

.. Add technical info as a sidebar and allow text below to wrap around it

Purpose of Module
_________________

This module implements the Chebyshev integration scheme for exact wavefunction propagation on the grid. This routine has been implemented and tested within the Quantics quantum dynamics package which is available on CCP Forge. The purpose of the module is to be used in quantum dynamical propagation problems as described by Leforestier et al, J. Comp Phys, 94, 59-80, 1991.


Background Information
______________________


Currently, the Chebyshev integration scheme resides within the Quantics software package available through CCPForge_.

.. _CCPFORGE: https://ccpforge.cse.rl.ac.uk/gf/project/quantics/

The module consists of a main routine, chebstep, which uses a separate routine bessjn to generate the real Bessel coefficients of integer order. Both are referenced and documented within the cheblib routine, and for further information on the Chebyshev integration scheme see Tal-Ezer, H. and Kosloff, R. (1984) Journal of Chemical Physics, 81, 3967.

Testing
_______

A test example is provided for the Chebyshev integration scheme and can be found at ~/quantics/elk_inputs/test89.inp. This test works for Quantics Revision 787. The Quantics README file will help you to install the Quantics code. The test can be done through the following command::

  $ quantics test89.inp  

A more detailed test documentation for Quantics code developers can be found `in this link <http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/quantics/elk.html>`_
 
Source Code
___________

The source code for the chebyshev propagator can be found within the Quantics software which can be downloaded via CCPForge_.  You firstly need to make an account (at CCPForge). The quantics project has a private repository so you also need to be a member of the project to checkout. then type into terminal::

 svn checkout --username your-user-name https://ccpforge.cse.rl.ac.uk/svn/quantics/gmctdh/quantics/trunk/  

.. _CCPFORGE: https://ccpforge.cse.rl.ac.uk/gf/project/quantics/


Within the Quantics program, explicit code for the Chebyshev routine is located through ~/quantics/source/lib/ode/cheblib.f90



