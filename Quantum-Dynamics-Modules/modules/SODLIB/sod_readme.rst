.. _SODLIB:

######################################
Second-Order Differencing Scheme (SOD)
######################################

.. sidebar:: Software Technical Information

  Language
    Fortran 90

  Licence
    None

  Documentation Tool
    Documentation provided as in-line comments within the source code

  Application Documentation
    Useful documentation can be found here_
    
    .. _here: http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/index.html 

  Relevant Training Material
    Training material is available through the test examples

.. contents:: :local:

.. Add technical info as a sidebar and allow text below to wrap around it

Purpose of Module
_________________

This module provides exact wavefunction propagation using the second-order differencing (SOD) integrator scheme to solve the time-dependent Schroedinger Equation as described by Leforestier et al, J. Comp Phys, 94, 59-80, 1991. Within this scheme the time interval is determined through dividing hbar by the eigenvalue of the Hamiltonian operator with the largest absolute value.      

Background Information
______________________

Currently the SOD integration scheme resides within the Quantics software package available through CCPForge_:

.. _CCPFORGE: https://ccpforge.cse.rl.ac.uk/gf/project/quantics/


Testing
_______

A test example is provided for the SOD integration scheme and can be found at ~/quantics/elk_inputs/test90.inp. This test works for Quantics Revision 787. The Quantics README file will help you to install the Quantics code. The test can be done through the following command::

  $ quantics test90.inp  

A more detailed test documentation for Quantics code developers can be found `in this link <http://stchem.bham.ac.uk/~quantics/doc/mctdh/elk.html>`_
 

Source Code
___________

The source code for the chebyshev propagator can be found within the Quantics software which can be downloaded via CCPForge_.  You firstly need to make an account (at CCPForge). The quantics project has a private repository so you also need to be a member of the project to checkout. then type into terminal::

 $ svn checkout --username your-user-name https://ccpforge.cse.rl.ac.uk/svn/quantics/gmctdh/quantics/trunk/  

.. _CCPFORGE: https://ccpforge.cse.rl.ac.uk/gf/project/quantics/

Within the Quantics program, explicit code for the SOD routine is located through ~/quantics/source/lib/ode/sodlib.f90

