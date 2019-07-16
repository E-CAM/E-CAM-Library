.. _Pergauss:

###########################################################
Periodic Boundary Conditions in a gaussian basis (Pergauss)
###########################################################

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


Purpose of Module
_________________

This module provides periodic boundary conditions for periodic normal modes while running a G-MCTDH or vMCG
calculation using the Quantics_ software package. 

Background Information
______________________

Currently the SOD integration scheme resides within the Quantics_ software package available through github_.


Testing
_______

A test example (``pergauss.inp``) is provided for the SOD integration scheme and can be found in the directory 
``~/quantics/elk_inputs``. 
This test works for Quantics_ Revision 787. 
The Quantics_ README file will help you to install the Quantics_ code. 
The test can be done through the following command

::

        $ quantics -mnd pergauss.inp  

A more detailed test documentation for Quantics_ code developers can be found `in this link 
<http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/quantics/elk.html>`_
 

Source Code
___________

The source code for pergauss can be found within the Quantics_ software which 
can be downloaded via CCPForge_. 
You firstly need to make an account (at `CCPForge <https://ccpforge.cse.rl.ac.uk/gf/project/quantics/>`_). 
The Quantics_ project has a private repository so you also need to be a member of the project to checkout. 
Then type into terminal

::

        $ svn checkout --username your-user-name https://ccpforge.cse.rl.ac.uk/svn/quantics/gmctdh/quantics/branches/ecam18 quantics.eca/  

Within the Quantics_ program, the explicit code is located in files ``~/quantics/source/blabla.f90``, ``~/quantics/source/blabla.f90``, ``~/quantics/source/blabla.f90`` and ``~/quantics/source/blabla.f90``. Every modified line will be preceded by a comment saying !pergauss to help people finding the modifications.

.. _Quantics: http://chemb125.chem.ucl.ac.uk/worthgrp/quantics
.. _gitlab: https://gitlab.com/quantics


References
__________

.. [Lef] C. Leforestier, R. H. Bisseling, C. Cerjan, M. D. Feit, R. Friesner, A. Guldberg, A. Hammerich, G. Jolicard, 
         W. Karrlein, H.-D. Meyer, N. Lipkin, O. Roncero, R. Kosloff *J. Comp. Phys.* **94** (1991) 59 
         `DOI: https://doi.org/10.1016/0021-9991(91)90137-A <http://www.sciencedirect.com/science/article/pii/002199919190137A>`_


