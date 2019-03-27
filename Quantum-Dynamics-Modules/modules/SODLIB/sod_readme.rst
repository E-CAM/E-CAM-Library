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

  Software Module Developed by
    Graham Worth, Kaite Spinlove, Marcus Taylor

.. contents:: :local:


Purpose of Module
_________________

This module provides exact wavefunction propagation using the second-order differencing (SOD) integrator scheme 
to solve the time-dependent Schrödinger equation as described by Leforestier et al. [Lef]_ 
Within this scheme the time interval is determined through dividing :math:`\hbar` by the eigenvalue of the Hamiltonian 
operator with the largest absolute value. 

Background Information
______________________

Currently the SOD integration scheme resides within the Quantics_ software package available through CCPForge_.


Testing
_______

A test example (``test90.inp``) is provided for the SOD integration scheme and can be found in the directory 
``~/quantics/elk_inputs``. 
This test works for Quantics_ Revision 787. 
The Quantics_ README file will help you to install the Quantics_ code. 
The test can be done through the following command

::

        $ quantics test90.inp  

A more detailed test documentation for Quantics_ code developers can be found `in this link 
<http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/quantics/elk.html>`_
 

Source Code
___________

.. The source code for the second-order differencing propagator can be found within the Quantics_ software which 
.. can be downloaded via CCPForge_. 
.. You firstly need to make an account (at `CCPForge <https://ccpforge.cse.rl.ac.uk/gf/project/quantics/>`_). 
.. The Quantics_ project has a private repository so you also need to be a member of the project to checkout. 
.. Then type into terminal

.. ::

..        $ svn checkout --username your-user-name https://ccpforge.cse.rl.ac.uk/svn/quantics/gmctdh/quantics/trunk/  

The source code for the second-order differencing propagator can be found within the Quantics software which can be downloaded via gitlab_.  You firstly need to make an account (at gitlab). The Quantics project has a private repository so you also need to be a member of the project to clone it into your computer, then type:

 git clone https://gitlab.com/quantics/quantics.git

.. _gitlab: https://gitlab.com/quantics/quantics.git


Within the Quantics_ program, explicit code for the SOD routine is located in file ``~/quantics/source/lib/ode/sodlib.f90``.

.. _Quantics: http://chemb125.chem.ucl.ac.uk/worthgrp/quantics
.. _CCPFORGE: https://ccpforge.cse.rl.ac.uk/gf/project/quantics/


References
__________

.. [Lef] C. Leforestier, R. H. Bisseling, C. Cerjan, M. D. Feit, R. Friesner, A. Guldberg, A. Hammerich, G. Jolicard, 
         W. Karrlein, H.-D. Meyer, N. Lipkin, O. Roncero, R. Kosloff *J. Comp. Phys.* **94** (1991) 59 
         `DOI: https://doi.org/10.1016/0021-9991(91)90137-A <http://www.sciencedirect.com/science/article/pii/002199919190137A>`_


