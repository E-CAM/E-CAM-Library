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

  Software Module Developed by
    Graham Worth, Ceridwen Ash

.. contents:: :local:


Purpose of Module
_________________

This module implements the Chebyshev integration scheme for exact wavefunction propagation on the grid. 
This routine has been implemented and tested within the Quantics_ quantum dynamics package which is available on CCPForge_. 
The purpose of the module is to be used in quantum dynamical propagation problems as described by 
Leforestier et al. [1Lef]_ 


Background Information
______________________


Currently, the Chebyshev integration scheme resides within the Quantics_ software package available through CCPForge_.

The module consists of a main routine, ``chebstep``, which uses a separate routine ``bessjn`` to generate the 
real Bessel coefficients of integer order. 
Both are referenced and documented within the *cheblib* routine, and for further information on the Chebyshev integration 
scheme see Tal-Ezer et al. [1Tal]_


Testing
_______

A test example (``test89.inp``) is provided for the Chebyshev integration scheme and can be found in the directory 
``~/quantics/elk_inputs``. 
This test works for Quantics_ Revision 787. 
The Quantics_ README file will help you to install the Quantics_ code. 
The test can be done through the following command

::

      $ quantics test89.inp  

A more detailed test documentation for Quantics_ code developers can be found `in this link 
<http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/quantics/elk.html>`_

 
Source Code
___________

The source code for the Chebyshev propagator can be found within the Quantics_ software which can be downloaded via CCPForge_.  
You firstly need to make an account (at CCPForge_). 
The Quantics_ project has a private repository so you also need to be a member of the project to checkout. 
Then type into terminal

::

      $ svn checkout --username your-user-name https://ccpforge.cse.rl.ac.uk/svn/quantics/gmctdh/quantics/trunk/  


Within the Quantics_ program, explicit code for the Chebyshev routine is located in the file 
``~/quantics/source/lib/ode/cheblib.f90``.

.. _Quantics: http://chemb125.chem.ucl.ac.uk/worthgrp/quantics
.. _CCPFORGE: https://ccpforge.cse.rl.ac.uk/gf/project/quantics/


References
__________

.. [1Lef] C. Leforestier, R. H. Bisseling, C. Cerjan, M. D. Feit, R. Friesner, A. Guldberg, A. Hammerich, G. Jolicard, 
         W. Karrlein, H.-D. Meyer, N. Lipkin, O. Roncero, R. Kosloff *J. Comp. Phys.* **94** (1991) 59 
         `DOI: https://doi.org/10.1016/0021-9991(91)90137-A <http://www.sciencedirect.com/science/article/pii/002199919190137A>`_
.. [1Tal] H. Tal‐Ezer, R. Kosloff *J. Chem. Phys.* **81** (1984) 3967 `DOI: 10.1063/1.448136 <https://doi.org/10.1063/1.448136>`_


