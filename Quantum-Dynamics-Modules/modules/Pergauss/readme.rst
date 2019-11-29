.. _Pergauss:

###########################################################
PerGauss: Periodic Boundary Conditions for gaussian bases
###########################################################

.. sidebar:: Software Technical Information

  Language
    Fortran 90

  Licence
    None

  Documentation Tool
    Documentation provided as in-line comments within the source code

  Application Documentation
    Quantics documentation can be found in quantics_
    
  Relevant Training Material
    Tutorial and exercises to test the code are available here_


.. contents:: :local:


Purpose of Module
_________________

This module provides periodic boundary conditions for gaussian basis sets to be used in  G-MCTDH[1] and vMCG[2]
calculations using the Quantics_ software package. 

Background Information
______________________

Currently pergauss resides within the Quantics_ software package available upon request through gitlab_.


Testing
_______

A test example (``pergauss.inp``) is provided to test the module and can be found in the directory 
``$quantics_path/inputs``, where ``quantics_path`` is where Quantics_ is located.
The test can be done through the following command

::

        $ quantics -mnd pergauss.inp  

A more detailed test documentation for Quantics_ code developers can be found `in this link 
<http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/quantics/elk.html>`_
 

Source Code
___________

The source code for pergauss can be found within the Quantics_ software which 
can be downloaded via gitlab_. 
The Quantics_ project has a private repository so you also need to be a member of the project to checkout. 
Then type into terminal

::

        $ git clone https://gitlab.com/quantics/quantics.git DIRECTORY  

Within the Quantics_ program, the explicit code is located at the source code folder in files ``mctdhlib/gwplib.f90``, ``geninwf/eininwfmod.f90``, ``geninwf/genphi1.f90``, ``gendvr/einpbasmod.f90`` and ``include/global.f90``. Every modified line will be preceded by a comment saying !pergauss to help users finding the modifications.

.. _Quantics: https://www2.chem.ucl.ac.uk/worthgrp/quantics/doc/index.html
.. _gitlab: https://gitlab.com/quantics

References
__________

.. [1] I. Burghardt, I, H.-D. Meyer, and L. S. Cederbaum 
       *J. Chem. Phys.* **115** (1999) 2927


.. [2] G. W. Richings, I. Polyak, K. E. Spinlove, G. A. Worth, I. Burghardt, 
       B. Lasorne *Int. Rev. Phys. Chem.* **34** (2015) 269
        
