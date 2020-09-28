.. 

.. _Direct_Dynamics_Database:

###########################################
Direct Dynamics Database improvements code
########################################### 

.. sidebar:: Software Technical Information

  Language
    Fortran 2003

  Licence
   GNU General Lesser Public License

  Documentation Tool
    Documentation provided as in-line comments within the source code

  Application Documentation
    Useful documentation can be found `here <http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/>`_ 


  Relevant Training Material
    Training material is available through the test examples

  Software Module Developed by
    Quantics code: G. A. Worth,  K. Giri,  G. W. Richings,  M. H. Beck,  A. J ̈ackle,  and H.-D. Meyer.  Module: Georgia Christopoulou and Graham Worth.    
    
.. contents:: :local:

Purpose of Module
_________________

The module focuses on improving the efficiency of Direct Dynamics variational multi-configuration Gaussian wavepacket (DD-vMCG) method. 
During every Direct Dynamics propagation step the calculated energies, gradients and hessian matrix are stored in a database. 
One important challenge of this method is the time needed to continually reread, sort and analyze this database which makes the 
calculation of a large system very expensive. Employing a dynamic and smaller version of the database with selected points only 
relevant to each basis function, each time the program needs to use stored data points, reduces massively the cost of the calculation. 
Thus, treatment of larger systems is now possible. The module has been added and tested within the Quantics quantum dynamics package 
which is available on Gitlab and at the same time the code benefits from parallel running.

Background Information
______________________


The latest version of quantics package and the code developed related to this module within the Quantics 
software package are merged and available through Quantics.gitlab_.

.. _Quantics.gitlab: https://gitlab.com/quantics/quantics.git


Application
______________________

This module will be extensively used in the near future to study the photochemistry of large systems, 
whose size limited the application of the previous version of the DD-vMCG code.


Testing
_______

After Quantics code has been successfully installed. The Quantics README file will help you to install the Quantics code. 
All the tests available for Direct Dynamics are suitable to test this module and can be found at ``inputs/``. 
A good and quick example is Butatriene. After you have copied the ``but_dd.inp`` file and the ``but_dddata`` directory, 
the test can be done through the following command::

  $ quantics but_dd.inp  


Source Code
___________

The source code for this module can be found within the Quantics software which can be downloaded via Quantics.gitlab_. 
You firstly need to make an account (gitlab). The quantics project has a private repository so you also need to be a member 
of the project to clone it into your computer, then type::

  $  git clone https://gitlab.com/quantics/quantics.git

  $  git checkout ECAM 

.. _Quantics.gitlab: https://gitlab.com/quantics/quantics.git
