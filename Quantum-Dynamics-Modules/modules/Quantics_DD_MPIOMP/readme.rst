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
    Quantics code: G. A. Worth,  K. Giri,  G. W. Richings,  M. H. Beck,  A. J ̈ackle,  and H.-D. Meyer.  Module: Thierry Tran and Graham Worth.    
    
.. contents:: :local:

Purpose of Module
_________________

The module focuses on improving the parallel version of Direct Dynamics 
variational multi-configuration Gaussian wavepacket (DD-vMCG) method.
At every step of the Direct dynamics propagation, the energies, gradients
and hessians are evaluated at the center of each gaussian wavepacket 
by calling an external program. One of the challenge of Direct Dynamics 
is the cost of computation for the evaluation of the potential energy 
surfaces. The electronic structure calculations are performed for each 
gaussian wavepacket individually and thus, parallelizing the call to the
electronic structure method greatly decreased the time of computation 
between each nuclear step. 
There is already an existing OpenMP parallelization of the code and the
purpose of this module is to add an extra MPI layer to it to allow 
affordable simulation of large system by spreading the calculations across
multiple computation nodes.
The module has been added and tested within the Quantics quantum dynamics 
package which is available on Gitlab.

Background Information
______________________


The latest version of quantics package and the code developed 
related to this module within the Quantics 
software package are merged and available through Quantics.gitlab_.

.. _Quantics.gitlab: https://gitlab.com/quantics


Application
______________________

This module will be extensively used in the near future to study the 
photochemistry of large systems, whose size limited the application 
of the previous version of the DD-vMCG code.


Testing
_______

After Quantics code has been successfully installed. The Quantics 
README file will help you to install the Quantics code. 
All the tests available for Direct Dynamics are suitable to test 
this module and can be found at ``inputs/``. A good and quick 
example is Butatriene. After you have copied the ``test.inp`` 
file and the ``but_dddata`` directory, the test can be done 
through the following command::

  $ mpirun quantics.mix -mpi test.inp  


Source Code
___________

The source code for this module can be found within the 
Quantics software which can be downloaded via Quantics.gitlab_. 
You firstly need to make an account (gitlab). The quantics 
project has a private repository so you also need to ask 
for access by emailing Graham Worth (g.a.worth@ucl.ac.uk).
In order to clone it into your computer, then type::

  $  git clone https://gitlab.com/quantics/quantics.git

.. _Quantics.gitlab: https://gitlab.com/quantics
