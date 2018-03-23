.. _zagrebsh:

######################### 
Zagreb surface hopping code
######################### 

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

.. contents:: :local:

.. Add technical info as a sidebar and allow text below to wrap around it

Purpose of Module
_________________

This module implements the Tully's fewest switch surface hopping algorithm written and mantained by the group of Nadja Doslic in Zagreb. The module has been added and tested within the Quantics quantum dynamics package which is available on CCP Forge. The purpose of the module is to consider the hop within two or more coupled electronic states in a typical quasiclassical trajectory propagation.   


Background Information
______________________


Currently, the Zagreb surface hopping quasiclassical trajectory code resides within the Quantics software package available through CCPForge_.

.. _CCPFORGE: https://ccpforge.cse.rl.ac.uk/gf/project/quantics/

The module consist of a set of subroutines to perfom quasiclassical trajectories using the surface hopping approach. There is an interface between the surface hopping program and the electronic time dependent Schrodinger equation solution, and all the subroutines to solve the Hamilton's classical equations. The module is fully integrated into QUANTICS code so that initial conditions, wavefunction definition, analysis programs, etc... can be used in the usual way. Further documentation related to QUANTICS commands, keywords and input/output files can be obtained in the application documentation. Although the module is implemented and run under QUANTICS, the module is a standalone program which has its own input manual. The input manual is in a separate pdf file which can be obtained from the QUANTICS documentation under the Zagreb surface hopping program. 

Testing
_______

A test example is provided for the excitation of an initial wavepacket into an excited state that is coupled with another excited state, the input file can be found at ~/quantics/inputs/ferretti_tsh.inp and the hamiltonian operator at ~/quantics/operators/ferretti.op. The test example is a simple analytical model provided by Ferreti et al. (JCP, 104,5517 (1996)). The documentation under the Zagreb surface hopping code will help you to install the Zagreb code. The test can be done through the following command::

  $ quantics ferretti_tsh.op

A more detailed test documentation for Zagreb code developers can be found `in this link <http://stchem.bham.ac.uk/~quantics/doc/sh_zagreb/sh_zagreb.html>`_
 
Source Code
___________

The source code for the Zagreb surface hopping code can be found within the Quantics software which can be downloaded via CCPForge_.  You firstly need to make an account (at CCPForge). The quantics project has a private repository so you also need to be a member of the project to checkout. then type into terminal::

 svn checkout --username your-user-name https://ccpforge.cse.rl.ac.uk/svn/quantics/gmctdh/quantics/branches/quantics.ecam17/  

.. _CCPFORGE: https://ccpforge.cse.rl.ac.uk/gf/project/quantics/


Within the Quantics program, explicit code for the Zagreb surface hopping code is located through ~/quantics/source/sh_zagreb/src/



