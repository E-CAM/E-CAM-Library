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

  Software Module Developed by
    Surface hopping code was developed in the group of Nadja Doslic in Zagreb, some of the authors are Marin Sapunar, Momir Malis and Nadja Doslic. The implementation of this module has been done by Cristina Sanz-Sanz and Prof. Graham A. Worth.    
.. contents:: :local:

.. Add technical info as a sidebar and allow text below to wrap around it

Purpose of Module
_________________

This module implements an interface between the Tully's fewest switch surface hopping code, written and mantained by the group of Nadja Doslic in Zagreb, and QUANTICS code. The module has been added and tested within the Quantics quantum dynamics package which is available on Gitlab. The purpose of the module is to add the solution of Hamilton classical equation using the surface hopping approach into QUANTICS code, at the same time as Zagreb code benefits from all functionalities implemented in QUANTICS as input definitions, Hamiltonian operator description, direct dynamics calculations and parallel running.    


Background Information
______________________


Currently, the Zagreb surface hopping quasiclassical trajectory code resides within the Quantics software package available through gitlab_. 

.. _GITLAB: https://gitlab.com/quantics/quantics.git

The module consists of an interface between QUANTICS package and Zagreb surface hopping code. The module is fully integrated into QUANTICS code so that initial conditions, wavefunction definition, analysis programs, direct dynamics etc... can be used in the usual way described in QUANTICS documentation. The interface creates the required input files to run separate trajectories using the Zagreb surface hopping code. Although the module is implemented and run under QUANTICS, the Zagreb code requires some directives that must be given in the input file under the SH_ZAGREB_SECTION. This directives are described in the Zagreb code input manual which can be found in a separate pdf file in the QUANTICS documentation under the Zagreb surface hopping program. 

Application
______________________

The Tully's surface hopping technique has been widely used in molecular dynamics simulations to incorporate the non-adiabatic effects.
The module can be apply to all classical propagations in multistate systems, specially in thoses systems where the dynamics cannot be explained using only one electronic state.


Testing
_______

A test example is provided for the excitation of an initial wavepacket into an excited state that is coupled with another excited state. The relevant input file is 'ferrety_tsh.inp' (which can be found in the 'inputs' subdirectory of the sources) and corresponding operator file is 'ferreti.op' (found in the 'operators' subdirectory). The test example is a simple analytical model provided by Ferreti et al. (JCP, 104,5517 (1996)). The documentation under the Zagreb surface hopping code will help you to install the Zagreb code. The test can be done through the following command::

  $ quantics -mnd ferretti_tsh

A more detailed test documentation file 'sh_zagreb.html' can be found in the subdirectory 'sh_zagreb' of the documentation subdirectory 'doc' of the sources. The html file has been provided by the Zagreb code developers. 
 
An output directory is provided for testing and comparison (it is available to download as a tarball, :download:'output.tgz'<./output.tgz>'.). The output directory includes the output file (quantics common output file) and the zagreb_trj directory. This directory only includes the first trajectory directory (traj.1 directory), for space saving reasons.  

Source Code
___________

The source code for the Zagreb surface hopping code can be found within the Quantics software which can be downloaded via gitlab_.  You firstly need to make an account (at gitlab). The quantics project has a private repository so you also need to be a member of the project to clone it into your computer, then type:

 git clone https://gitlab.com/quantics/quantics.git

.. _gitlab: https://gitlab.com/quantics/quantics.git


Within the Quantics program, explicit code for the Zagreb surface hopping code is located in the subdirectory 'sh_zagreb' into the subdirectory 'source' of the sources.


