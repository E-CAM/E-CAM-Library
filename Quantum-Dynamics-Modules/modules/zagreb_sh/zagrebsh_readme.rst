.. _zagrebsh:

########################### 
Zagreb surface hopping code
########################### 

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
    Surface hopping code: Nadja Doslic, Marin Sapunar and Momir Malis. Module: Graham Worth, Cristina Sanz-Sanz.    
.. contents:: :local:

.. Add technical info as a sidebar and allow text below to wrap around it

Purpose of Module
_________________

This module implements an interface between the Tully's fewest switch surface hopping code, written and mantained by the group of Nadja Doslic in Zagreb, and Quantics code. The module has been added and tested within the Quantics quantum dynamics package which is available on Gitlab. The purpose of the module is to add the solution of Hamilton classical equation using the surface hopping approach into Quantics code, at the same time as Zagreb code benefits from all functionalities implemented in Quantics as input definitions, Hamiltonian operator description, direct dynamics calculations and parallel running.    


Background Information
______________________


Currently, the Zagreb surface hopping quasiclassical trajectory code resides within the Quantics software package available through gitlab_. 

.. _GITLAB: https://gitlab.com/quantics/quantics.git

The module consists of an interface between Quantics package and Zagreb surface hopping code. The module is fully integrated into Quantics code so that initial conditions, wavefunction definition, analysis programs, direct dynamics etc... can be used in the usual way described in Quantics documentation. The interface creates the required input files to run separate trajectories using the Zagreb surface hopping code. Although the module is implemented and run under Quantics, the Zagreb code requires some directives that must be given in the input file under the SH_ZAGREB_SECTION. This directives are described in the Zagreb code input manual which can be found in a separate pdf file in the Quantics documentation under the Zagreb surface hopping program. 

Application
______________________

The Tully's surface hopping technique has been widely used in molecular dynamics simulations to incorporate the non-adiabatic effects.
The module can be apply to all classical propagations in multistate systems, specially in thoses systems where the dynamics cannot be explained using only one electronic state.


Testing
_______

A test example is provided for the excitation of an initial wavepacket into an excited state that is coupled with another excited state. The relevant input file is 'ferrety_tsh.inp' (which can be found in the 'inputs' subdirectory of the sources) and corresponding operator file is 'ferreti.op' (found in the 'operators' subdirectory). The test example is a simple analytical model provided by Ferreti et al. (JCP, 104,5517 (1996), https://doi.org/10.1063/1.471791). The documentation under the Zagreb surface hopping code will help you to install the Zagreb code. The test can be done through the following command::

  $ quantics -mnd ferretti_tsh

A more detailed test documentation file 'sh_zagreb.html' can be found in the subdirectory 'sh_zagreb' of the documentation subdirectory 'doc' of the sources. The html file has been provided by the Zagreb code developers. 
 
An output directory is provided for testing and comparison (it is available to download as a tarball, :download:`output.tgz<./output.tgz>`). The output directory includes the output file (Quantics common output file) and the zagreb_trj directory. This directory only includes the first trajectory directory (traj.1 directory) of a run of a total of 5 trajectories, for space saving reasons. 

The run of this test would produce a directory called 'ferretti_tsh' in which there are the typical output files produced by Quantics program run, plus a directory 'zagreb_trj' in which all the individual trajectories appear (traj.1, traj.2, etc...) as independent directories. Inside any of the trajectory directory, there are the output files of the classical trajectories run for this test. In order to compare that the run of the test has gone well the file 'dynamics.out' inside the 'traj.1' directory should be compared. After the propagation of the total number of trajectories the output file inside the 'ferretti_tsh' directory is written and this output file should be as well compared. Please notice that, due to the random number generation, required by the algorithms of classical trajectories, some numerical deviation should be expected in both output files. 

Source Code
___________

The source code for the Zagreb surface hopping code can be found within the Quantics software which can be downloaded via gitlab_.  You firstly need to make an account (at gitlab). The Quantics project has a private repository so you also need to be a member of the project to clone it into your computer, then type:

 git clone https://gitlab.com/quantics/quantics.git

.. _gitlab: https://gitlab.com/quantics/quantics.git


Within the Quantics program, explicit code for the Zagreb surface hopping code is located in the subdirectory 'sh_zagreb' into the subdirectory 'source' of the sources.


