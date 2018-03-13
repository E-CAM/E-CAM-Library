.. _Quantics_omp_module:

######################################
Quantics OpenMP Improvements Module
######################################

.. sidebar:: Software Technical Information

  Language
    Fortran 90

  Licence
    Academy License

  Documentation Tool
    Documentation provided as in-line comments within the source code

  Application Documentation
    Useful documentation can be found `here <http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/index.html>`_.


  Relevant Training Material
    Training material is available through the test examples

.. contents:: :local:

.. Add technical info as a sidebar and allow text below to wrap around it

Purpose of Module
_________________

This module is related to code developed for the  2 Openmp-Improved SVN revision: v855 and v878 of Quantics here_
   .. _here:  https://ccpforge.cse.rl.ac.uk/svn/quantics/gmctdh/quantics/branches/ecam17

31 source files are changed in v855 compared to v854, such as the openmpmod.f90, quantics.F90, mmomplib.f90 and so on... While in the v878, OpenMP in database reading/interpolation (dd_db.f90) improved.
Here are the  file1_ and file2_ showing the changes made by the OpenMP Improvements.
   .. _file1: https://gitlab.e-cam2020.eu/liang/E-CAM-Library/blob/Module_OpenMP_Improvements_Quantics/Quantum-Dynamics-Modules/modules/Quantics_Openmp_Improvements_Module/Diff_quantics_ecam_854-855
   .. _file2: https://gitlab.e-cam2020.eu/liang/E-CAM-Library/blob/Module_OpenMP_Improvements_Quantics/Quantum-Dynamics-Modules/modules/Quantics_Openmp_Improvements_Module/Diff_quantics_ecam_863-878

Background Information
______________________

Currently the code developed related to this module within the Quantics software package is available through CCPForge_
  .. _CCPFORGE: https://ccpforge.cse.rl.ac.uk/svn/quantics/gmctdh/quantics/branches/ecam17


Install
_______

1. Under the ``install`` folder,  once the fortran compile is available, do ``./install_quantics``.
2. Once the quantics serial version is correctely installed, in the same folder, do ``source QUANTICS_client``.
3. Run ``compile -O quantics`` in order to install the OpenMP version of Quantics.  


Testing
_______

Several test example are provided for this module and can be found at ``inputs/test90.inp``. For example the ``p24+.inp``. This test works for Quantics's ECAM branch  Revision v974 . The Quantics README file will help you to install the Quantics code.  After creatin a folder named ``p24+`` and put  ``p24+.inp`` in this folder  then change its name to ``input``.  The test can be done through the following command::

  $ quantics.omp -omp np  -w -I p24+.inp  

 

Source Code
___________

The source code for this module can be found within the Quantics software which can be downloaded via `CCPForge <https://ccpforge.cse.rl.ac.uk/gf/project/quantics/>`_.  You firstly need to make an account (at CCPForge). The quantics project has a private repository so you also need to be a member of the project to checkout. then type into terminal::

 $ svn checkout --username your-user-name https://ccpforge.cse.rl.ac.uk/svn/quantics/gmctdh/quantics/branches/ecam17  



