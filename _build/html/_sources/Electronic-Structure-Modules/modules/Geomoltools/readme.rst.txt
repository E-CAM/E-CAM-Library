###########
Geomoltools
###########

.. sidebar:: Software Technical Information

  This section describes the module Geomoltools.

  Language
    Fortran 95 

  Documentation Tool
    Sphinx, ReStructuredText

  Relevant Training Material
    See usage examples in the ``tutorial`` directory.
  
  Licence
    GNU General Public License (GPL) version 2. 

.. contents:: :local:

Purpose of Module
_________________

Geomoltools is a set of computer codes designed to manipulate molecules.
From simple changes of coordinates (Z-matrix to XYZ coordinates and *vice versa*)
to more complicated operations as the generation of different stacking
arrangements between molecules are quick and easy to perform.

Background Information
______________________

Geomoltools is a set of standalone codes to be employed independently
or taking part in the development of a more general project.

Installation
____________

The module does not need installation because is formed by independent pieces.
The dependency of each single code with common subroutines are managed in the
Makefile file, which leds an easy and straightforward compilation of the
desired codes. 
 
Testing
_______

In the ``tutorial`` directory input/output files for each code are found
and constitute examples of test and usage.

Source Code
___________

The package, including all the files as well as technical help, can be found in the `E-CAM Gitlab`__ webpage. 

.. __: https://gitlab.e-cam2020.eu/plesiat/geomoltools
