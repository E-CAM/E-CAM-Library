.. _poke:

####
POKE
####

.. sidebar:: Software Technical Information

 Language
   Fortran 2008                   


 Documentation Tool
   Doxygen,Sphinx,ReStructuredText


 Application Documentation
   `ESL wiki <http://esl.cecam.org/POKE>`_ 

 Licence
   L-GPL v3

.. contents:: :local:

Poke is a solver for the Poisson equation designed for electronic structure codes

Purpose of Module
_________________

Poke is a solver for the Poisson equation designed for electronic structure codes. Similarly to the eigensolvers, the aim
is be to implement in a single package several different algorithms of use in different situations, providing a unified
and clean interface for the user. Special attention goes to allowing different FFT back ends to be connected to the
library.

Software Technical Information
______________________________

License
  LGPL v3

Language
  Fortran 2008

Documentation Tool
   Doxygen,Sphinx,ReStructuredText

Application Documentation
   `The ESL wiki <http://esl.cecam.org/POKE>`_ 

Installation
___________ 

A release can be download from `this link <https://gitlab.e-cam2020.eu/ESL/poke/tags/Version-Poke-ahi>`_
Current installation and testing are done with gcc compiler. FFTW is required for installation and testing. 

Here are the commands for installation::

 $ tar xfvz poke-ahi.tar.gz
 $ ./configure
 $ make

.. note ::
 We provide also the possibility to build modules with Autotools. `This <https://gitlab.e-cam2020.eu/ESL/omm/blob/master/libOMM/doc/hacking-the-build-system.md>`_ is a useful document. 

Testing
_______

Poke contains several unit tests that can be used to check the
compilation and to perform regression testing. These tests can be
executed by doing::

 $ make check

Source Code
___________

 The source code is available from the `E-CAM Gitlab`__ under the `poke`__
 project. The poke directory can be found `here`__.
 
 .. __: https://gitlab.e-cam2020.eu/
 .. __: https://gitlab.e-cam2020.eu/ESL/poke/
 .. __: https://gitlab.e-cam2020.eu/ESL/poke/tree/develop
