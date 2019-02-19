.. _pspio:

########
Libpspio
########

.. sidebar:: Software Technical Information

 Language
   Libpspio is written in C, with bindings in Fortran 2003.

 Documentation Tool
   Doxygen,Sphinx,ReStructuredText

 Application Documentation
   Provide a link to any documentation

 Application Documentation
   `ESL wiki <http://esl.cecam.org/Libpspio>`_ 

 Licence
    GNU Lesser GPL 

.. contents:: :local:

Libpspio is a pseudopotentials I/O library for Density-Functional
Theory (DFT) calculations. It can both read and write pseudopotential
data, which makes it suitable for use with pseudopotential generators
and electronic structure codes.

Purpose of Module
_________________

The main objective of Libpspio is to let any DFT code access or
produce pseudopotential information without having to care about file
formats. Libpspio is a valuable alternative to most error-prone
homemade implementations and is helpful in improving file format
specifications.


Software Technical Information
______________________________

Language
  Libpspio is written in C, with bindings in Fortran 2003.

Documentation Tool
  Doxygen,Sphinx,ReStructuredText

Application Documentation
  Provide a link to any documentation

Application Documentation
  `The ESL wiki <http://esl.cecam.org/Libpspio>`_ 

Licence
   GNU Lesser GPL 

Installation
____________ 

A release can be download from `This link <https://gitlab.e-cam2020.eu/ESL/pspio/tags/v0.0.0>`_
Current installation and testing are done with gcc compiler. GNU Scientific Library (GSL, version>1.15) and 
Check (a unit test framework for C, version>0.94) is required for installation and testing. 

Here are the commands for installation::

 $ tar xfvz libpspio-0.0.0.tar.gz
 $ ./configure
 $ make

.. note ::
 We provide also the possibility to build modules with Autotools. `This <https://gitlab.e-cam2020.eu/ESL/omm/blob/master/libOMM/doc/hacking-the-build-system.md>`_ is a useful document. 



Testing
_______

Libpspio contains several unit tests that can be used to check the
compilation and to perform regression testing. These tests can be
executed by doing::

 $ make check


Source Code
___________


The source code is available from the `E-CAM Gitlab`__ under the `pspio`__
project. The Libpspio directory can be found `here`__.

.. __: https://gitlab.e-cam2020.eu/
.. __: https://gitlab.e-cam2020.eu/ESL/pspio/
.. __: https://gitlab.e-cam2020.eu/ESL/pspio/tree/master
