########
miniPWPP
########

.. sidebar:: Software Technical Information

 Language
   Fortran 1995.

 Documentation Tool
   Sphinx,ReStructuredText

 Application Documentation
   `Doc mirror <https://gitlab.com/kucukben/minipwpp-esl-ecam/blob/master/doc/readme.rst>`_

 Relevant Training Material
   See usage examples in the ``examples`` directory of the source code.

 Licence
    GNU Lesser General Public License v3.0

.. contents:: :local:


Purpose of Module
___________________

miniPWPP is a barebone DFT code that uses plane wave basis set. Its purpose is to serve as a testbed, 
benchmark platform, and a demonstrator for modules and libraries that are created for pseudopotential-plane wave codes. 

State of the art electronic structure packages cater to many possibilities: spin polarization, relativistic effects, 
several different treatments of electronic temperature etc. Because of this, their structure get more complicated over time,
making it difficult to test new ideas, to benchmark core libraries, to profile different algorithms, or simply, to learn 
what exactly is happening under the hood of an electronic structure engine.

With miniPWPP, we present a simple, modularized electronic structure engine, 
with core capabilities (less than 10 main routines, each beloging to a single step of a DFT workflow)
and with minimum decoration.

Due to this simple structure, miniPWPP can also serve for didactic purposes, both in physics and in information technologies.
As a first example, in this module we demonstrate how the FFT interface library (FFTXlib) can be split from the 
rest of the electronic structure code; while retaining the matrix algebra library (LAXlib) as a subdirectory.


Features
__________

Currently it uses pseudopotential form factors as defined by Cohen and Bergstresser in 1966. 
Pseudopotentials in Kleinman-Bylander form are not yet supported.

It supports solution of Kohn Sham equations at gamma or arbitrary k points.
These two cases correspond to two distinct executables, in order to increase the transparency of each:
mpp_gamma.x and mpp.x

It supports MPI parallelization.

It requires an FFT interface library (such as the one in E-CAM-Library: 
`FFTXlib module <https://gitlab.e-cam2020.eu/E-CAM-Library/tree/master/Electronic-Structure-Modules/modules/FFTXlib>`_



Building and Testing
______________________________

A stable version of the module can be downloaded using `this link <https://gitlab.com/kucukben/minipwpp-esl-ecam/>`_ 

Current installation and testing are done with gfortran compiler, version 6.3.0.
The configuration uses GNU Autoconf 2.69.

Here are the commands for installation::

 $ tar -zxvf miniPWPP-1.0.tgz
 $ ./configure
 $ make miniPWPP

During configure, you can either specify the FFT interface library using the FFTX_LIBS and FFTX_INCLUDE variables::

  $ ./configure FFTX_LIBS=/path/to/libfftx.a FFTX_INCLUDE=/path/to/fftx/modules/

If no library is specified, the FFTXlib module distributed from E-CAM-Library is downloaded, unpacked and used.

To test whether the module is working as expected, run:: 

..  $ make miniPWPP_ktest
..  $ make miniPWPP_gtest

The first tests the executable for the generic k point, while the second is for gamma point executable. 
The examples cover three different systems: free electrons in a periodic box, Silicon and GaAs crystals. 
By changing the input files in the examples directory, you can create your custom examples.
Refer to README.examples file in the examples directory for further details.


Source Code
___________


The miniPWPP bundle corresponding to the stable release can be downloaded from this `link <https://gitlab.com/kucukben/minipwpp-esl-ecam/>`_.
The source code itself can be found under the subdirectory ``src``.

Further Information
______________________

This documentation can be found inside the ``docs`` subdirectory.
Further diagonalization techniques such as Davidson and ParO will be added in the future. 

The miniPWPP module is developped with the contributions of S. de Gironcoli, A. Chandran and E. Kucukbenli
