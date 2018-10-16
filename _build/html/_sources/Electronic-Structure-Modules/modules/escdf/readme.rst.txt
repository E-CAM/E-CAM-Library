.. _Libescdf:

########
Libescdf
########

.. sidebar:: Software Technical Information

 Language
   C with Fortran 2003 bindings.


 Documentation Tool
   Doxygen,Sphinx,ReStructuredText


 Application Documentation
   `The ESL wiki <http://esl.cecam.org/ESCDF_-_Electronic_Structure_Common_Data_Format>`_ 

 Licence
   L-GPL v3

.. contents:: :local:

Libescdf is a library containing tools for reading and writing massive
data structures related to electronic structure calculations,
following the standards defined in the `Electronic Structure Common
Data Format
<http://esl.cecam.org/ESCDF_-_Electronic_Structure_Common_Data_Format>`_

Purpose of Module
_________________

Libescdf is a library created to exchange electronic-structure-related
data in a platform-independent and efficient manner. It is based on
the Electronic Structure Common Data Format Specifications, as well as
HDF5.

Software Technical Information
______________________________

License
  L-GPL v3

Language
  C with Fortran 2003 bindings.

Documentation Tool
  Doxygen

Application Documentation
  `ESL wiki <http://esl.cecam.org/Libescdf>`_

Instalation
___________ 

A release can be download from `this link <https://gitlab.e-cam2020.eu/ESL/escdf/tags/Version0.1.0>`_
Current installation and testing are done with gcc compiler. HDF5 is required for instalation and testing. 

Here are the commands for instalation::

 $ tar xfvz libescdf-0.1.0.tar.gz
 $ ./configure
 $ make

.. note ::
 We provide also the possibility to build modules with Autotools. `Here <https://gitlab.e-cam2020.eu/ESL/escdf/tree/master/doc>`_ are some userful documents. 

Testing
_______

Libescdf contains several unit tests that can be used to check the
compilation and to perform regression testing. Check (version>=0.9.4) is required for instalation and testing. These tests can be
executed by doing::

   $ make check


Source Code
___________

 The source code is available from the `E-CAM Gitlab`__ under the `escdf`__
 project. The Libescdf directory can be found `here`__.
 
 .. __: https://gitlab.e-cam2020.eu/
 .. __: https://gitlab.e-cam2020.eu/ESL/escdf/
 .. __: https://gitlab.e-cam2020.eu/ESL/escdf/tree/master

