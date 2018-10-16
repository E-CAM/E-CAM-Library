##########################
FDF - Flexible Data Format
##########################

.. sidebar:: Software Technical Information

  Language
    Fortran 95

  Documentation Tool
    Sphinx, ReStructuredText

  Application Documentation
   `ESL wiki <http://esl.cecam.org/FDF_-_Flexible_Data_Format>`_ 

  Licence
    GPL             

.. contents:: :local:

Purpose of Module
_________________

FDF (Flexible Data Format) is an input file parser that offers an easy,
transferable and practical way for a Fortran program to read its input. It is
text (ASCII) based, and conceived for small data (input parameters). Every
input piece of data is introduced in a line of an input file (which can be
standard input) by writing a name-value pair, that is, a name characterising
the data, and its value. If the latter corresponds to a physical magnitude,
the units can also be specified after the value. Names can be long and should
be descriptive of the value it corresponds to. FDF blocks are used to input
structured data, in which case, the program using FDF reads the inside of the
block.

From the programming point of view, FDF allows for any data to be retrieved
whenever, from any part of the code, and in any order.

If a piece of data sought by FDF is not found in the input file, FDF will
return a default value, as set up in the call to the FDF routine. 

Background Information
______________________

FDF is a software library and module to be used within a calling code. It is
developed as part of the Siesta DFT code (see `Source Code`_), but is
self-contained within a separate directory and can be used independently of the
main code.

Software Technical Information
______________________________

License
  GPL

Language
  Fortran 95

Documentation Tool
  Source code documentation in progress.

Application Documentation
  `ESL wiki <http://esl.cecam.org/FDF_-_Flexible_Data_Format>`_

Relevant Training Material
  Creation of materials in progress.

Installation
____________

.. note::
 The information contained in the *Installation* and *Testing* sections are
 likely to work with the latest version of the source code from the Siesta website.
 If this is not the case you can download the `siesta-4.1-b2 release <https://launchpad.net/siesta/4.1/4.1-b2>`_  where the information is
 guaranteed to work.

For now, FDF has to be compiled as part of Siesta; see the documentation in the
``Docs`` directory. Once compiled, the FDF library and module files can be
found in the ``fdf`` subdirectory of the building directory.

1.For the senquential version installation, go to ``Obj`` and issue the command::
  
 sh ../Src/obj_setup.sh

* If the intel compiler is used, do::
  
   cp intel.make arch.make

* If the gcc compiler is used, do::
  
   cp gfortran.make arch.make

then do::
 
  make 

2. For parallel version installation, you should follow the same procedure except of using a approriate
parallel ``arch.make``. A `arch.make <https://gitlab.e-cam2020.eu:10443/E-CAM/Electronic-Structure-Modules/uploads/5fb8a4bbd4612fcfb4ea932d30804d6f/arch.make>`_ file with gcc compiler is available in E-CAM website.   

Testing
____________

Choose one specific test under the ``Obj/Tests`` directory, do::
 
 make 

Compare the output files with those under ``Tests/Reference``.
   

Source Code
___________

The source code is available from the `Launchpad`__ under the `siesta`__
project. The FDF directory can be found `here`__.

.. __: https://launchpad.net/
.. __: https://code.launchpad.net/siesta/
.. __: http://bazaar.launchpad.net/~siesta-maint/siesta/trunk/files/head:/Src/fdf/

