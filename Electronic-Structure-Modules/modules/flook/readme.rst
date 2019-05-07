#####
flook
#####

.. sidebar:: Software Technical Information

  Language
    Fortran 1990/2003

  Documentation Tool
    Doxygen

  Application Documentation
   `ESL wiki <http://esl.cecam.org/Flook>`_
   `API <http://electronicstructurelibrary.github.io/flook/doxygen/index.html>`_ 

  Relevant Training Material
    See usage examples in the ``src/test`` directory of the source code.
  
  Licence
    MPL-2.0

.. contents:: :local:

Purpose of Module
_________________

The flook library is a simplifiled API for communicating between fortran code and
the `Lua <https://www.lua.org>`_ scripting language. A basic method is to
use flook as an interactive interpreter to pass variables back and forth between a
parent fortran program. It does not only serve as a simple input engine but also
allows calling specific fortran functions from within Lua. Thus by exposing
fortran module procedures one can control the flow of programs as well as parameters
in programs.

Application
-----------

The library is currently enabled in `Siesta <https://launchpad.net/siesta>`_ and
`ESL-demo <https://esl.cecam.org/Esl-demo>`_ where it can be used to change convergence
parameters *on the fly* and/or being used as an MD back-end.
Since it allows exchange of data between Lua and fortran basically every variable in fortran
can be exposed to the user via a Lua script.


Background Information
______________________

The flook library is built on two libraries; 1) Lua and 2) `AOTUS <https://bitbucket.org/haraldkl/aotus>`_.
Both are shipped together with the software and are required when building.
Lua is required to be able to run the Lua interpreter in-memory while the AOTUS
library is required for the low-level communication layer. 


Installation
____________

The source code of the ``flook`` module is hosted on Github and can be obtained
using ``git`` or via the release page of ``flook``::

  git clone https://github.com/ElectronicStructureLibrary/flook
  cd flook
  git submodule update --init --recursive

The source code of the ``flook`` module itself is contained in the ``src``
subdirectory. Lua depends on the readline library (with headers) to be
installed. Please install this library first.

.. note::
 The information contained in the *Downloading and installation* section are
 likely to work with the latest version of the source code from the repository.

 1. Create an ``obj`` directory.

 2. Create a ``Makefile`` in the ``obj`` directory containing:

    
        TOP_DIR=..
	include ../Makefile


 3. Type ``make`` to compile ``flook``, alternatively type ``make liball`` to
    create a unified library (with Lua, AOTUS and flook linked together).


Testing
_______

The ``src/test`` directory contains a number of small programs that make use of
flook. These may be useful to understand the flow of programming. You can build
and test ``flook`` with the included shell script ``quick_test.sh``.

Source Code
___________

The source code is available from the ```flook repo`` on Github <https://github.com/ElectronicStructureLibrary/flook>`_.
