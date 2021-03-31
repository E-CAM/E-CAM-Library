

..  sidebar:: Software Technical Information

  Name
    Caesar

  Language
    Fortran

  Licence
    `GNU Lesser General Public License version 3 <https://www.gnu.org/licenses>`_

  Documentation Tool
    `Ford <https://github.com/Fortran-FOSS-Programmers/ford>`_

  Application Documentation
    See the `Caesar repository <https://github.com/veryreverie/caesar>`_

  Software Module Developed by
    Mark Johnson


.. _Caesar_Docs_and_Tests:

##################################
Caesar - Documentation and Testing
##################################

..  contents:: :local:


Purpose of Module
_________________

The features described in this module aim to make :ref:`Caesar <Caesar>` easier to use,
maintain and develop, by anyone who is interested in doing so.

Unit Tests
----------

:ref:`Caesar <Caesar>` uses
`pFUnit <https://github.com/Goddard-Fortran-Ecosystem/pFUnit>`_ to build and run unit
tests. The unit test files used by
`pFUnit <https://github.com/Goddard-Fortran-Ecosystem/pFUnit>`_ are preprocessed into
fortran, and so the unit tests can be built and run alongside the rest of the code,
using `CMake <https://cmake.org/runningcmake>`_.

Each module file ``*.f90`` has an accompanying test file, ``*_test.pf``, containing the
test procedures for that module. It is impractical for the unit tests to be exhaustive,
and so they aim to cover several standard use cases as well as any obvious edge cases.
In particular, use cases which were known to cause bugs in previous versions of Caesar
are included in the unit tests, as a form of regression testing.

Documentation
-------------

The documentation for :ref:`Caesar <Caesar>` is generated using
`Ford <https://github.com/Fortran-FOSS-Programmers/ford>`_. Ford generates
documentation directly from the fortran source code, and so the documentation
for each procedure is written in the same place as the interface for that procedure.

Each procedure has documentation describing what the procedure does, what the input and
output arguments to the procedure are, and what happens in the case of an error. The
details of how each procedure works are presented separately, as code comments in the
implementation of each procedure.

Helptext
--------

:ref:`Caesar <Caesar>` uses a custom system to process the input arguments for the
various user-accessible procedures.

Each input argument is defined using a ``KeywordData`` type, which stores the name of
the argument, a helptext string describing the argument, and relevant metadata
including whether or not the argument is optional, and what the argument's default
value is, if relevant.

Once the array of input arguments for a given procedure has been created, it is passed
to the input parsing system. The input parsing system then gets the value of each
argument from the user, either from command line arguments, from an input file, or
from an interactive procedure which describes each argument to the user in turn, and
prompts them to input the argument values.

Once the inputs are parsed, the relevant :ref:`Caesar <Caesar>` subroutine is called,
with a dictionary-style container, containing the input arguments as key-value pairs.
Each input argument can then be accessed by name, and the container returns the string
representation of the argument's value, which can be parsed by the subroutine.

In addition to driving the input parser, the list of arguments is used to generate
helptext. Calling ``caesar [mode] --help`` generates the list of arguments for the
requested ``mode``, and prints the helptext for each argument in turn.

Submodules
----------

As part of the process of writing documentation and unit tests, every procedure in
:ref:`Caesar <Caesar>` was separated into an interface and and implementation, through
the use of fortran
`submodules <https://software.intel.com/content/www/us/en/develop/blogs/doctor-fortran-in-we-all-live-in-a-yellow-submodule.html>`_.
This has a number of advantages:

 - Submodules allow for circular dependencies which break free from fortran's strict
   module hierarchy. This allow modules to be separated into inter-dependent libraries,
   grouped together based on their functionality rather than their dependencies.
 - Circular dependencies make it much easier to add new implementations of abstract
   classes, as functions which "know about" (i.e. have dependencies on) all of the
   class implementations can be called by the methods of the parent class. This means
   that :ref:`Caesar <Caesar>` can easily be updated to use new potential and state
   representations, sampling methods, and convergence algorithms, without requiring
   future developers to modify the code in more than a couple of places.
 - The documentation for the procedure interfaces and the procedure implementations
   is separated. This allows for easy browsing of interface documentation, with
   implementation documentation hidden until needed.
 - Unit tests can be checked for code coverage by comparing the test files with the
   interface files, without having to consider the implementation files.
 - When procedure implementations are modified, only the modified submodules need to
   be re-compiled. This avoids the compilation cascades which are endemic to module-only
   fortran projects, and dramatically reduces total compilation time for developers.

Source Code
___________

The source code for Caesar is available from the
`Caesar repository <https://github.com/veryreverie/caesar>`_. The source code for the
helptext system is found in the ``src/common/arguments`` directory of this repository.

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html
