

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


.. _Caesar:

##################################################################################
Caesar; a utility for calculating the vibrational free energy of periodic crystals
##################################################################################

..  contents:: :local:


Caesar calculates the vibrational free energy, and a number of related vibrational
properties, of periodic crystals.

Purpose of Module
_________________

Caesar is intended to provide a vibrational method which is more accurate than the
widely-used harmonic approximation [Hoja_ea]_ and the more sophisticated effective
harmonic approximation [Errea_ea]_, but which is computationally inexpensive enough
to be integrated into high-throughput workflows.

Caesar can calculate vibrational properties using several vibrational methods. The
:ref:`Caesar_Harmonic` performs calculations under the harmonic
approximation [Hoja_ea]_. The :ref:`Caesar_Anharmonic` performs calculations under
the vibrational self-consistent harmonic approximation (VSCHA) [Errea_ea]_ or using
vibrational self-consistent field theory (VSCF) [Christiansen]_.

In order to perform vibrational calculations, Caesar must interface with an
electronic structure code. A wide range of electronic structure codes can be
used, via the :ref:`Caesar_Electronic_Interface`.

.. [Hoja_ea] First-principles modelling of molecular crystals: structures and stabilities, temperature and pressure. https://doi.org/10.1002/wcms.1294
.. [Errea_ea] Anharmonic free energies and phonon dispersions from the stochastic self-consistent harmomic approximation: Application to platinum and palladium hydrides. https://doi.org/10.1103/PhysRevB.89.064302
.. [Christiansen] Vibrational structure theory: new vibrational wave function methods for calculation of anharmonic vibrational energies and vibrational contributions to molecular properties. https://doi.org/10.1039/B618764A

Building and Testing
____________________

The details of how to build and testing Caesar are given in the
`Caesar README.txt file <https://github.com/veryreverie/caesar>`_.

Details of how the documentation and unit tests were written are presented
in :ref:`Caesar_Docs_and_Tests`.

Compilation
-----------

An out-of-source build is recommended. For this, a clean ``build`` directory should
be made, and then `CMake <https://cmake.org/runningcmake>`_ and
`Make <https://www.gnu.org/software/make/manual/make.html>`_ should be run from the
``build`` directory, e.g. as

::

  mkdir build
  cd build
  cmake [options] path_to_src
  make

where ``[options]`` are the desired `CMake <https://cmake.org/runningcmake>`_
configuration options, and ``path_to_src`` is the path to the ``caesar/src`` directory.

Caesar has been tested using version ``10.1`` of the ``gfortran`` compiler.

Dependencies
------------

Caesar requires the `spglib <https://github.com/spglib>`_ crystal symmetry
library. `CMake <https://cmake.org/runningcmake>`_ will search ``LIB``
for `spglib <https://github.com/spglib>`_'s ``lib`` directory, and will search
``PATH`` for `spglib <https://github.com/spglib>`_'s ``include`` directory.

Caesar also requires the `BLAS <http://www.netlib.org/blas>`_ and
`LAPACK <http://www.netlib.org/blas>`_ linear algebra libraries. These are located
using `CMake <https://cmake.org/runningcmake>`_'s
`FindLAPACK <https://cmake.org/cmake/help/latest/module/FindLAPACK.html>`_ utility,
which searches a range of standard install locations and can be configured using
additional `CMake <https://cmake.org/runningcmake>`_ configuration options.

These dependencies can be suppressed by setting the
`CMake <https://cmake.org/runningcmake>`_ options ``LINK_TO_SPGLIB`` and
``LINK_TO_LAPACK`` to false, although this will disable many of Caesar's features.

Documentation and Helptext
--------------------------

The software documentation for Caesar can be generated using
`Ford <https://github.com/Fortran-FOSS-Programmers/ford>`_. This should be generated in
the ``doc`` directory, by calling

::

  ford caesar.md

Documentation will be generated in the ``doc/ford`` directory, and
``doc/ford/index.html`` can be viewed by using an html reader (e.g. a web browser).

Caesar also has its own helptext system, which can be accessed through the
``caesar`` executable by calling ``caesar --help``. This system includes helptext for
each of the modes in which Caesar can be called, including details of the input settings
for each mode.

Unit Tests
----------

The unit tests for Caesar are generated using
`pFUnit <https://github.com/Goddard-Fortran-Ecosystem/pFUnit>`_. When building with
tests, `pFUnit <https://github.com/Goddard-Fortran-Ecosystem/pFUnit>`_ becomes a
dependency of Caesar, and `CMake <https://cmake.org/runningcmake>`_ will search
``PATH`` for `pFUnit <https://github.com/Goddard-Fortran-Ecosystem/pFUnit>`_'s
``bin`` directory.

Unit tests can be run by calling

::

  ctest


from the ``build`` directory where `CMake <https://cmake.org/runningcmake>`_ was run.

Unit tests are built by default, but can be suppressed by setting the
`CMake <https://cmake.org/runningcmake>`_ option ``ENABLE_TESTS`` to false.

Output Visualisation
--------------------

Caesar uses `python <https://www.python.org>`_ scripts to visualise output data. These
can be run using Caesar, or can be run directly. When Caesar is built, the
`python <https://www.python.org>`_ scripts will be written to the ``python`` directory
within the ``build`` directory.

Performing Calculations
_______________________

Caesar is a command line utility. The behaviour of Caesar can be controlled using
command line options, a configuration file, interactive input, or a combination of
these. Detailed usage information can be obtained by calling

::

  caesar --help

Source Code
___________

The source code for Caesar is available from the
`Caesar repository <https://github.com/veryreverie/caesar>`_

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html
