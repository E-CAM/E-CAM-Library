##################
DBCSR@MatrixSwitch
##################

.. sidebar:: Software Technical Information

  The information in this section describes `DBCSR@MatrixSwitch` as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Language
    Fortran 2008

  Documentation Tool
    Sphinx, ReStructuredText

  Application Documentation
   `ESL wiki <http://esl.cecam.org/MatrixSwitch>`_

  Relevant Training Material
    See a usage example in the ``omm/MatrixSwitch/examples`` directory of the source code.

  Software Module Developed by
    Alfio Lazzaro and David López-Durán

  Licence
    Simplified BSD

.. contents:: :local:

Purpose of Module
_________________

`MatrixSwitch` is a module which acts as an intermediary interface layer between
high-level and low-level routines
dealing with matrix storage and manipulation. It allows a seamlessly switch
between different software implementations of the matrix operations.

`DBCSR` is an optimized library to deal with sparse matrices, which appear
frequently in many kind of numerical simulations. In `DBCSR@MatrixSwitch`
`DBCSR` capabilities have been added to `MatrixSwitch` as an *optional*
library dependency.

Background Information
______________________

`MatrixSwitch`, `DBCSR`, and `DBCSR@MatrixSwitch` are software libraries
to be used within a calling code.
`MatrixSwitch` has been developed within the same repository of other
self-contained libraries,
all them collected in the `omm-bundle` project (see the `Source Code`_ section below).
As `DBCSR` has been added to `MatrixSwitch`
in a modular way, all them can be used together or separated.

To carry out calculations in serial mode may be too slow sometimes and a paralellisation
strategy is needed. In serial/parallel `MatrixSwitch` employs Lapack/ScaLapack to perform
matrix operations, irrespective of their dense or sparse character.
The disadvantage of the Lapack/ScaLapack schemes is that they are not optimized
for sparse matrices. `DBCSR` provides the necessary algorithms to solve this problem and
in addition is specially suited to work in parallel.

Installation
____________

The source code of the `MatrixSwitch` module is contained in a subdirectory of
of the `omm-bundle` package with the same name, ``omm/MatrixSwitch``.
'omm-bundle' is in a ``git`` repository and can be obtained in this way:

  git clone https://gitlab.e-cam2020.eu/esl/omm.git

The `DBCSR` library was originally developed as part of the `CP2K`__ code, it is now
available as a standalone library, and can be found in the ``CP2K`` releases directory:

.. __: https://www.cp2k.org/

  https://github.com/cp2k/dbcsr/releases/download/v1.0.0/dbcsr-1.0.0.tar.gz

`Build instructions for the DBCSR library <https://github.com/cp2k/dbcsr#dbcsr-distributed-block-compressed-sparse-row-matrix-library>`_ are available on the project page.

To enable `DBCSR@MatrixSwitch` in the `omm-bundle` package follow the steps below:

  1. Enter the ``omm`` directory.

  2. Copy ``make.inc.example`` to ``make.inc`` and modify it to suit your needs. To use `DBCSR` in `MatrixSwitch` include in your ``make.inc`` the path to the `DBCSR` library and add to ``FPPFLAGS`` the new flag ``-DHAVE_DBCSR`` (this requires that ``-DHAVE_MPI`` is also enabled).

  3. Type ``make -f Makefile.manual``.

  4. Type ``make -f Makefile.manual install``.

Testing
_______

The ``examples`` directory of ``MatrixSwitch`` contains ``example_pdcsr_pddbc.F90``. It explains
the use of `DBCSR@MatrixSwitch` and how `DBCSR` works. `DBCSR` results are compared to those
obtained with Scapalack to check the validity of the new procedure. If this comparison fails,
the program will exit immediately. To compile it:

1. Enter the ``omm/MatrixSwitch/examples`` directory.

2. Copy ``make.inc.example`` to ``make.inc`` and modify it to suit your needs.
   Be aware that ``make.inc`` in the ``src`` directory will also be used.

3. Type ``make -f Makefile.manual``.

As in the other examples in `MatrixSwitch`, ``example_pdcsr_pddbc.F90`` contains a header
explaining what the program does and provides a sample output to compare with.

Source Code
___________

In the `E-CAM Gitlab`__ can be found all the source codes of `MatrixSwitch`__
and `omm-bundle`__, while `DBCSR`__ itself is in the set of `CP2K`__ `Github`__ repositories.

.. __: https://gitlab.e-cam2020.eu/
.. __: https://gitlab.e-cam2020.eu/esl/omm/tree/master/MatrixSwitch/
.. __: https://gitlab.e-cam2020.eu/esl/omm/
.. __: https://github.com/cp2k/dbcsr/
.. __: https://github.com/cp2k/
.. __: https://github.com/
