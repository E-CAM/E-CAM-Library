######
libOMM
######

.. sidebar:: Software Technical Information

  Language
    Fortran 2008

  Documentation Tool
    Sphinx, ReStructuredText

  Application Documentation
   `ESL wiki <http://esl.cecam.org/LibOMM>`_ 

  Relevant Training Material
    See usage examples in the ``examples`` directory of the source code.
  
  Licence
    Simplified BSD

.. contents:: :local:

Purpose of Module
_________________

libOMM solves the Kohn-Sham equation as a generalized eigenvalue problem for a
fixed Hamiltonian. It implements the orbital minimization method (OMM), which
works within a density matrix formalism. The basic strategy of the OMM is to
find the set of Wannier functions (WFs) describing the occupied subspace by
direct unconstrained minimization of an appropriately-constructed functional.
The density matrix can then be calculated from the WFs. The solver is usually
employed within an outer self-consistency (SCF) cycle. Therefore, the WFs
resulting from one SCF iteration can be saved and then re-used as the initial
guess for the next iteration.

Background Information
______________________

libOMM is a software library to be used within a calling code. It is built on
top of the MatrixSwitch library for dealing with matrix storage and operations.
Both libraries are developed within the same repository project (see
`Source Code`_), but are self-contained within separate directories.

Software Technical Information
______________________________

License
  Simplified BSD

Language
  Fortran 2008

Documentation Tool
  Source code documentation in progress.

Application Documentation
  `The ESL wiki <http://esl.cecam.org/libOMM>`_

Relevant Training Material
  See usage examples in the ``examples`` directory of the source code.

Installation
____________

The source code of the `LibOMM` module is bundled in the git repository of
the ``omm-bundle`` software which you can obtain using ``git``::

  git clone https://gitlab.e-cam2020.eu/ESL/omm.git

The source code of the `LibOMM` module itself is contained in a
subdirectory with the same name, ``LibOMM``.

.. note::
 The information contained in the *Installation* and *Testing* sections are
 likely to work with the latest version of the source code from the repository.
 If this is not the case you can revert to the commit where the information is
 guaranteed to work::

   git checkout  7eda3275

1. Enter the ``src`` directory.

2. Copy ``make.inc.example`` to ``make.inc`` and modify it to suit your needs.
   ``MSLIBPATH`` should point to the MatrixSwitch directory (default in
   ``make.inc.example`` is for the version included in the distribution).
   LibOMM should be compiled with the ``-DCONV`` flag. Some available options
   for ``FPPFLAGS`` are:

   * ``-DHAVE_MPI``: enable MPI parallel routines
   * ``-DHAVE_LAPACK``: enable LAPACK routines (currently necessary for
     preconditioning/Cholesky factorization)
   * ``-DHAVE_SCALAPACK``: enable ScaLAPACK routines (requires ``-DMPI``)
   * ``-DNORAND``: fixed seed for the random number generator. Enable for
     testing purposes.
   * ``-DCBIND``: use ISO_C_BINDING for LOGICAL inputs in the wrapper
     interfaces. Enable for linking to C.

3. Type ``make -f Makefile.manual``.

4. Type ``make -f Makefile.manual install``.

.. note:: We provide also the possibility to build modules with Autotools. You can find the related documentation in the following files:`omm-bundle <https://gitlab.e-cam2020.eu/ESL/omm>`_ and `LibOMM/doc <https://gitlab.e-cam2020.eu/fcorsetti/omm/tree/master/libOMM/doc>`_


Testing
_______

The ``examples`` directory contains a number of small programs that make use of
libOMM with MatrixSwitch. These can be useful both for testing the installation
and for learning how to use the library. To compile them:

1. Enter the ``examples`` directory.

2. Copy ``make.inc.example`` to ``make.inc`` and modify it to suit your needs.
   Be aware that ``make.inc`` in the ``src`` directory will also be used.

3. Type ``make -f Makefile.manual``.

Each example contains a header explaining what the program does and providing
sample output to compare against.

Source Code
___________

The source code is available from the `E-CAM Gitlab`__ under the `omm-bundle`__
project. The libOMM directory can be found `here`__.

.. __: https://gitlab.e-cam2020.eu/
.. __: https://gitlab.e-cam2020.eu/ESL/omm/
.. __: https://gitlab.e-cam2020.eu/ESL/omm/tree/master/libOMM
