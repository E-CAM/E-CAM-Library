############
MatrixSwitch
############

.. sidebar:: Software Technical Information

  The information in this section describes MatrixSwitch as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Language
    Fortran 2008

  Documentation Tool
    Sphinx, ReStructuredText

  Application Documentation
   `ESL wiki <http://esl.cecam.org/MatrixSwitch>`_ 

  Relevant Training Material
    See usage examples in the ``examples`` directory of the source code.
  
  Licence
    Simplified BSD

.. contents:: :local:

Purpose of Module
_________________

MatrixSwitch is a module which acts as an intermediary interface layer between
high-level routines for physics-related algorithms and low-level routines
dealing with matrix storage and manipulation. This allows the high-level
routines to be written in a way which is physically transparent, and enables
them to switch seamlessly between different software implementations of the
matrix operations.

Background Information
______________________

MatrixSwitch is a software library and module to be used within a calling code.
It is developed within the same repository project as other libraries (see
Source Code Section), but all are self-contained within separate directories.

Installation
____________

The source code of the `MatrixSwitch` module is bundled in the git repository of
the ``omm-bundle`` software which you can obtain using ``git``::

  git clone https://gitlab.e-cam2020.eu/ESL/omm.git

The source code of the `MatrixSwitch` module itself is contained in a
subdirectory with the same name, ``MatrixSwitch``.

.. note::
 The information contained in the *Installation* and *Testing* sections are
 likely to work with the latest version of the source code from the repository.
 If this is not the case you can revert to the commit where the information is
 guaranteed to work::

   git checkout 919d916f
 

1. Enter the ``src`` directory.

2. Copy ``make.inc.example`` to ``make.inc`` and modify it to suit your needs.
   Available options for ``FPPFLAGS`` are:

   * ``-DHAVE_MPI``: enable MPI parallel routines
   * ``-DHAVE_LAPACK``: enable LAPACK routines
   * ``-DHAVE_SCALAPACK``: enable ScaLAPACK routines (requires MPI)
   * ``-DHAVE_PSPBLAS``: enable to link to pspBLAS (requires pspBLAS installed at first)
   * ``-DCONV``: enable automatic conversion of scalar types (real/complex) to
     agree with matrix definitions (real/complex). Note that conversions from
     complex to real will simply discard the imaginary part.

3. Type ``make -f Makefile.manual``.

4. Type ``make -f Makefile.manual install``.

.. note::

 We provide also the possibility to build modules with Autotools. You can find the related documentation in the following files: `omm-bundle <https://gitlab.e-cam2020.eu/ESL/omm>`_ and `MatrixSwith/doc <https://gitlab.e-cam2020.eu/ESL/omm/tree/master/MatrixSwitch/doc>`_

Testing
_______

The ``examples`` directory contains a number of small programs that make use of
MatrixSwitch. These can be useful both for testing the installation and for
learning how to use the library. To compile them:

1. Enter the ``examples`` directory.

2. Copy ``make.inc.example`` to ``make.inc`` and modify it to suit your needs.
   Be aware that ``make.inc`` in the ``src`` directory will also be used.

3. Type ``make -f Makefile.manual``.

Each example contains a header explaining what the program does and providing
sample output to compare against.


Source Code
___________

The source code is available from the `E-CAM Gitlab`__ under the `omm-bundle`__
project. The MatrixSwitch directory can be found `here`__.

.. __: https://gitlab.e-cam2020.eu/
.. __: https://gitlab.e-cam2020.eu/ESL/omm/
.. __: https://gitlab.e-cam2020.eu/ESL/omm/tree/master/MatrixSwitch
