.. _check_config:

#####################################################
[WIP] Consistency check of input files in DL_MESO_DPD
#####################################################

.. sidebar:: Software Technical Information

  Language
    FORTRAN 90

  Licence
    BSD

  Documentation Tool
    RST and LaTex-generated .pdf file

  Application Documentation
    :download:`Click to download the manual <manchk.pdf>` with more details

  Relevant Training Material
    See the Testing section

.. contents:: :local:	      
	      

Purpose of Module
_________________

This module, ``check_config.f90``, is a pre-processing
utility for DL_MESO_DPD, the Dissipative Particle Dynamics (DPD) code from the DL_MESO_ package.
It checks that the content of the optional configuration (CONFIG) file is consistent with that
of the necessary input files (CONTROL and FIELD). In particular, it checks: the system
dimensions, its composition and the bead content of all the molecules. 
In addition, in case hard walls are present, it checks that none of the
stretching bonds between beads crosses a hard wall.


Background Information
______________________

The base code for this module is DL_MESO_DPD, the Dissipative Particle
Dynamics code from the mesoscopic simulation package DL_MESO_,
developed by M. Seaton at Daresbury Laboratory.
This open source code is available from STFC under both academic (free) and
commercial (paid) licenses. The module is to be used with DL_MESO
in its last released version, version 2.6 (dating November 2015).

Testing
_______

The present module, ``check_config.f90``, is compiled with the available
Fortran90 compiler [1]_, e.g.:

``gfortran -o check_config.exe check_config.f90``

and the executable must be in the same directory of the three files to be
analyzed (i.e., CONTROL, FIELD and CONFIG).


When running ``check_config.f90``, the outcome of the different checks is
sent to the standard output. The most important messages are: warnings,
error messages and hints to fix them. For completeness, some information
about the system size and composition is printed too. 

We suggest as a test a very small system with three species of beads (A, B, C)
and a total population of 24 beads. Of these, 6 are unbonded, while the others
are grouped into 7 molecules of two types.
In the first test, consistent input is given. In the following ones, small
changes rising warnings and errors are analyzed, to demonstrate the behaviour
of the module.

**Test 1**

Use for the CONTROL file

.. literalinclude:: ./CONTROL

for the FIELD file

.. literalinclude:: ./FIELD

and for the CONFIG file this (correct labeling) one, where
the beads are randomly located in the cubic box

.. literalinclude:: ./CONFIG

Running the utility ``check_config.f90``, this output is printed on the
standard output

.. literalinclude:: ./out-1

**Test 2**

Instead, altering just two particle species in the CONFIG file given above:

- `"B   3"`  changes into `"A   3"`
- `"C  20"` changes into `"B 20"`

an error message is given

.. literalinclude:: ./out-2
   :lines: 19-25

**Test 3**

If instead these two lines of the CONFIG file are altered

- `"A   10"`  into `"C   10"`
- `"C  11"` into `"A  11"` 

the error message is

.. literalinclude:: ./out-3
   :lines: 19-22

**Test 4**

Here instead we propose to add a hard wall orthogonal to the `z` axis: this
is done uncommenting the ``surface hard z`` line in the CONTROL file.
Running the utility, one obtains

.. literalinclude:: ./out-4
   :lines: 7-8, 23-26


Source Code
___________
.. literalinclude:: ./check_config.f90
   :language: fortran
   :linenos:

.. Here are the URL references used
.. _DL_MESO: http://www.ccp5.ac.uk/DL_MESO
.. _ReST: http://docutils.sourceforge.net/docs/user/rst/quickref.html
.. [1] Compilation has been tested with the GNU compiler GCC, version 8.1.1.
