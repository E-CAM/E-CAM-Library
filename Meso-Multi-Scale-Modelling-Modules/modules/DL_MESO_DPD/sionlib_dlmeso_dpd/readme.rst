.. _dlmeso_sionlib:

#####################################################################################
[WIP] Using SIONlib (parallel I/O library) to write/read HISTORY files in DL_MESO_DPD
#####################################################################################

.. sidebar:: Software Technical Information

  Language
    FORTRAN 90

  Licence
    BSD

  Documentation Tool
    RST and LaTex-generated .pdf file

  Application Documentation
..    :download:`Click to download the manual <manaf.pdf>` with more details

  Relevant Training Material
    See the Testing section

.. contents:: :local:	      
	      

Purpose of Module
_________________

This module proposes to use the SIONlib_ library to write/read the trajectory (HISTORY)
files in DL_MESO_DPD, the Dissipative Particle Dynamics (DPD) code from the
DL_MESO_ package. In the last release (2.6, dating November 2015),
the MPI version of DL_MESO_DPD generates *multiple* trajectory files, one for each
process. The use of SIONlib_ allows to minimally modify the writing so that just *one*
physical file is produced.

An analogous modification has to be implemented in the post-processing
utilities that read the HISTORY files. As an example, here the modifications
are implemented for one specific utility, ``format_history.f90``.

Notice that the next released version of DL_MESO_DPD (in development)
will tackle the writing of files differently, producing a single trajectory
file from the start.

The implementation presented here is meant to show the feasibility of the
interfacing, not to tackle all the possible cases.
We therefore restrict in this module to the relevant case in which i) the simulation is run in
parallel using MPI, ii) a single SIONlib physical file is produced, and iii) the
post-processing is done by a single process.



 .. Possible uses ... (see :ref:`moldip_af`).

Background Information
______________________

The base code for this module is DL_MESO_DPD, the Dissipative Particle
Dynamics code from the mesoscopic simulation package DL_MESO_,
developed by M. Seaton at Daresbury Laboratory.
This open source code is available from STFC under both academic (free) and
commercial (paid) licenses. The module is to be used with DL_MESO
in its last released version, version 2.6 (dating November 2015).

The present module requires the SIONlib_ library to be installed.
Its last released version is number 1.7.1 (dating November 2016).

Testing
_______


Source Code
___________
..
   .. literalinclude:: ./gen_dipoleaf.f90
      :language: fortran
      :linenos:

.. Here are the URL references used
.. _DL_MESO: http://www.ccp5.ac.uk/DL_MESO
.. _SIONlib: http://www.fz-juelich.de/ias/jsc/EN/Expertise/Support/Software/SIONlib/_node.html
.. _ReST: http://docutils.sourceforge.net/docs/user/rst/quickref.html
..
   .. _FFTW: http://www.fftw.org/
   .. [1] Disambiguation on the concept of molecule. In DL\_MESO a *defined molecule*
	    is a set of beads, which can be bonded or not.
	    For the purpose of this module it is *required* that each molecule is a
	    connected cluster (via stretching bonds).
	    In fact, this, together with the reasonable assumption that each stretching
	    bond cannot be stretched to more than half the system linear size, allows
	    to univocally define the charge dipole moment of each molecule.
   .. [2] M. P. Allen and D. J. Tildesley, "Computer simulation of liquids", Oxford University Press, Oxford (1987).
