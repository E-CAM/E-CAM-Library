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



Source Code
___________
.. literalinclude:: ./check_config.f90
   :language: fortran
   :linenos:

.. Here are the URL references used
.. _DL_MESO: http://www.ccp5.ac.uk/DL_MESO
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
