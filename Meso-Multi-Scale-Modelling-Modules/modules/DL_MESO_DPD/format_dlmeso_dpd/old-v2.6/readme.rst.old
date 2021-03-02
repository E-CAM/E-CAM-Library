.. _history_format_DPD:

############################################
Formatting the HISTORY files of DL_MESO_DPD
############################################

.. sidebar:: Software Technical Information

  Language
    FORTRAN 90

  Licence
    BSD

  Documentation Tool
    RST and LaTex-generated .pdf    

  Application Documentation
    :download:`Click to download the manual <manhis.pdf>` with more details

  Relevant Training Material
    See the Testing section

.. contents:: :local:
	      

Purpose of Module
_________________

This module ``format_history.f90`` is a post-processing
utility for DL_MESO_DPD, the Dissipative Particle Dynamics (DPD) code from the DL_MESO_ package.

It converts the trajectory (HISTORY) files from *unformatted* to a
human readable form, (optionally) including explicative comments about all the
quantities. This module is mainly for learning/checking purposes.
The first aim is to help the user to check that the
system was prepared as intended (e.g., showing all the bead properties and
initial positions, all the bonds etc).
The idea is to use it on small systems when familiarizing with the structure
of input files needed for the simulation.
Secondly, it can be used as a starting point for a user-defined analysis
of trajectories.

.. Mention portability? 

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

The present module is compiled with the available Fortran90 compiler, e.g.:

``gfortran -o format.exe format_history.f90``

and the executable must be in the same directory of the HISTORY* files to be
analyzed. To test the module, run the simulation with the *toy* input files given in the following.
(Note that these files contain commented lines as suggestions for further tests.)
For the CONTROL file

.. literalinclude:: ./CONTROL

and for the FIELD file		    

.. literalinclude:: ./FIELD

After analyzing the trajectories, for a serial run (i.e., a single HISTORY
file) and for both ``lcomm`` and ``lmcheck`` set to ``.TRUE.``,
this output should be printed on the screen

.. literalinclude:: ./out-ch

and the HISTORY-F file should be
		    
.. literalinclude:: ./HISTORY-F

Source Code
___________

.. literalinclude:: ./format_history.f90
   :language: fortran
   :linenos:
		    
.. Here are the URL references used
.. _DL_MESO: http://www.ccp5.ac.uk/DL_MESO
.. _ReST: http://docutils.sourceforge.net/docs/user/rst/quickref.html

