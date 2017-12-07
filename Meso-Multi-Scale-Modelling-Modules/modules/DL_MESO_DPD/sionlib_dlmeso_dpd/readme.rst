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

The modifications of DL_MESO_DPD (see below) concern the subroutines dealing with the opening, writing
and closing of the trajectory files. 

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

The version of DL_MESO_DPD including SIONlib is compiled using the
corresponding makefile (``Makefile-MPI``).

The utility ``format_history_sion.f90`` is compiled with the available
Fortran90+MPI compiler, and using appropriate flags for the SIONlib library, e.g:

::
   
  mpifort -c format_history.f90 `/home/user/sionlib/bin/sionconfig --cflags --f77 --mpi --threadsafe --64`
  mpifort -o format_history-sion.exe format_history.o `/home/user/sionlib/bin/sionconfig --libs --f77 --mpi --threadsafe --64`

It is assumed that SIONlib has been installed in the `/home/user/sionlib/`
directory, where of course the `user` name has to be adapted. 


Source Code
___________

A number of DL_MESO_DPD modules have to be slightly modified to use SIONlib_ when
writing the trajectories, namely: ``variables.f90``, ``constants.f90``,
``start_module.f90``, ``dlmesodpd.f90``, ``error_module.f90`` and the
``Makefile-MPI``. As an example of the post-processing of a SIONlib
trajectory, we propose ``format_history_sion.f90``, a formatting utility
analogous to ``format_history.f90`` (see :ref:`history_format_DPD`):
it reads the SIONlib trajectory file and produces multiple formatted
trajectory files. Beside showing how to adapt the reading, this allows a robust check
of the implementation, since the output is human readable, contains the full
trajectories, and can be readily
compared with that obtained using ``format_history.f90``
with the standard version of DL_MESO_DPD.

In the following we give the needed changes in the form of patches: in the
`git diff`, `a` is the branch with the standard version, `b` the SIONlib one.

The patch for ``Makefile-MPI`` is


.. literalinclude:: ./patch-for-Makefile-MPI
      :emphasize-lines: 1,8-10,16,25,31
      :linenos:

The patch for ``variables.f90`` is

.. literalinclude:: ./patch-for-variables.f90
      :emphasize-lines: 1,9-22
      :linenos:
	 
The patch for  ``constants.f90`` is
	 
.. literalinclude:: ./patch-for-constants.f90
      :emphasize-lines: 1,10-12
      :linenos:

The patch for ``dlmesodpd.f90`` is 

.. recall to remove the check line write (nprint,*) ...

.. literalinclude:: ./patch-for-dlmesodpd.f90
      :emphasize-lines: 1,9-12
      :linenos:

The patch for ``error_module.f90`` is 

.. literalinclude:: ./patch-for-error.f90
      :emphasize-lines: 1, 9-11
      :linenos:
	 
The patch for ``start_module.f90`` is 
	 
.. literalinclude:: ./patch-for-start.f90
      :emphasize-lines:
	 1,9-11,19-23,31-41,51-57,60-66,70-76,78-84,92-110,113-131,139-145,
	 151-156, 164-170, 178-184
      :linenos:

These changes only affect one subroutine (``start``) within the ``start_module.f90``.
The user can either implement the changes shown above, or replace the
second part of the subroutine ``start`` with the file provided
(:download:`downloadable version of the second part of subroutine start <part-of-start.f90>`).

The patch for ``statistics_module.f90`` is 

.. literalinclude:: ./patch-for-statistics.f90
      :emphasize-lines: 1,9-15,22-28,35-42,50-57,63-70
      :linenos:

Also here the changes only affect one subroutine (``histout``) within the ``statistics_module.f90``.
The user can either implement the changes shown above, or replace the
subroutine ``histout`` with the file provided
(:download:`downloadable version of the subroutine histout <histout.f90>`).

Finally, the formatting utility ``format_history_sion.f90`` is

.. literalinclude:: ./format_history_sion.f90
      :language: fortran
      :linenos:



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
