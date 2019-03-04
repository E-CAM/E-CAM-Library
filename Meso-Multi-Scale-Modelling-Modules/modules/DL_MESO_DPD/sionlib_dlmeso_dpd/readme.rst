.. _dlmeso_sionlib:

###############################################################################
Using SIONlib (parallel I/O library) to write/read HISTORY files in DL_MESO_DPD
###############################################################################

.. sidebar:: Software Technical Information

  Language
    FORTRAN 90

  Licence
    BSD / DL_MESO_ Licence for the base code

  Documentation Tool
    RST

  Application Documentation
    See the Source Code section
  
  Relevant Training Material
    See the Testing section

.. contents:: :local:	      
	      

Purpose of Module
_________________

This module proposes to use the SIONlib_ library to write/read the trajectory (HISTORY)
files in DL_MESO_DPD, the Dissipative Particle Dynamics (DPD) code from the
DL_MESO_ package. In the last release (2.6, dating November 2015),
the MPI version of DL_MESO_DPD generates *multiple* trajectory files, one for
each MPI task. The use of SIONlib_ allows to minimally modify the writing so that just *one*
physical file (history.sion) is produced.
An analogous modification has to be implemented in the post-processing
utilities that read the HISTORY files. As an example, here the modifications
are implemented for one specific utility, ``format_history_sion.f90``, a
formatting tool analogous to ``format_history.f90`` (see :ref:`history_format_DPD`).
Beside showing how to adapt the reading, this allows a robust check
of the implementation, since the output is human readable, contains the full
trajectories, and can be readily compared with that obtained using ``format_history.f90``
with the standard version of DL_MESO_DPD.

Notice that the next released version of DL_MESO_DPD (in development)
will tackle the writing of files differently, producing a single trajectory
file from the start. However, the interface proposed here provides this feature
to the users of version 2.6, and represents an alternative solution for the
handling of the trajectories.

The implementation presented here is meant to show the feasibility of the
interfacing, not to deal with all the possible cases.
We therefore restrict in this module to the relevant case in which: i) the simulation is run in
parallel using MPI, ii) a single SIONlib-type physical file is produced, and iii) the
post-processing is done by a single process.

Finally, we would like to underline that, while SIONlib_ is optimized for a
large number of MPI tasks, the reduction from
several output files to just one is in any case a benefit, for example when it
comes to the maintenance of the simulation output.

Background Information
______________________

The base code for this module is DL_MESO_DPD, the Dissipative Particle
Dynamics code from the mesoscopic simulation package DL_MESO_,
developed by M. Seaton at Daresbury Laboratory.
This open source code is available from STFC under both academic (free) and
commercial (paid) licenses. The module is to be used with DL_MESO_
in its last released version, version 2.6 (dating November 2015).

The present module requires the SIONlib_ library to be installed.
Its last released version is number 1.7.1 (dating November 2016).

Testing
_______

The version of DL_MESO_DPD including SIONlib_ (see below) is compiled using the
corresponding makefile (``Makefile-MPI``).
Two pre-processing flags can be used when compiling:
``-D DEBUG``, to print information for any SIONlib-related action, and
``-D STDTRAJ``, to recover the standard printing of trajectories as HISTORY* files.

The utility ``format_history_sion.f90`` is compiled with the available
Fortran90+MPI compiler, and using appropriate flags for the SIONlib_ library, e.g:

::
   
  mpifort -c -cpp format_history_sion.f90 `/home/user/sionlib/bin/sionconfig --cflags --f77 --mpi --threadsafe --64`
  mpifort -o format_history_sion.exe format_history_sion.o `/home/user/sionlib/bin/sionconfig --libs --f77 --mpi --threadsafe --64`

and the executable must be in the same directory of the history.sion file. 
It is assumed that SIONlib_ has been installed in the `/home/user/sionlib/`
directory, where of course the `user` name has to be adapted.
If the pre-processing flag ``-D DEBUG`` is used when compiling, the result of each read
statement is printed to the standard output and an eventual mismatch in the
number of read elements is signaled.

To test the writing/reading of the trajectories, the user can choose any
simulation run using DL_MESO_DPD, then analyze the trajectories with both
``format_history.f90`` (which reads standard DL_MESO_DPD binary HISTORY*
files) and ``format_history_sion.f90`` (which reads the SIONlib-type history.sion file):
the formatted files so obtained, HISTORY*-F and sion*-F, respectively, should coincide.

However, for completeness, we provide the input files for a possible test: 
the CONTROL file

.. literalinclude:: ./CONTROL

and the FIELD file		    

.. literalinclude:: ./FIELD

Source Code
___________

A number of DL_MESO_DPD modules have to be slightly modified to use SIONlib_ when
writing the trajectories, namely: ``variables.f90``, ``constants.f90``,
``start_module.f90``, ``dlmesodpd.f90``, ``error_module.f90`` and the
``Makefile-MPI``. As an example of the post-processing of a SIONlib-type
trajectory, we provide the formatting utility ``format_history_sion.f90``,
analogous to ``format_history.f90`` (see :ref:`history_format_DPD`):
it reads the SIONlib trajectory file (history.sion) and produces multiple formatted
trajectory files (sion*-F).

In the following we give the needed changes in the form of patches [1]_: in the
`git diff`, `a` is the branch with the standard version (version 2.6, revision
15 [2]_), `b` the SIONlib one.

The patch for ``Makefile-MPI`` is


.. literalinclude:: ./patch-for-Makefile-MPI
      :emphasize-lines: 1,8-10,13,17,26,32
      :linenos:

The patch for ``variables.f90`` is

.. literalinclude:: ./patch-for-variables.f90
      :emphasize-lines: 1,9-22
      :linenos:
	 
The patch for  ``constants.f90`` is
	 
.. literalinclude:: ./patch-for-constants.f90
      :emphasize-lines: 1,9-10
      :linenos:

The patch for ``dlmesodpd.f90`` is 

.. literalinclude:: ./patch-for-dlmesodpd.f90
      :emphasize-lines: 1,10,12-18
      :linenos:

The patch for ``error_module.f90`` is 

.. literalinclude:: ./patch-for-error.f90
      :emphasize-lines: 1, 9-13,21,23-26
      :linenos:
	 
The patch for ``start_module.f90`` is 
	 
.. literalinclude:: ./patch-for-start.f90
      :emphasize-lines: 1,9-11,19-22,31,34-47,55,57,59-69,71,75-87,89-100,102-112,119,
			121-147,149,151-177,184,186-196,201,203-212,220,222-232,240,242-252
      :linenos:

These changes only affect one subroutine (``start``) within the ``start_module.f90``.
The user can either implement the changes shown above, or replace the
second part of the subroutine ``start`` with the file provided
(:download:`downloadable version of the second part of subroutine start <part-of-start.f90>`).

The patch for ``statistics_module.f90`` is 

.. literalinclude:: ./patch-for-statistics.f90
      :emphasize-lines: 1,9,18,20-30,36,38-48,53,56-67,73,76-87,91,94-105,112,116
      :linenos:

Also here the changes only affect one subroutine (``histout``) within the ``statistics_module.f90``.
The user can either implement the changes shown above, or replace the
subroutine ``histout`` with the file provided
(:download:`downloadable version of the subroutine histout <histout.f90>`).

Finally, the formatting utility ``format_history_sion.f90`` is

.. literalinclude:: ./format_history_sion.f90
      :language: fortran
      :linenos:


**Additional information** 

Using the modifications proposed above, the trajectories will be written
by DL_MESO_DPD in the SIONlib format (history.sion file). For comparison
purposes, the standard HISTORY* files can be also written at the same time,
using the ``-D STDTRAJ`` flag for compilation in ``Makefile-MPI``.

To be able to use SIONlib, the writing statements for which the records are formed
by inhomogeneous items (e.g., two 8-byte strings and a 4-byte integer)
have to be split into different records, hence the increased number of write/read statements.
To help the reader, comments have been added to label all
the SIONlib-related commands, namely: "SIONlib 0, 1a, 1b, 2a, 2b, ..., 2p, 3".
The writing statements are labelled "2a, 2b, etc", and each one
corresponds to the writing of single record in the standard version of
DL_MESO_DPD. The SIONlib file definition, opening and closing statements have been labelled 0, 1 and 3 in the comments.

Important SIONlib variables:

- ``fsblksize``: file system block size in bytes. If set to -1, it is read by
  SIONlib. (Typically, this value is 4096.)

- ``chunksize``: size in bytes of the data written by a task in a single write
  call. It is internally increased by SIONlib to the next multiple of the
  filesystem block size. (For DL_MESO_DPD, the largest record has size 80 bytes, hence we choose
  chunksize = 100, which, typically, will be internally increased to 4096.)

- ``nfiles``: number of physical files produced by SIONlib (set to 1 here).

**Acknowledgements** 

We are very grateful to Dr. Wolfgang Frings for kind support concerning the usage of
the Fortran version of SIONlib_.

.. Here are the URL references used
.. _DL_MESO: http://www.ccp5.ac.uk/DL_MESO
.. _SIONlib: http://www.fz-juelich.de/ias/jsc/EN/Expertise/Support/Software/SIONlib/_node.html
.. [1] If patching is done with GNU `patch` command, the `-l` option (ignoring
       whitespaces) has to be active.
.. [2] On CCPForge_, a software development framework where, in particular,
       the different versions of DL_MESO_DPD are stored,
       version 2.6 in its revision 15 corresponds to the commit number
       48e9a42a51f4cb450eb9c39dcbf6eb4a38c7cd32.
.. _CCPForge: https://ccpforge.cse.rl.ac.uk/gf/
	 
.. the subroutines dealing with the opening, writing and closing of the trajectory files. 
