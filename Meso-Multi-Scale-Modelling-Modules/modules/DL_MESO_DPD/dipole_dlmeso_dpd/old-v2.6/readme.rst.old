.. _dipole_moments:

################################################
Analysis of charge dipole moments in DL_MESO_DPD
################################################

.. sidebar:: Software Technical Information

  Language
    FORTRAN 90

  Licence
    BSD

  Documentation Tool
    RST and LaTex-generated .pdf file

  Application Documentation
    :download:`Click to download the manual <mandip.pdf>` with more details

  Relevant Training Material
    See the Testing section

.. contents:: :local:	      
	      

Purpose of Module
_________________

This module, ``gen_dipole.f90``, is a generalization of the ``dipole.f90`` post-processing
utility of DL_MESO_DPD, the Dissipative Particle Dynamics (DPD) code from the DL_MESO_ package.

It processes the trajectory (HISTORY) files to obtain the charge dipole moments
of all the (neutral) molecules in the system.
It produces files `dipole_*` containing the time evolution of relevant
quantities (see below). In the case of a single molecular species, it also prints
to the standard output the Kirkwood number :math:`g_k` and the relative electric
permittivity :math:`\epsilon_r` for this species, together with an estimate for their errors (standard error).

The module can be applied to systems including molecules with a generic charge structure, as long
as each molecule is neutral (otherwise the charge dipole moment would be frame-dependent).

The charge dipole moment of a neutral molecule is :math:`\vec{p}_{mol}=\sum_{i\in mol}q_i \vec{r}_i` where
:math:`\vec{r}_i` are the bead positions and :math:`q_i` their charges. The
total charge dipole moment of the simulated volume :math:`V` is
:math:`\vec{P}=\sum_{mol\in V} \vec{p}_{mol}`.
If more than one molecular species are present, one can split :math:`\vec{P}` into the different species contributions.

In general:

For any molecular species a file `dipole_{molecule name}` is produced, whose columns are
:math:`\textrm{snapshot index}, P_x, P_y, P_z, \sum_{i=1}^{N_{mol}}\frac{\vec{p}_i ^2}{N_{mol}},\frac{\vec{P} ^2}{V}`.
It is intended that for any quantity the contribution given *from the
species {molecule name}* is reported (i.e., the sums are restricted to
molecules of a single type).

Possible uses of the output files are: monitoring the polarization in response to an
external electric field, measuring the fluctuations in molecular/total charge
dipole moments.

Extra output for a single molecular species:

The Kirkwood number for a pure system is
:math:`g_k=\frac{\langle\vec{P}^2\rangle}{N_{mol}\langle \vec{p}^2\rangle}`,
where :math:`\langle\dots\rangle` indicates an average over trajectories.
If the dipoles' orientations are not correlated, then :math:`g_k\simeq 1`.
Also, the relative dielectric permittivity of the medium is calculated from linear response
theory: :math:`\epsilon_r= 1 + \frac{4 \pi}{3} l_B \frac{\langle\vec{P}^2\rangle}{V}`,
where :math:`l_B` is Bjerrum length and tin-foil boundary conditions are used.

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

The present module ``gen_dipole.f90`` is compiled with the available Fortran90 compiler, e.g.:

``gfortran -o gen_dipole.exe gen_dipole.f90``

and the executable must be in the same directory of the HISTORY* files to be
analyzed. The user is asked to provide the number of nodes used to run the
simulation, the electric charges for all the types of beads
and the Bjerrum length.

To input the user-defined parameters one can enter them from the keyboard or write them into a text file (say, input.txt), one per line
(in the right order) and run the program in this way:

``gen_dipoleaf.exe < input.txt``

We propose two tests to familiarize with the utility and a third one on a
physically relevant system.

The first two tests involve two (toy) molecular species: a
branched one (four beads, T-shaped) and a simple dimer. All the beads carry charges.
In the first case 10 molecules of each type are present and are followed for a few time steps.
In the second case it is suggested to analyze a single snapshot with just two
molecules and all the beads sitting at user-defined positions (via the CONFIG
file).

Four type of beads are used with charges :math:`q_A=0.2, q_B=-1, q_C=0.6,q_D=1`;
the Bjerrum length is fixed as :math:`l_B=1`.

The bonding connections in the two molecules are pictorially given below:

::

       B - A - C    B - D
           |
           A

**First test**

Run the DL_MESO_DPD simulation on a single node (serial run)
using for the CONTROL file

.. literalinclude:: ./CONTROL

and for the FIELD file

.. literalinclude:: ./FIELD

		    
Analyzing the HISTORY file with `gen_dipole.exe`, this output is printed on the standard output
		    
.. literalinclude:: ./out-d

The first line shows the histogram of cluster sizes: in this case,
it correctly gives 10 molecules of two beads, and 10 molecules of 4 beads.
Since internally the module checks that each molecule is a connected cluster [1]_,
this line should always give a histogram with the molecule sizes
(by default, shown up to ten beads).
		    
The `dipole_BD` file is

.. literalinclude:: ./dipole_BD

and the `dipole_BRANCH` one is

.. literalinclude:: ./dipole_BRANCH

If instead the simulation is run on multiple nodes, only the results for the
first snapshot will be unchanged (i.e., first line of each `dipole_*` file),
the other results will vary because a different sequence of random numbers
will enter the time evolution of the system. 

**Second test**

Run DL_MESO_DPD using the same CONTROL and FIELD files as above, with the only changes:

- `"steps 1000"` changes into `"steps 1"`
- `"nummols 10"` changes into `"nummols 1"` (NB: appears twice)

Also, use this CONFIG file that will initially put the molecules
branches aligned with the cartesian axes

.. literalinclude:: ./CONFIG

where the identity of each bead is fixed by the FIELD file and is shown below

::

    B(1) - A(2) - C(3)    B(5) - D(6)
            |
           A(4)

One can easily check that the dipole of each molecule is as expected:

 :math:`\vec{p}_{BRANCH}=(0.04,0.32,0), \quad   \vec{p}_{BD}=(0,0,-0.2)~.`

In fact: the `dipole_BD` file is

.. literalinclude:: ./dipole_BD-2

and the `dipole_BRANCH` one is

.. literalinclude:: ./dipole_BRANCH-2
		    
The results of this test do not depend on the number of nodes used to run the simulation.
		    
**Third test: water in oil**

Here we suggest to consider a fluid made of harmonically bonded dimers
:math:`(+q,-q)`. Fixing appropriately the partial charge :math:`q`
and the Bjerrum length :math:`l_B` this system
mimics water in an oil background, as long as the dielectric properties
are concerned. For more details about this model, please see the page :ref:`dimers`.

Run DL_MESO_DPD using for the CONTROL file

.. literalinclude:: ./CONTROL-3

and for the FIELD file

.. literalinclude:: ./FIELD-3

Analyzing the HISTORY file with `gen_dipole.exe`, this output is printed on
the standard output

.. literalinclude:: ./out-3

In particular, we see that:

- :math:`\vec{P}=(0.0 \pm 0.1, 0.2 \pm 0.1, 0.1 \pm 0.1)`
- :math:`\epsilon_r= 43 \pm 2`
- :math:`g_k = 1.12 \pm 0.04`

Please notice that the error estimates are done assuming all the samples are
independent. From the results obtained in the testing case of the
module `gen_dipoleaf.f90`, one sees that the auto-correlation time of
:math:`\vec{P}` in this system is about 1-2 DPD time units, so the sampling choice
done here (trajectories are written every 100 time steps, i.e., every 1 DPD
time units) seems reasonable, even if a bit optimistic.
To confirm the reliability of the error estimate one can do
another run with a different random number sequence (see the directive `seed` of
DL_MESO) and see if the two results are compatible within error bars.

  

Source Code
___________
.. literalinclude:: ./gen_dipole.f90
   :language: fortran
   :linenos:

.. Here are the URL references used
.. _DL_MESO: http://www.ccp5.ac.uk/DL_MESO
.. _ReST: http://docutils.sourceforge.net/docs/user/rst/quickref.html
.. [1] Disambiguation on the concept of molecule. In DL\_MESO a *defined molecule*
         is a set of beads, which can be bonded or not.
         For the purpose of this module it is *required* that each molecule is a
	 connected cluster (via stretching bonds).
	 In fact, this, together with the reasonable assumption that each stretching
	 bond cannot be stretched to more than half the system linear size, allows
	 to univocally define the charge dipole moment of each molecule.
