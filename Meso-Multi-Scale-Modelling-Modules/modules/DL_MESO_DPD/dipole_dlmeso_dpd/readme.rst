.. _dipole_moments:

################################################
Analysis of charge dipole moments in DL_MESO_DPD
################################################

.. sidebar:: Software Technical Information

  Language
    Fortran 2003

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

It processes trajectory (HISTORY) files to obtain the charge dipole moments
of all the (neutral) molecules in the system.
It produces files `dipole_*` containing the time evolution of relevant
quantities (see below). In the case of a single molecular species, it also prints
to the standard output the Kirkwood number :math:`g_k` and the relative electric
permittivity :math:`\epsilon_r` for this species, together with an estimate for
their errors (standard deviation).

The module can be applied to systems including molecules with a generic charge structure, as long
as each molecule is neutral (otherwise the charge dipole moment would be frame-dependent).

The charge dipole moment of a neutral molecule is :math:`\vec{p}_{mol}=\sum_{i\in mol}q_i \vec{r}_i`
where :math:`\vec{r}_i` are the bead positions and :math:`q_i` their charges. The
total charge dipole moment of the simulated volume :math:`V` is
:math:`\vec{P}=\sum_{mol\in V} \vec{p}_{mol}`.
If more than one molecular species are present, one can split :math:`\vec{P}` into the
different species' contributions.

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
in its last released version, version 2.7 (dating December 2018).

A variant of this module for use with a previous version of DL_MESO,
version 2.6 (dating November 2015), can be found in the ``old-v2.6``
directory [1]_.

Testing
_______

The present module ``gen_dipole.f90`` is compiled with the available Fortran90 compiler, e.g.:

``gfortran -o gen_dipole.exe gen_dipole.f90``

and the executable must be in the same directory of the HISTORY file to be
analyzed. The user will be asked to provide the Bjerrum length used in single
molecule simulations: all other information (including electric charges on all
bead types) required for analyses is provided in the HISTORY file.

To input the Bjerrum length, one can either enter it from the keyboard or write it
into a text file (say, input.txt) and run the program in this way:

``gen_dipoleaf.exe < input.txt``

We propose two tests to familiarize users with the utility and a third one on a
physically relevant system.

The first two tests involve two (toy) molecular species: a
branched one (four beads, T-shaped) and a simple dimer. All the beads carry charges.
In the first case 10 molecules of each type are present and are followed for a few time steps.
In the second case we suggest analyzing a single snapshot with just two
molecules and all the beads sitting at user-defined positions (via a CONFIG
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
using the following CONTROL file:

.. literalinclude:: ./CONTROL

and the FIELD file:

.. literalinclude:: ./FIELD


Analyzing the HISTORY file with `gen_dipole.exe`, this output is printed on the standard output

.. literalinclude:: ./out-d

The first line shows the histogram of cluster sizes: in this case,
it correctly gives 10 molecules of two beads, and 10 molecules of 4 beads.
Since internally the module checks that each molecule is a connected cluster [2]_,
this line should always give a histogram with the molecule sizes
(up to the detected maximum number of beads per molecule).

The resulting `dipole_BD` file is

.. literalinclude:: ./dipole_BD

and the `dipole_BRANCH` one is

.. literalinclude:: ./dipole_BRANCH

If instead the simulation is run on multiple nodes, only the results for the
first snapshot will be unchanged (i.e., the first line of each `dipole_*` file).
The other results will vary because different sequences of random numbers
will be used by DL_MESO_DPD for the time evolution of the system.

**Second test**

Run DL_MESO_DPD using the same CONTROL and FIELD files as above, with the
following changes:

- change `"steps 1000"` to `"steps 1"` (in CONTROL)
- change `"nummols 10"` to `"nummols 1"` (NB: appears twice in FIELD)

Also, use this CONFIG file that will initially align the molecule
branches with the Cartesian axes:

.. literalinclude:: ./CONFIG

where the identity of each bead is fixed by the FIELD file and is shown below:

::

    B(1) - A(2) - C(3)    B(5) - D(6)
            |
           A(4)

One can easily check that the dipole of each molecule is as expected (within
machine precision):

 :math:`\vec{p}_{BRANCH}=(0.04,0.32,0), \quad   \vec{p}_{BD}=(0,0,-0.2)~.`

The resulting `dipole_BD` file is

.. literalinclude:: ./dipole_BD-2

and the `dipole_BRANCH` one is

.. literalinclude:: ./dipole_BRANCH-2

The results of this test will not depend on the number of nodes used to run the
simulation [3]_.

**Third test: water in oil**

Here we suggest considering a fluid made up of harmonically bonded dimers
:math:`(+q,-q)`. Appropriately fixing the partial charges :math:`q`
and the Bjerrum length :math:`l_B`, this system
mimics water in an oil background as far as its dielectric properties
are concerned. For more details about this model, please see the page :ref:`dimers`.

Run DL_MESO_DPD using the following CONTROL file:

.. literalinclude:: ./CONTROL-3

and the FIELD file:

.. literalinclude:: ./FIELD-3

Analyzing the HISTORY file with `gen_dipole.exe`, this output is printed to
the standard output:

.. literalinclude:: ./out-3

In particular, we see that:

- :math:`\vec{P}=(0.0 \pm 0.1, 0.1 \pm 0.1, 0.1 \pm 0.1)`
- :math:`\epsilon_r= 42 \pm 2`
- :math:`g_k = 1.09 \pm 0.04`

Please note that the error estimates are calculated assuming all the samples are
independent. From the results obtained in the testing case of the
module `gen_dipoleaf.f90`, one sees that the auto-correlation time of
:math:`\vec{P}` in this system is about 1-2 DPD time units, so the sampling choice
used here (trajectories are written every 100 time steps, i.e., at each DPD
time unit) seems reasonable, even if a little bit optimistic.
To confirm the reliability of the error estimate, one can carry out
another run with a different random number sequence (using the CONTROL file
directive `seed`) and see if the two results are compatible within error bars.


Source Code
___________
.. literalinclude:: ./gen_dipole.f90
   :language: fortran
   :linenos:

.. Here are the URL references used
.. _DL_MESO: http://www.ccp5.ac.uk/DL_MESO
.. _ReST: http://docutils.sourceforge.net/docs/user/rst/quickref.html
.. [1] A small change to specifying charge smearing schemes and lengths in CONTROL
       files has been made since version 2.6: the ``old-v2.6`` folder includes
       CONTROL files for the tests shown here that will work with this version
       of DL\_MESO.
.. [2] Disambiguation on the concept of molecule. In DL\_MESO a *defined molecule*
         is a set of beads, which can be bonded or not.
         For the purpose of this module it is *required* that each molecule is a
         connected cluster (via stretching bonds).
         In fact, this - together with the reasonable assumption that each stretching
         bond cannot be stretched to more than half the system linear size - allows
         us to univocally define the charge dipole moment of each molecule.
.. [3] The tiny value for :math:`P_z` in `dipole_BRANCH` may vary, but for this test
         it should be no greater than the smallest available non-negligible
         floating-point number.
