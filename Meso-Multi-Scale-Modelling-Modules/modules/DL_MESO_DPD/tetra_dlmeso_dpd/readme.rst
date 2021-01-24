.. _dlmeso_tetrahedral:

######################################################
Analysis of local tetrahedral ordering for DL_MESO_DPD
######################################################

.. sidebar:: Software Technical Information

  Language
    Fortran 2003

  Licence
    BSD 

  Documentation Tool
    RST

  Application Documentation
    See the Source Code section
  
  Relevant Training Material
    See the Testing section

.. contents:: :local:	      


Purpose of Module
_________________

This module, ``tetrahedral.f90``, is a postprocessing utility for DL_MESO_DPD,
the Dissipative Particle Dynamics (DPD) code from the DL_MESO_ package.
It processes trajectory (HISTORY) files and analyzes the local tetrahedral
ordering, a feature that is relevant, for example, in water-like systems.

The local ordering in liquid water can be assessed considering the coordinates
of oxygen atoms [Duboue2015]_. In particular, for each oxygen, its four nearest neighbouring
oxygens are considered, whereas the hydrogens are disregarded.
At the mesoscale level, the user will select one (appropriate) bead species and analyze its local
ordering.

Given a particle :math:`j`, we first find its *four nearest neighbours*
(n.n.). Then, an *orientational tetrahedral order parameter* is built using
:math:`q=1-\frac{3}{8}\sum_{i=1}^3\sum_{k=i+1}^4\left(\cos\psi_{ik}+\frac{1}{3}\right)^2`,
where :math:`i,k` are n.n. of :math:`j` and :math:`\psi_{ik}=\theta_{ijk}` is the angle [1]_
between the particles :math:`i`, :math:`j` and :math:`k`.
Of course, the quantity is then averaged over the central particle :math:`j` and over time.

A *translational tetrahedral order parameter*, :math:`S_k`, is defined as
:math:`S_k=1-\frac{1}{3}\sum_{i=1}^4 \frac{(r_i - \bar{r})^2}{4\bar{r}^2}`,
where :math:`i` is a n.n. of :math:`j` and :math:`\bar{r}=\frac{1}{4}\sum_{i=1}^4 r_i`.

Concerning the limiting values of these parameters:
in a regular tetrahedron (if the four vertices are referred to the
center of the solid) one has :math:`q=S_k=1`.
In an ideal gas, where the angle :math:`\psi_{ik}` is randomly distributed,
:math:`q\simeq0`. On the other hand, :math:`S_k\simeq 0` if the density
fluctuations are large enough.

As a result of the analysis, a file `TETRADAT` is produced, whose columns are
:math:`\textrm{snapshot index}, q, S_k`, the instantaneous values of the order
parameters defined above. At the end of the file, the averages and
standard errors (computed assuming the snapshots are uncorrelated) of both order parameters are given.


Background Information
______________________

The base code for this module is DL_MESO_DPD, the Dissipative Particle
Dynamics code from the mesoscopic simulation package DL_MESO_,
developed by M. Seaton at Daresbury Laboratory.
This open source code is available from STFC under both academic (free) and
commercial (paid) licenses. The module is to be used with DL_MESO_
in its latest released version, version 2.7 (released December 2018).
A variant of this module to be used with a previous version of DL_MESO,
version 2.6 (dating November 2015), can be found in the ``old-v2.6``
directory.

Testing
_______

The utility ``tetrahedral.f90`` is compiled with the available
Fortran 2003 compiler [2]_, e.g.:

``gfortran -o tetrahedral.exe tetrahedral.f90``
   
and the executable must be in the same directory of the HISTORY file.
The user is asked to provide the number of the species for which ordering has to be
analyzed. To input the user-defined parameter, one can enter it interactively
at runtime or write it into a text file (say, ``input.txt``) and run the program
in this way:

``tetrahedral.exe < input.txt``

Below we propose a test where a fluid is prepared in a ordered configuration
(diamond cubic lattice)
and rapidly goes into an orientationally disordered one.

**Test**

The sources used for this test are available to :download:`download <./tetra_example.tgz>`. 

Run the DL_MESO_DPD simulation on a single node (serial run)
using the `CONTROL` file,

.. literalinclude:: ./CONTROL

the `FIELD` file

.. literalinclude:: ./FIELD

and the `CONFIG` file

.. literalinclude:: ./CONFIG

This configuration corresponds to a diamond cubic lattice [3]_, while the ``nfold``
directive in the `CONTROL` file replicates the configuration twice in all three dimensions.

Analyzing the resulting trajectory (``HISTORY``) file with ``tetrahedral.exe``
(compiled as indicated above) and inputing :math:`1` for the runtime argument,
the following output is printed to the standard output:

.. literalinclude:: ./out

The output file `TETRADAT`

.. literalinclude:: ./TETRADAT
   :lines: 1-13

contains the values of :math:`q` and :math:`S_k` for each snapshot and their
averages are also produced.

One can see that in the initial snapshot, both order parameters detect an
ordered state (i.e., :math:`S_k=q=1`).
With the evolution in time, since the system is a dilute fluid without bonds
between particles, the orientational ordering is rapidly lost (i.e., :math:`q\simeq 0`).
On the other hand, the translational order parameter stays close to one since the density of the
system is roughly uniform.


Source Code
___________

You can directly download the source file :download:`tetrahedral.f90 <./tetrahedral.f90>`
and we also include its contents below (as well as in the test tarball).

.. literalinclude:: ./tetrahedral.f90
      :language: fortran
      :linenos:


.. Here are the URL and references used
.. _DL_MESO: http://www.ccp5.ac.uk/DL_MESO
.. [Duboue2015]  E. Duboué-Dijon, A. Laage, *Characterization of the local structure in liquid water
      by various order parameters*, J. Phys. Chem. B, **119**, 8406 (2015).
.. [1] The angle
       :math:`\theta_{ijk}=\cos^{-1}\left\{\frac{\vec{r}_{ij}\cdot\vec{r}_{kj}}{r_{ij}r_{kj}}\right\}`
       where :math:`\vec{r_{ij}} = \vec{r_i} -\vec{r_j}` and :math:`r=|\vec{r}|`. 
.. [2] Compilation has been tested with the GNU compiler GCC, version 10.2.0.
.. [3] The diamond cubic crystal lattice (https://en.wikipedia.org/wiki/Diamond_cubic) is a repeating pattern of
       8 atoms. Their coordinates may be given as:
       :math:`A=(0,0,0)`, :math:`B=(0,2,2)`, :math:`C=(2,0,2)`, :math:`D=(2,2,0)`, :math:`E=(3,3,3)`,
       :math:`F=(3,1,1)`, :math:`G=(1,3,1)`, and :math:`H=(1,1,3)` in a unit cubic cell of side :math:`L=4`.
       One can check that, with the minimum image convention, each
       particle has its 4 closest neighbours at a distance :math:`\sqrt{3}`, and all the angles are
       :math:`\textrm{acos}(-1/3)`.
       For this configuration (also if repeated periodically along the three Cartesian axis),
       :math:`q=1` and :math:`S_k=1`.

