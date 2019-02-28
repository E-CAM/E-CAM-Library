.. _dlmeso_tetrahedral:

###########################################################
WIP: Analysis of local tetrahedral ordering for DL_MESO_DPD
###########################################################

.. sidebar:: Software Technical Information

  Language
    FORTRAN 90

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
It processes the trajectory (HISTORY) files and analyzes the local tetrahedral
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



Background Information
______________________

The base code for this module is DL_MESO_DPD, the Dissipative Particle
Dynamics code from the mesoscopic simulation package DL_MESO_,
developed by M. Seaton at Daresbury Laboratory.
This open source code is available from STFC under both academic (free) and
commercial (paid) licenses. The module is to be used with DL_MESO_
in its second to last released version, version 2.6 (dating November 2015).
A variant of this module to be used with its last released version,
version 2.7 (released December 2018), will be provided soon.

Testing
_______

The utility ``tetrahedral.f90`` is compiled with the available
Fortran90 compiler [2]_, e.g.:

``gfortran -o tetrahedral.exe tetrahedral.f90``
   
and the executable must be in the same directory of the HISTORY file. 

Source Code
___________

.. literalinclude:: ./tetrahedral.f90
      :language: fortran
      :linenos:

	 
.. Here are the URL references used
.. _DL_MESO: http://www.ccp5.ac.uk/DL_MESO
.. [Duboue2015]  E. Duboué-Dijon, A. Laage, *Characterization of the                                                                                             
		 local structure in liquid water by various order parameters*,
		 J. Phys. Chem. B, **119**, 8406 (2015).	 
.. [1] The angle
       :math:`\theta_{ijk}=\cos^{-1}\left\{\frac{\vec{r}_{ij}\cdot\vec{r}_{kj}}{r_{ij}r_{kj}}\right\}`
       where :math:`\vec{r_{ij}} = \vec{r_i} -\vec{r_j}` and :math:`r=|\vec{r}|`. 
.. [2] Compilation has been tested with the GNU compiler GCC, version 8.2.1.
