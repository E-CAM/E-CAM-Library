.. _Pergauss:

###########################################################
PerGauss: Periodic Boundary Conditions for gaussian bases
###########################################################

.. sidebar:: Software Technical Information

  Language
    Fortran 90

  Licence
    None

  Documentation Tool
    Documentation provided as in-line comments within the source code

  Application Documentation
    Quantics documentation can be found in quantics_
    
  Relevant Training Material
    Tutorial and exercises to test the code are available here_


.. contents:: :local:


Purpose of Module
_________________

The module PerGauss (Periodic Gaussians) consists on an implementation of periodic boundary conditions for gaussian bases for the  Quantics_  program package. 
In quantum dynamics, the choice of coordinates is crucial to obtain meaningful results. While xyz or normal mode coordinates are linear and do not need a periodical treatment, particular angles, such as dihedrals, must be included to describe accurately the (photo-)chemistry of the system under consideration. In these cases, periodicity can be taken into account, since the value of the wave function and hamiltonian repeats itself after certain intervals. 
This feature is already implemented for grid basis functions such as exponential-DVR and FFT to use in the framework of the MCTDH method, within the quantics package. Using as wave function ansatz a linear combination of gaussians, following the original idea of Heller, has enormous advantages: First, a gaussian that follows a classical trajectory is the exact solution of the quantum harmonic oscillator and harmonic oscillators are generally the first step into approximating potential energy surfaces. This also allows a smooth transition to dynamics methods based on classical trajectories such as Ab-Initio Multiple Spawning (AIMS) and Surface Hopping. Second, one can easily take advantage of the locality of gaussians and move towards on-the-fly methods, where the potential is calculated as the basis functions span the conformational space.
In the case of methods that use gaussian basis functions, such as G-MCTDH [1]_ , vMCG [2]_ and its on-the-fly version DD-vMCG within the quantics set of programs, no implementation of periodic boundary conditions has been made until this contribution. 

The module is expected to provide the quantum dynamics community with a more efficient way of treating large systems whose excited state driving forces involve periodic coordinates. When used on precomputed potentials (in G-MCTDH and vMCG), the model can improve the convergence since smaller grid sizes are needed. Used on-the-fly, it reduces considerably the amount of electronic structure computations needed compared to cartesian coordinates, since conformations that seemed far in the spanned space may be closer after applying a periodic transformation.  


Background Information
______________________

Currently pergauss resides within the Quantics_ software package available upon request through gitlab_.


Testing
_______

A test example (``pergauss.inp``) is provided to test the module and can be found in the directory 
``$quantics_path/inputs``, where ``quantics_path`` is where Quantics_ is located.
The test can be done through the following command

::

        $ quantics -mnd pergauss.inp  

A more detailed test documentation for Quantics_ code developers can be found `in this link 
<http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/quantics/elk.html>`_
 

Source Code
___________

The source code for pergauss can be found within the Quantics_ software which 
can be downloaded via gitlab_. 
The Quantics_ project has a private repository so you also need to be a member of the project to checkout. 
Then type into terminal

::

        $ git clone https://gitlab.com/quantics/quantics.git DIRECTORY  

Within the Quantics_ program, the explicit code is located at the source code folder in files ``mctdhlib/gwplib.f90``, ``geninwf/eininwfmod.f90``, ``geninwf/genphi1.f90``, ``gendvr/einpbasmod.f90`` and ``include/global.f90``. Every modified line will be preceded by a comment saying !pergauss to help users finding the modifications.

.. _Quantics: https://www2.chem.ucl.ac.uk/worthgrp/quantics/doc/index.html
.. _gitlab: https://gitlab.com/quantics
.. _here: https://www2.chem.ucl.ac.uk/worthgrp/quantics/ 

References
__________

.. [1] I. Burghardt, I, H.-D. Meyer, and L. S. Cederbaum 
       *J. Chem. Phys.* **115** (1999) 2927


.. [2] G. W. Richings, I. Polyak, K. E. Spinlove, G. A. Worth, I. Burghardt, 
       B. Lasorne *Int. Rev. Phys. Chem.* **34** (2015) 269
        
