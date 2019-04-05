.. _Particle_Insertion_core:

#######################
Particle Insertion Core
#######################

.. sidebar:: Software Technical Information

  This is the core module for the particle insertion suite of codes

  Languages
    C, Python 2.7, LAMMPS Scripting language

  Licence
    MIT -however note that LAMMPS is now changing from GPL to  LGPL so when used togetherwith LAMMPS  LGPL applies

  Documentation Tool
    All source code should be documented so please indicate what tool has been used for documentation. We can help you
    with Doxygen and ReST but if you use other tools it might be harder for us to help if there are problems.

  Application Documentation

  Relevant Training Material
    Add a link to any relevant training material.

.. contents:: :local:


.. Add technical info as a sidebar and allow text below to wrap around it

Purpose of the Module
_____________________

This software module computes the change in free energy associated with the insertion or deletion of Lennard Jones particles in dilute or dense
conditions in a variety of Thermodynamic Ensembles, where statistical sampling through molecular dynamics is performed under `LAMMPS <https://lammps.sandia.gov/>`_ but 
will be extended to other molecular dynamics engines at a later date. Lennard-Jones type interactions are the key source of 
difficulty associated with particle insertion or deletion, which is why this module is a core module, as other interactions including 
Coulombic and bond, angle and dihedral interactions will be added in a second module. It differs from the main community approach used to 
date to compute such changes as it does not use soft-core potentials. Its key advantages over soft-core potentials are: (a) electrostatic interactions 
can in principle be performed simultaneously
with particle insertion (this and other functionalities will be added in a new module); and, (b) essentially exact long-range dispersive interactions 
using `dispersion Particle Mesh Ewald <https://doi.org/10.1063/1.4764089>`_ (PMME)  or EWALD if desired  can  be selected at runtime  by  the user. 


Background Information
______________________

Particle insertion can be used to compute the free energy associated with hydration/drying, the insertion of cavities in fluids/crystals,
changes in salt levels, changes in solvent mixtures, and alchemical changes such as the mutation of amino-acids.   in crystals. It can also 
be used to compute the free energy of solvent mixtures and the addition of salts, which is used in the purification processing  
industrially, for instance in the purification of pharmaceutical active ingredients. Particle insertion can in principle also be  
used to compute the free energy associated with changes in the pH, that is the proton transfer from a titratable site to the bulk, 
for example in water. 

Our approach consists  of rescaling the effective size of inserted atoms through a parameter  :math:`\lambda` so that all interactions between 
nserted atoms and  interactions between inserted atoms and atoms already present in the system are zero when  :math:`\lambda = 0`,  creating at most an 
integrable singularity which we can safely handle.  In the context of Lennard-Jones type pair potentials,  
our approach at a mathematical level is similar to Simonson, who investigated the mathematical conditions required to `avoid the
singularity of insertion <https://doi.org/10.1080/00268979300102371>`_. It turns out that a non-linear dependence of the 
interaction on  :math: '\\lambda'  between inserted
atoms and those already present is required (i.e. a simple linear dependence on :math: '\lambda' necessarily introduces a singularity).



This module and  upcoming modules include computing the free energy changes associated with the following applications
   (a) hydration and drying;
   (b) the addition of multiple molecules into a condenses environment;
   (c) residue mutation and alchemy;
   (d) constant pH simulations, this also will also exploit modules created in E-CAM work package 3 (quantum dynamics); and,
   (e) free energy changes in chemical potentials associated with changes in solvent mixtures.
    

    
    
General Formulation
___________________

Consider a  system consisting of :math:`N+M` degrees of freedom  and the Hamiltonian

.. math::
  H(r,p,\lambda) =&H_0 + KE_{insert} +  \Delta V(r, \lambda)

where :math:`H_0` corresponds to an unperturbed Hamiltonian, and the perturbation :math:`\Delta V(r, \lambda)` depends 
nonlinearly on a control parameter :math:`\lambda`. The first set of N degrees of freedom is denoted by A and the second 
set of  M degrees of freedom is denoted by B.  To explore equilibrium properties of the system, thermostats, and barostats 
are used to sample either the NVT (canonical) ensemble or the NPT (Gibbs) ensemble. The perturbation is devised so that 
when  :math:`\lambda = 0`, :math:`\Delta V(r, \lambda) = 0`, B is in purely virtual. When :math:`\lambda = 1`, B 
corresponds to a  fully physical augmentation of the original system.


In the present software module, we consider only interaction Lennard Jones atoms. 

.. math::
  \Delta V(r,\lambda) = V_{lj}(r,\lambda)

where for each inserted atom i

.. math::
  \hat{\sigma}( \lambda)_i &= \lambda \sigma_i   \\

  \hat{\epsilon}( \lambda)_i &= \lambda \epsilon_i   \\

  

and the mixing rule for Van der Waals diameters and binding energy between different atoms uses the geometric mean. 
The dependence of :math:`\sigma` on :math:`\lambda` has the  consequence that the mean 
:math:`\sigma` between a pair of inserted atoms scales as :math:`\lambda`, but scales as :math:`\sqrt{\lambda}` when one atom in the pair is  
inserted and the other is already present. These choices of perturbations guarantees that the particle insertion and deletion catastrophes are avoided.

Algorithms
__________

At the core of the PI core module there are four functions/codes.  The first written in python generates the interpolation points  which are
the zero's of suitably transformed Chebyshev functions. 

The second code written ln LAMMPS scripting language performs the simulation in user-defined ensembles at the selected
interpolation values of :math:'lambda', at a user-specified frequency, computing two-point central difference estimates of derivatives of the 
potential energy needed for thermodynamic integration,  computing the energy
functions for all values of :math:'lambda' in the context of MBAR.  The user also specifies the locations of the inserted particles. 
The user also specifies whether 
Particle Mesh Ewald or EWALD  should be used for dispersive interactions. 

The third code written in python takes the output data from LAMMPS, prepares it so that free energy differences in the 
selected ensemble can be computed using MBAR provided by the pymbar suite of python codes of the Chodera group. 

The fourth code, also written in python take the LAMMPS output and performs the thermodynamic integration.
.. image:: ./flowchart1.png

Source Code
___________

The source codes comprise the following 8 files. They are in the `directory <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIcore/>`
https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIcore/chebychev-lambda-input.py

   (1) A python code `chebychev-lambda-input.py <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIcore/chebychev-lambda-input.py>`_ that generates the lambda values to be input into LAMMPS according to the users' choices of  number of  interpolation points and the 
   minimum value of lambda to be used as the domain of integration 
   (2,3) Two LAMMPS script codes `templatev5.in <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIcore/templatev5.in>`_
   and  `templatev5_nokspace.in  <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIcore/templatev5_nokspace.in>`_
   that generate the data required for estimating the changes in free energy due to the insertion or deletion of particles using Particle Mesh Ewald long range estimate of dispersion and standard cut-off respectively.
   (4,5) Two examples of coordinate input files for LAMMPS:  `example_lj400b.lammps <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIcore/example_lj400b.lammps>`_
   
   and  `example_lj3200b.lammps <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIcore/example_lj3200b.lammps>`_
   (6) A python script `data-new8-thdy.py <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIcore/data-new8-thdy.py>`_ that takes as input the thermodynamic integration output data from LAMMPS and uses it to compute the corresponding free energy change using thermodynamic integration.
   (7) A python script `data-new8-mbar.py <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIcore/data-new8-mbar.py>`_ that takes as input the MBAR  output data from LAMMPS and uses it to compute the corresponding free energy change using MBAR
   (8) A c code `ljeos1.c <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIcore/ljeos1.c>`_ that uses published results of `K. Johnson et al <https://doi.org/10.1080/00268979300100411>`_ to estimate the chemical potential for a Lennard Jones Fluid. This allows direct comparison of our predictions as a test with a wide variety of densities and temperatures of a Lennard-Jones fluid.



Compilation and Linking
_______________________

    (1) The initialization python code - this just used numpy so should work without additional libraries. We assume python 2.7 - but this can be easily adjusted 
        (only the print and possibly input commands may need to be adjusted for more recent version.
    (2) The LAMMPS script will run on any standard LAMMPS distribution from  2016 to 2018.
    (3) The example LAMMPS input functions with  any standard LAMMPS distribution from at least 2016 and up
    (4) The python code that takes as input the thermodynamic integration uses similar libraries to pymbar of the Chodera lab - but are pretty much standard.
    (5) The MBAR python code uses pymbar from the `Chodera lab <https://github.com/choderalab/pymbar>`_
    (6) The c code for comparing the output with Equation of State Results is vanilla C.
    

    
Scaling  and Performance
________________________

As the module uses LAMMPS, the performance and scaling of this module should essentially be the same, provided data for thermodynamic integration and 
MBAR are not generated too often, as is demonstated below. In the case of thermodynamic integration, this is due to the central difference approximation of derivatives, and in the case
of MBAR, it is due to the fact that many virtual moves are made which can be extremely costly if the number of interpolating points is large. Also, when using
PMME, the initial setup cost is computationally expensive, and should, therefore, be done as infrequently as possible. A future module in preparation will 
circumvent the use of central difference approximations of derivatives. The scaling performance of PI-CORE was tested on Jureca multi node. 
The results for weak scaling (where the number of core and the system size are doubled from 4 to 768 core) are as follows.
Weak Scaling:

==================  ===========  
Number of MPI Core  timesteps/s
==================  ===========
4                   1664.793 
8                   1534.013
16                  1458.936
24                  1454.075
48                  1350.257
96                  1301.325 
192                 1263.402
384                 1212.539 
768                 1108.306
==================  ===========

and for the strong scaling (where the number of core are doubled from 4 to 384 but the system size is fixed equal to 768 times the original system 
size considered for one core/processor for weak scaling) Strong Scaling:

==================  =============  
Number of MPI Core  timesteps/s 
==================  =============
4                   9.197
8                   17.447
16                  34.641
24                  53.345
48                  104.504
96                  204.434
192                 369.178
384                 634.022
==================  =============


Documentation
_____________

Particle Integration Core of codes consists of 6 codes in the directory (short URL) https://goo.gl/iyJxbT  


`chebychev-lambda-input.py <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIcore/chebychev-lambda-input.py>`_ 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This is a python code that generates the lambda values to be input into LAMMPS according to the users' choices of number of interpolation points and the minimum value of lambda to be used as the domain of integration. The set of lambda values  and 0  correspond to the LAMMPS script public values "LAMBDAS and LLAMBDAS" listed below, arranged in increasing order, and the the number of values equals the public variable "NUMBER_of_lambas"  


`templatev5.in <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIcore/templatev5.in>`_   and  `templatev5_nokspace.in  <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIcore/templatev5_nokspace.in>`_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These two LAMMPS based script codes generate the data required for estimating the changes in free energy due to the insertion of deletion of particles using Particle Mesh Ewald long-range estimate of dispersion and standard cut-off respectively. The codes are essentially identical apart from their different treatment of long-range dispersion. They each have 18 public variables which the user can set to suit the specific problem they have. All but the last two public variables (LAMBDAS and LLAMBDAS) can be changed at a LAMMPS command level at startup using the commands -var variable_name1 value1 -var variable_name2 value2 ...etc. 
The latter two can only be changed by direct editing of the input scripts. All public variables have names including a mixed upper and lower case letters. All non-public or internal variables names have letters written lower case format.  The codes have several internal loops. The  code has a large number of explanatory comments within the script.

:Public Variables:

   1. variable input_COORDINATES_file index lj3200nvt # input coordinate data filename
   2. variable input_RESTART_filename index lj3200-rcut13-equil.restart
   3. variable LJ_sigma_final3 index 1 # Final sigma of inserted particle type . If there is 
      more than one type, add more rows here.
   4. variable LJ_epsilon_final3 index 1 # Final binding energy of inserted particle type. 
      If there is more than one type, add more rows here.
   5. variable system_TEMPERATURE index 2
   6. variable system_PRESSURE index 4 # Note if pressure is not isotropic add additional 
      rows with ensemble variables and info here
   7. variable LJ_system_RCUT index 3.5 # value of rcut for dispersion (pme  or  lj/cut1. )
   8. variable displacment_CENTRAL_difference index 0.00002  # optimal value for central 
      difference estimate of derivatives in lammps runs
   9.  variable disp_KSPACE_parameter index 0.65 # kspace PME parameter
   10. variable THERMODYNAMIC_output_frequency index 1000
   11. variable RUNTIME index 100000 # production run time
   12. variable SAMPLE_frequency index 1000 # measured as number of steps
   13. variable RELAXATION_time index 50000
   14. variable TIME_step index 0.005
   15. variable MBAR_switch index 10
   16. variable NUMBER_of_lambas index 10  # excluding zero lambda. 
   17.  variable LAMBDAS index 0.0  0.106836511145 0.160288568297 0.26074557564     0.396090935503 0.55 0.703909064497 0.839254424359 0.939711431703 0.993163488855   
   18. variable LLAMBDAS_mbar index 0.0  0.106836511145 0.160288568297 0.260745575641 0.396090935503 0.55 0.703909064497 0.839254424359 0.939711431703 0.993163488855 

Running the lammps scripts
~~~~~~~~~~~~~~~~~~~~~~~~~~

Four examples of running the lammps scripts are as follows.

   1. mpirun -np 24 lmp_fionn.mpi -var input_COORDINATES_file example_3200b.lammps -var RUNTIME 10000  -var RELAXATION_time 5000 -in templatev5.in 
   2. mpirun -np 24 lmp_fionn.mpi -var input_COORDINATES_file example_3200b.lammps -var RUNTIME 10000  -var RELAXATION_time 5000 -in templatev5_nokspace.in 
   3. mpirun -np 24 lmp_fionn.mpi  -var RUNTIME 10000  -var RELAXATION_time 5000 -in templatev5.in
   4. mpirun -np 24 lmp_fionn.mpi  -var RUNTIME 10000  -var RELAXATION_time 5000 -in templatev5_nokspace.in

The examples 1. and 2.  use the initial coordinates consisting of 3200 atoms defined by the   -var input_COORDINATES_file example_3200b.lammps option, 
whereas   the examples 3. and 4. use the default coordinates of 400 atoms.  The  RUNTIME and RELAXATION_time are very short 
for testing purposes. For  production runs they should be at least ten times longer. 

`data-new8-thdy.py <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIcore/data-new8-thdy.py>`_ 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This python code takes as input the thermodynamic integration output data from LAMMPS and uses it to compute the corresponding free energy change using thermodynamic integration. The user should call it from the directory where the output data from LAMMPS is held. It expects output data fo have the format header-name.tdy.number.dat where number equals the number of lambda values excluding zero. Here it is assumed that one particle is inserted. It will print the estimates of the free energy of insertion or deletion and also creates a director called TDY
and subdirectories where the results of the analysis are stored.


`data-new8-mbar.py <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIcore/data-new8-mbar.py>`_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This python code takes as input the multiple Bennet Acceptance Ratio (MBAR)  output data from LAMMPS and uses it to compute the corresponding free energy change using the  pymbar code from the Chodera lab. The user should call it from the directory where the output data is held. It expects output data fo have the format header-name.mbar.number.dat where number equals the number of lambda values including zero. Here it is assumed that one particle is inserted. It will print the estimates of the free energy of insertion or deletion and also creates a director called MBAR and subdirectories where the results of the analysis are stored. 


`ljeos1.c <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIcore/ljeos1.c>`_ 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This simple C code uses published results of `K. Johnson et al <https://doi.org/10.1080/00268979300100411>`_ to estimate the chemical potential for a Lennard Jones Fluid. This allows direct comparison of our predictions as a test with a wide variety of densities and temperatures of a Lennard-Jones fluid. The user needs to input the target density and temperature. 


