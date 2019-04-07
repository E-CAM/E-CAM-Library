.. _Particle_Insertion_hydration:

#########################
Particle Insertion Hydration
#########################

.. sidebar:: Software Technical Information

  This is the core module for the particle insertion suite of codes

  Languages
    C, Python 2.7, LAMMPS Scripting language

  Licence
    MIT -however, note that LAMMPS is GPL so when used together GPL applies

  Documentation Tool
    All source code should be documented so please indicate what tool has been used for documentation. We can help you
    with Doxygen and ReST but if you use other tools it might be harder for us to help if there are problems.

  Application Documentation
.. contents:: :local:
  Relevant Training Material
    Add a link to any relevant training material.

.. contents:: :local:


.. Add technical info as a sidebar and allow text below to wrap around it

Purpose of the Module
_________________
This software module computes the change in free energy associated with the insertion or deletion of water in dilute or dense conditions in a variety of Thermodynamic Ensembles, where statistical sampling through molecular dynamics is performed under `LAMMPS<https://lammps.sandia.gov/>`_ but will be extended to other molecular dynamics engines at a later date. It builds on the PI Core module of codes by adding electrostatic, bond, and angle
:math:`\lambda`   dependent interactions including SHAKE to the Lennard-Jones interactions that were dealt with in PIcore. It differs from the main community approach used to date to compute such changes as it does not use soft-core potentials. Its key advantages over soft-core potentials are: (a) electrostatic interactions 
can in principle be performed simultaneously
with particle insertion (this and other functionalities will be added in a new module); and, (b) essentially exact long-range dispersive interactions 
using `dispersion Particle Mesh Ewald <https://doi.org/10.1063/1.4764089>`_ (PMME)  or EWALD if desired  can  be selected at runtime  by  the user. 



Background Information
_____________________
Particle insertion can be used to compute the free energy associated with hydration/drying, the insertion of cavities in fluids/crystals, 
changes in salt levels, changes in solvent mixtures, and alchemical changes such as the mutation of amino-acids.   in crystals. It can also be used to compute the free energy of solvent mixtures and the addition of salts, which is used in the purification processing industrially, for instance in the purification of pharmaceutical active ingredients. Particle insertion can in principle also be used to compute the free energy associated with changes in the pH, that is the proton transfer from a titratable site to the bulk, 
for example in water. 

Our approach consists  of rescaling electrostatic charges of inserted atoms so that they converge to zero faster than inerted Van der Waals 
atoms where  the later uses the geometric mean for Lennard Jones diameters and binding energies, and that bond, angle, and dihedral spring constants  and where 
necessary  also bond lengths  scale to zero in the same fashion 

the effective size of inserted atoms through a parameter  :math:`\lambda` so that all interactions between inserted atoms and interactions between inserted atoms and atoms already present in the system are zero when  :math:`\lambda = 0`,  creating at most an integrable singularity which we can safely handle.  In the context of Lennard-Jones type pair potentials,  
our approach at a mathematical level is similar to Simonson, who investigated the mathematical conditions required to `avoid the
singularity of insertion <https://doi.org/10.1080/00268979300102371>`_. It turns out that a non-linear dependence of the interaction on  :math: '\\lambda'  between inserted
atoms and those already present is required (i.e. a simple linear dependence on :math: '\\lambda' necessarily introduces a singularity).



The applications of this module use in upcoming modules include computing the free energy changes associated with:
::
    (a) hydration and drying;
    (b) the addition of multiple molecules into a condenses environment;
    (c) residue mutation and alchemy;
    (d) constant pH simulations, this also will also exploit modules created in E-CAM work package 3
        (quantum dynamics); and,
    (e) free energy changes in chemical potentials associated with changes in solvent mixtures.
    

    
    
General Formulation
____________________

Consider a  system consisting of :math:`N+M` degrees of freedom  and the Hamiltonian

.. math::
  H(r,p,\lambda) =&H_0 + KE_{insert} +  \Delta V(r, \lambda)
where :math:`H_0` corresponds to an unperturbed Hamiltonian, and the perturbation :math:`\Delta V(r, \lambda)` depends nonlinearly on a control parameter :math:`\lambda`. The first set of N degrees of freedom is denoted by A and the second set of  M degrees of freedom is denoted by B.  To explore equilibrium properties of the system, thermostats, and barostats are used to sample either the NVT (canonical) ensemble or the NPT (Gibbs) ensemble. The perturbation is devised so that 
when  :math:`\lambda = 0`, :math:`\Delta V(r, \lambda) = 0`, B is in purely virtual. When :math:`\lambda = 1`, B 
corresponds to a  fully physical augmentation of the original system.


In the present software module, we include in the perturbation  interaction Lennard Jones potenetials, harmonic bond and angle interactions, and 
electostatic interactions:

.. math::
  \Delta V(r,\lambda) = V_{lj}(r,\lambda) + V_{b}(r,\lambda) + V_{a}(r,\lambda) + V_{el}(r,\lambda).

where for each inserted atom i

.. math::
  \hat{\sigma}( \lambda)_i &= \lambda \sigma_i   \\

  \hat{\epsilon}( \lambda)_i &= \lambda \epsilon_i   \\
  
  \hat{q}( \lambda)_i &= \lambda ^p \\
  
and the mixing rule for Van der Waals diameters and binding energy between different atoms uses the geometric mean for atoms pairs where one or more of the atoms is inserted but retains the mixing rule for atoms already present. The dependence of 
math:`\sigma` on :math:`\lambda` has the  consequence that the mean 
:math:`\sigma` between a pair of inserted atoms scales as :math:`\lambda`, but scales as :math:`\sqrt{\lambda}` when one atom in the pair is  
inserted and the other is already present. The dependence of math:`\epsilon` on  :math:`\lambda` ensures that forces behave regularly when 
:math:`\lambda` is very small. These choices of perturbations guarantees that the particle insertion and deletion catastrophes are avoided.
Regarding electrostatic interactions, the exponent p   allows the rate of convergence electrostatic interactions to zero to be faster than the rate at which that the effective diameters between corresponding Lennard Jones atoms go to zero, so as to ensure divergences are avoided. Currently p = 1.5.  The spring constants for harmonic, angular and torsional interactions involving inserted atoms are currently simply multiplied by :math:`\lambda`.It is also possible to replace
bond, angle and torsional interactions involving only inserted atoms with shake constraints. In such cases, the shake constraints are continuously on. For cases where arithmetic sum rules apply to the original system, an additional lambda bases perturbation stage can be applied to transform geometric mean based mixing rules for Lennard Jones interactions to arithmetic mean rules governing interactions between inserted atoms or inserted atoms and original atoms.



Algorithms
______________________________________
At the core of the PI core module there are four functions/codes.  The first written in python generates the interpolation points  which are
the zero's of suitably transformed Chebyshev functions. 

The second code written ln LAMMPS scripting language performs the simulation in user-defined ensembles at the selected
interpolation values of :math:'lambda', at a user-specified frequency, computing two-point central difference estimates of derivatives of the 
potential energy needed for thermodynamic integration,  computing the energy
functions for all values of :math:'lambda' in the context of MBAR.  The user also specifies the locations of the inserted particles. 
The user also specifies whether 
Particle Mesh Ewald or EWALD  should be used for dispersive interactions. 

The third code written in python takes the output data from LAMMPPS, prepares it so that free energy differences in the selected ensemble can be computed using MBAR provided by the pymbar suite of python codes of the Chodera group. 

The fourth code, also written in python take the LAMMPS output and performs the thermodynamic integration.


Source Code
___________

The source codes comprise the following 5 files
::


   (1) A python code `chebychev-lambda-input.py <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIhydration/chebychev-lambda-input.py>`_ that generates the lambda values to be input into LAMMPS according to the users' choices of  number of  interpolation points and the 
   minimum value of lambda to be used as the domain of integration 
   (2) A LAMMPS script codes `noinsert_trimer_elec6.in <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIhydration/harmonic/templatev5_nokspace-noinsert_trimer_elec6.in>`_ that generates the data required for estimating the changes in free energy due to the insertion TIP3P water molecules using a  standard cut-off respectively.
   (3) One example of coordinate input files for LAMMPS:  `example_lj403v2.lammps <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIhydration/harmonic/example_lj403v2.lammps>`_
   
   and  `example_lj3200b.lammps <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIhydration/example_lj3200b.lammps>`_
   (4) A python script `data-new8-thdy.py <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIhydration/data-new8-thdy.py>`_ that takes as input the thermodynamic integration output data from LAMMPS and uses it to compute the corresponding free energy change using thermodynamic integration.
   (5) A python script `data-new8-mbar.py <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIhydration/data-new8-mbar.py>`_ that takes as input the MBAR  output data from LAMMPS and uses it to compute the corresponding free energy change using MBAR
   
Compilation and Linking
_______________________

::
    (1) The initialization python code - this just uses numpy, so should work without additional libraries. We assume python 2.7 - but this can be easily adjusted 
        (only the print and possibly input commands may need to be adjusted for more recent version.
    (2) The LAMMPS script will run on any standard LAMMPS distribution from  2016 to 2018.
    (3) The example LAMMPS input functions with  any standard LAMMPS distribution from at least 2016 and up
    (4) The python code that takes as input the thermodynamic integration uses similar libraries to pymbar of the Chodera lab - but are pretty much standard.
    (5) The MBAR python code uses pymbar from the `Chodera lab <https://github.com/choderalab/pymbar>`_
    

    
Scaling  and Performance
_________________________

As the module uses LAMMPS, the performance and scaling of this module should essentially be the same, provided data for thermodynamic integration and 
MBAR is not generated too often. In the case of thermodynamic integration, this is due to the central difference approximation of derivatives, and in the case
of MBAR, it is due to the fact that many virtual moves are made which can be extremely costly if the number of interpolating points is large. Also, when using
PMME, the initial setup cost is computationally expensive, and should, therefore, be done as infrequently as possible. A future module in preparation will
circumvent the use of central difference approximations of derivatives.

Documentation
_____________
The  LAMMPS based script noinsert_trimer_elec6.in with  lammps  generates the data required for estimating the changes in free energy due to the insertion of deletion of particles using standard cut-off respectively. The codes are essentially identical apart from their different treatment of long-range dispersion. They each have 34 public variables which the user can set to suit the specific problem they have. All but the last two public variables (LAMBDAS and LLAMBDAS) can be changed at a LAMMPS command level at startup using the commands -var variable_name1 value1 -var variable_name2 value2 ...etc. 
The latter two can only be changed by direct editing of the input scripts. All public variables have names including a mixed upper and lower case letters. All non-public or internal variables names have letters written lower case format.  The codes have several internal loops. The code has a large number of explanatory comments within the script.

:Public Variables:

   1. variable input_COORDINATES_file index example_lj403v2.lammps # input coordinate data filename
   2. variable input_RESTART_filename index lj3200-rcut13-equil.restart
   3. variable LJ_sigma_final3 index 1 # Final sigma of inserted particle type . If there is more than one type, add more rows here.
   4. variable LJ_sigma_final4 index 0.00001
   5. variable LJ_sigma_final5 index 0.00001
   6. variable LJ_epsilon_final3 index 1 # Final binding energy of inserted oxygen
   7. variable LJ_epsilon_final4 index 0 # Final binding energy of inserted hydrogen 
   8. variable LJ_epsilon_final5 index 0 # Final binding energy of inserted hydrogen 
   9. variable CHARGE_final1 index 0
   10. variable CHARGE_final2 index 0
   11. variable CHARGE_final3 index -39.746355843   # TIP3P change on oxygen
   12. variable CHARGE_final4 index +19.873177922   # TIP3P change on hydrogen 
   13. variable CHARGE_final5 index +19.873177922   # TIP3P change on hydrogen 
   14. variable CHARGE_exponent index 1.5 # this allows  tweaking of rate at which electrostic interactions converge zero in lambda zero limit. In the method section above the CHARGE_exponent is given by the variable p
   15. variable BOND_length_final3 index 0.7876423    # harmonic bondlength TIP3P
   16. variable BOND_k_final3 index 45080    # harmonic spring constant k  final value (remember in LAMMPS the potential is k(r-r_0 -L)^2 )
   17. variable ANGLE_length_final3 index 104.52    # harmonic angle 3 final value
   18. variable ANGLE_k_final3 index 275r_0 -L)^2 )
   19. variable ANGLE_length_final3 index 104.52    # harmonic angle 3 final value
   20. variable ANGLE_k_final3 index 275variable LJ_sigma_final3 index 1 # Final sigma of inserted particle type . If there is   more than one type, add more rows here.
   21. variable system_TEMPERATURE index 2
   22. variable system_PRESSURE index 4 # Note if pressure is not isotropic add additional  rows with ensemble variables and info here
   23. variable LJ_system_RCUT index 3.5 # value of rcut for dispersion (pme  or  lj/cut1. )
   24. variable displacment_CENTRAL_difference index 0.00002  # optimal value for central difference estimate of derivatives in lammps runs
   25. variable disp_KSPACE_paramater index 0.65 # kspace PME paramater
   26. variable THERMODYNAMIC_output_frequency index 1000
   27. variable RUNTIME index 100000 # production run time
   28. variable SAMPLE_frequency index 1000 # measured as number of steps
   29. variable RELAXATION_time index 50000
   30. variable TIME_step index 0.005
   31. variable MBAR_switch index 10
   32. variable NUMBER_of_lambas index 10  # excluding zero lambda. 
   33. variable LAMBDAS index 0.0  0.106836511145 0.160288568297 0.26074557564     0.396090935503 0.55 0.703909064497 0.839254424359 0.939711431703 0.993163488855   
   34. variable LLAMBDAS_mbar index 0.0  0.106836511145 0.160288568297 0.260745575641 0.396090935503 0.55 0.703909064497 0.839254424359 0.939711431703 0.993163488855 

Running the lammps scripts
~~~~~~~~~~~~~~~~~~~~~~~~~

: Examples:


   1. mpiexec -np 4 lmp_mpi  -var RUNTIME 10000  -var RELAXATION_time 5000 -in templatev5_nokspace-noinsert_trimer_elec6.in



The examples 1. and 2.  use the initial coordinates consisting of 3200 atoms defined by the   -var input_COORDINATES_file example_3200b.lammps option, whereas the examples 3. and 4. use the default coordinates of 400 atoms.  The  RUNTIME and RELAXATION_time are very short for testing purposes. For production runs, they should be at least ten times longer. 

 `data-new8-thdy.py <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIhydration/data-new8-thdy.py>`_
~~~~~~~~~~~~~~~~~~~~~~~~~

This python code takes as input the thermodynamic integration output data from LAMMPS and uses it to compute the corresponding free energy change using thermodynamic integration. The user should call it from the directory where the output data from LAMMPS is held. It expects output data fo have the format header-name.tdy.number.dat where number equals the number of lambda values excluding zero. Here it is assumed that one particle is inserted. It will print the estimates of the free energy of insertion or deletion and also creates a director called TDY
and subdirectories where the results of the analysis are stored.


`data-new8-mbar.py <https://gitlab.e-cam2020.eu/mackernan/particle_insertion/blob/master/PIhydration/data-new8-mbar.py>`_ 
~~~~~~~~~~~~~~~~~~~~~~~~
This python code takes as input the multiple Bennet Acceptance Ratio (MBAR)  output data from LAMMPS and uses it to compute the corresponding free energy change using the pymbar code from the Chodera lab. The user should call it from the directory where the output data is held. It expects output data fo have the format header-name.mbar.number.dat where number equals the number of lambda values including zero. Here it is assumed that one particle is inserted. It will print the estimates of the free energy of insertion or deletion and also creates a director called MBAR and subdirectories where the results of the analysis are stored. 




.. Here are the URL references used

.. _ReST: http://docutils.sourceforge.net/docs/user/rst/quickref.html

