:orphan:

..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    GC-AdResS: Local thermostat adaption of the Abrupt AdResS scheme

  Language
    Implemented in GROMACS version 5.1.5  

  Licence
    MD Simulation:
    See GROMACS web page: `<http://www.gromacs.org/>`_
    
    Analysis tools and thermodynamic force calculation:
    see VOTCA web page: `<http://www.votca.org/home>`_

  Documentation Tool

  Application Documentation
    See GROMACS web page: `<http://www.gromacs.org/>`_
    See VOCTA web page: `<http://www.votca.org/Documentation>`_

  Relevant Training Material
    See GROMACS web page: `<http://www.gromacs.org/>`_
    See VOCTA web page: `<http://www.votca.org/tutorials>`_
    
..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _LocalT:

########################################################
Local thermostat adaption of the Abrupt GC-AdResS scheme 
########################################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

The original idea of our proposal: to work on a general implementations of AdResS in
class. MD packages. With Abrupt AdresS we implemented a new partitioning scheme to bypass the performance problems in the smooth coupling GC-AdResS implementation. We switched to the general interaction kernel and simplified the neighbor list search. This module takes this new ansatz and combines it with a local thermostat approach (see Ref. `<http://iopscience.iop.org/article/10.1088/1367-2630/17/8/083042>`_). 

The advantages of this module are: we can thermalize the transition and the coarse grained region, the atomistic region is thermalized indirectly. Furthermore, we still have a method which is decoupled from the core of any MD code. Theory, application and tests see `Link(J.Chem.Phys.): <https://aip.scitation.org/doi/10.1063/1.5031206>`_ or `Link(arXiv): <https://arxiv.org/abs/1806.09870>`_.


..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

.. This is an example of what a *module* for E-CAM looks like. The original source of this page (:download:`readme.rst`) contains lots of additional comments to help you create your module (and understand ReST_ syntax) so please use this as a starting point. You are free add any level of complexity you wish (within the bounds of what ReST_ can do). More general instructions for making your contribution can be found in ":ref:`contributing`".

.. Remember that for a module to be accepted into the E-CAM repository, your source code changes in the target application must pass a number of acceptance criteria:

.. * Style *(use meaningful variable names, no global variables,...)*

.. * Source code documentation *(each function should be documented with each argument explained)*

.. * Tests *(everything you add should have either unit or regression tests)*

.. * Performance *(If what you introduce has a significant computational load you should make some performance optimisation effort using an appropriate tool. You should be able to verify that your changes have not introduced unexpected performance penalties, are threadsafe if needed,...)*

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment


.. Give a brief overview of why the module is/was being created, explaining a little of the scientific background and how it fits into the larger picture of what you want to achieve.

.. If needed you can include latex mathematics like 
.. :math:`\frac{ \sum_{t=0}^{N}f(t,k) }{N}`
.. which won't show up on GitLab/GitHub but will in final online documentation.

.. If you want to add a citation, such as [CIT2009]_. Note that citations may get rearranged, e.g., to the bottom of the "page".

.. : .. [CIT2009] A citation (as often used in journals).

The original idea of our proposal: to work on a general implementation of AdResS in
class. MD packages. If one looks at the AdResS simulations it is possible to describe it in a nutshell as partitioning the simulation box into different regions. The Abrupt coupling scheme has one atomistic and one coarse grained region, coupled via a transition region, where an additional force is acting on the molecules. In previous work (Ref. `<http://iopscience.iop.org/article/10.1088/1367-2630/17/8/083042>`_) the idea of a local thermostat was introduced. This module describes how to couple that ansatz with our new Abrupt AdResS. The first test, as well as an overview over the theory, can be found here  `<https://aip.scitation.org/doi/10.1063/1.5031206>`_ or `<https://arxiv.org/abs/1806.09870>`_.


.. The interface between the regions is more fluctuating and needs a more responsive thermodynamic force but it works reasonably well. 

.. The second piece of the puzzle is the spatial partitioning as we showed at the ESDW8 in Berlin and as Guzman et al. (arXiv:1711.03290v1) published recently it is possible to use a spatial partitioning for GC-AdResS. 


Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module combines two different codes. The first part is the Abrupt coupling scheme already presented in a module. In any AdResS simulations the simulation box is partitioned into different regions. Previous AdResS implementations have three regions (atomistic, hybrid and coarse grained). They are coupled in the hybrid region by slowly switching between atomistic forces and coarse grained forces. That is done by introducing the weighting function required for the smooth coupling into the force kernel, which slows down the performance of the code. The Abrupt coupling scheme has one atomistic and one coarse grained region, coupled via a transition region, which is only due to the fact that an additional force is acting on the molecules. Since the weighting function is not needed any more, the forces are calculated via the standard GROMACS kernels, which increases the performance. 

In previous work by Agarwal et al. (Ref. `<http://iopscience.iop.org/article/10.1088/1367-2630/17/8/083042>`_) the idea of a local thermostat was introduced. Since in AdResS the simulation box is partitioned into different regions the next logical step is to adapt the thermalization of the box and apply the thermostat only in the hybrid and coarse grained region, since the hybrid region is an artificial region and the coarse grained region represents a reservoir.

This module describes how to couple these two approaches. The first test, as well as an overview over the theory, can be found here  `Link(J.Chem.Phys.): <https://aip.scitation.org/doi/10.1063/1.5031206>`_ or `Link(arXiv): <https://arxiv.org/abs/1806.09870>`_.


Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

We tested this new implementation on SPC water and 1,3-dimethyl imidazolium chloride. The results are published see Ref `<https://aip.scitation.org/doi/10.1063/1.5031206>`_ or `<https://arxiv.org/abs/1806.09870>`_.

Here is a short manual on how to run the test and set up the local thermostat simulation within this Abrupt AdResS simulations in GROMACS:

1) mandatory requirement: a working full atomistic simulation (molecular configuration, force fields and optimal MD input parameter)

2) You need a very well converged NVT run, which can be used as starting point for the coarse grained (CG) and then later the AdResS simulations.

3) You have to generate a coarse grained (CG) potential. We use, for convenience, the inverse Boltzmann iteration provided in the VOTCA package (`<http://www.votca.org/home>`_). The resulting tabulated CG potentials are used in the AdResS simulation. Alternatively you can use WCA potentials or standard Lennard-Jones potentials. The main requirement for the AdResS simulation is that the density in the CG region is the same as in the atomistic (AT) region.

  NOTE: The method can be used with any potential, which preserves the correct density. If only a SPC/E CG potential is available it can be used for SPC/e water models as well as for a more advanced water model. It is possible to use a WCA potential, which is basically a Lennard-Jones potential. *And* it is possible to switch the CG potential completely off. That will transform the CG region to a true thermodynamic reservoir with a non-interacting gas.

4) The next step is to create a double resolution configuration and adjust the dependencies (force field, topology, index file, GROMACS input file). Creating the configuration is straight forward (we use VOTCA `VOTCA <http://www.votca.org/home>`_):

::

  Example from VOTCA: 
  csg_map --top topol.tpr --cg cg_mapping_scheme --hybrid --trj conf.gro --out conf_hybrid.gro

Of course, if you want to use this configuration in a MD simulation you have to adjust the force field (see example file: *spc.adress.itp*). You have to define a virtual site:  

::

  [ virtual_sites3 ]
  ; Site from funct a d
  ; atom dependencies func     a            b
     4      1 2 3     1    0.05595E+00 0.05595E+00

The next step is to adjust the status of the CG particle in the topology file (in our example: *topol.top*) from *A* for *atom* to *V* as *virtual particle*. And of course insert the new force field.

::

  #include "spc.adress.itp"  

Then you have to generate an index file with the different energy groups. In this example, we have 2 groups (EXW and WCG, the name of the CG particle): 

::

  gmx make_ndx -f conf_hybrid.gro
  > a WCG
  > !3
  > name 4 EXW
  > q


The next step is to adjust the GROMACS input file. AdResS needs the Langevin dynamics, so you have to choose: 
::

  integrator = sd 


Since the system is double resolution, meaning we have the atomistic details and the virtual particles, we have to define the energy groups:

::

  ; Selection of energy groups 
  energygrps = EXW WCG 
  energygrp_table = WCG WCG


Note: the table for the WCG and WCG particles can be a pre-defined coarse grained potential but can be also set to zero. 

Alternatively, the non-bonded interactions for WCG in the force field can be set to zero.a Then the input would be like:

::

  ; Selection of energy groups 
  energygrps = EXW WCG 


GROMACS  version 5.1.5 is using verlet as standard cutoff-scheme, so we have to change that to *group*:

::

  ; nblist update frequency 
  cutoff-scheme = group 


In case of local thermostat simulations (see `Link (for J.Chem.Phys.): <https://aip.scitation.org/doi/10.1063/1.5031206>`_ or `Link (for arXiv): <https://arxiv.org/abs/1806.09870>`_) we use:

:: 

  coulombtype = reaction-field-zero 
  rcoulomb = 1.0 
  vdw-type = user  
  rvdw = 1.0 

If you use the stochastic dynamics, we add the following entries to make sure we have only NVT and a thermalization via the Langevin dynamics.  

::

  ; Temperature coupling 
  Tcoupl = no 
  Pcoupl = no

To switch the simulation to AdResS this is the key part. This starts the AdResS runs.

::

  ; AdResS parameters 
  adress = yes ;no 

Here you define the geometry of the atomistic region, either *sphere* (a spherical region anywhere in the simulation box) or *xsplit* (a cuboid slice of the whole simulation box for the atomistic region, with the transition and coarse grained region on each side). 

::

  adress_type = sphere ;xsplit sphere or constant 

This defines the width of the atomistic region, starting from the given reference coordinate (keyword *adress_reference_coords*, by simply using: *tail conf_hybrid.gro | awk '(NF==3){print $1/2., $2/2., $3/2.}'*). In the older versions of AdResS, with a smooth coupling between AT and CG the width of the hybrid region width (*adress_hy_width*) was also defined. In the Abrupt_AdResS setup it is not necessary any more, even if you put a number that region is counted (in the code) as AT. 

::

  adress_ex_width = 1.5 
  adress_hy_width = 1.5 
  adress_ex_forcecap = 2000  
  adress_interface_correction =  thermoforce ;off
  adress_site = com 
  adress_reference_coords = 3.7500 1.860355 1.860355
  adress_tf_grp_names = WCG
  adress_cg_grp_names = WCG 
  adress_do_hybridpairs = no

Another important aspect is the force capping. Abrupt AdResS works fine for small molecules like water, but for larger or more complex molecules the force capping is very important. We cap every force component (i.e. f(x),f(y),f(z)) acting on a particle and not the norm of the force, which reduces the computational time spend. This is described in another module. 

*adress_interface_correction* defines if you use an external force to correct the density or not. In case of the old AdResS (smooth coupling) that correction simply refined the simulation, as the density difference was not significant. For the Abrupt AdResS, and the method development based on it, and more complex molecules (i.e. polymers) the thermodynamic force is essential. If it is not taken into account the risk to form interfaces between AT and CG is high. Also if particles coming too close (basically overlap) the run can crash. The role of the thermodynamic force, the force cap and the basic theory behind it see `Link (for J.Chem.Phys.): <https://aip.scitation.org/doi/10.1063/1.5031206>`_ or `Link (for arXiv): <https://arxiv.org/abs/1806.09870>`_. For this to work you must have a file e.g. in our example case: *tabletf_WCG.xvg* in the directory, otherwise you have to set:

::

  adress_interface_correction =  off

The local thermostat simulations are significantly different from the Abrupt coupling AdResS simulations. The atomistic region is indirectly thermalized by the hybrid/coarse grained, which leads to an NVE-like environment. To make sure the simulations run smoothly, a tabulated potential for shifted Lennard-Jones potentials is needed. Furthermore, GROMACS has to be compiled with double resolutions. It is easy to see when the simulation didn't work, as the atomistic region is evacuated by all molecules and the resulting density has an error of around 50% and higher. 

When the simulation worked, the same checks as for Abrupt AdResS are required. The first check is about the density: if you have no thermodynamic force you will have rather pronounced spikes in the density at the interfaces. If you have a converged thermodynamic force the density has to be within +/- 3% (optimal) and +/- 5% (still valid) off from a comparable full atomistic simulation / experimental data. However, an "artificial" interface is introduced and checks for the diffusion, the RDFs, etc. (full list see below), ensure that the regions mix together and that you have proper particle transfer.


This is an example of test scenario for GROMACS version 5.1.5 with a possible CG potential and all necessary input files, see `<https://gitlab.e-cam2020.eu:10443/abrupt_adress/abrupt_adress>`_ . To run it simply run *gmx grompp -f grompp.mdp -c conf.gro -p topol.top -n index.ndx -maxwarn 5; gmx mdrun* using the patched version of GROMACS version 5.1.5 (see above). 

When *gmx mdrun* finishes normally (with the above mentioned setup), we have several mandatory checks to see if the simulation was successful or not.
  
0) Easiest check: load the conf.gro and the trajectory file in vmd and check if you see particle diffusion or depleted areas.
  
1) we check the density along the X-direction (*xsplit*: e.g. gmx density -f traj_compt.xtc -d X) or along the radius (*sphere*: e.g. via VOTCA: *csg_density --axis r --rmax <value> --ref [x_ref,y_ref,z_ref] --trj traj_comp.xtc --top topol.tpr --out test.dens.comp*), the density has to be less then 3% different from experimental data or the density from a full atomistic MD simulation. The density of the example is 1000 kg m^-3. 

2) static properties: crucial RDFs (e.g. for water the oxygen-oxygen RDF) 

  
3) p(N): It describes the average number of particles in the AT region throughout the simulation.

  
4) the density diffusion for each region (via a very helpful expansion for `VMD <http://www.ks.uiuc.edu/Research/vmd/>`_, the density profile tool see `Link: <https://github.com/tonigi/vmd_density_profile>`_).

  
5) If we only thermalize the transition region, the AT region is NVE-like, which means it is even possible to determine the dynamics of the system.


Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_

To apply the patch: 
1) copy into the main directory (gromacs/)
2) patch < localT_abrupt_adress.patch

The patch for Abrupt_AdResS can be found here:(:ref:`localT_abrupt_adress`)

.. toctree::
   :glob:
   :maxdepth: 1

   localT_abrupt_adress

..  Remember to change the reference "patch" for something unique in your patch file subpage or you will have
    cross-referencing problems





