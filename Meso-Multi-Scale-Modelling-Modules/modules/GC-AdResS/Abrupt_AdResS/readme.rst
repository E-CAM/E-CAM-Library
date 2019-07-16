..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    GC-AdResS: Abrupt scheme

  Language
    Implemented in GROMACS version 5.1.5  

  Licence
    MD Simulation:

    See GROMACS web page: `<http://www.gromacs.org>`_
    
    Analysis tools and thermodynamic force calculation:
    
    see VOTCA web page: `<http://www.votca.org/home>`_

  Documentation Tool
  
  Application Documentation
    
  See GROMACS web page: `<http://www.gromacs.org>`_
  
  See VOCTA web page: `<http://www.votca.org/Documentation>`_

  Relevant Training Material

  See GROMACS web page: `<http://www.gromacs.org>`_

  See VOCTA web page: `<http://www.votca.org/tutorials>`_



..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _abrupt_adress:

#######################################################
Abrupt GC-AdResS: A new and more general implementation  
#######################################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

The original idea of our proposal: to work on a general implementation of AdResS in
class. MD packages. The current implementation of GC- AdResS in GROMACS has several performance problems. This module presents a very straight forward way to implement a new 
partitioning scheme, which solves two problems which affect the performance, the neighborlist 
search and the generic force kernel. Furthermore, we update the implementation to address this in a way that decouples the method directly from the core of any MD code, which does not hinder the performance and makes the scheme hardware independent.
Theory, application and tests see `<https://aip.scitation.org/doi/10.1063/1.5031206>`_ or `<https://arxiv.org/abs/1806.09870>`_.


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

The main performance loss of AdResS simulations in GROMACS is  in the neighboring list search and the generic serial force kernel, linking the atomistic (AT) and coarse grained (CG) forces together via a smooth weighting function. Thus, to get rid of the bottleneck with respect to performance and a hindrance regarding the easy/general implementation into other codes and thus get rid of the not optimized force kernel used in GROMACS we had to change the neighborlist search. This lead to a considerable speed up of the code. Furthermore it decouples the method directly from the core of any MD code, which does not hinder the performance and makes the scheme hardware independent. For the theory, application and tests see `<https://aip.scitation.org/doi/10.1063/1.5031206>`_ or `<https://arxiv.org/abs/1806.09870>`_.


.. The interface between the regions is more fluctuating and needs a more responsive thermodynamic force but it works reasonably well. 

.. The second piece of the puzzle is the spatial partitioning as we showed at the ESDW8 in Berlin and as Guzman et al. (arXiv:1711.03290v1) published recently it is possible to use a spatial partitioning for GC-AdResS. 


Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module presents a very straight forward way to implement a new partitioning scheme. And this
solves two problems which affect the performance, the neighborlist search and the generic force kernel. 


In GROMACS the neighbor list is put together and organized in the file 'ns.c'. In GROMACS 5.1
there are two functions which basically sort the incoming
particles into the different neighbor list. In its current official GROMACS release everything other than CG (with :math:`w_i=w_j=1`) or AT (with :math:`w_i=w_j=0` ) is sorted into the neighbor lists. Any other particles are sorted into a special neighbor list only for AdResS. 


We now changed this neighborlist sorting into: Everything is taken into account other than: (AT and ( :math:`w_i=0` or :math:`w_j=0`)) or (CG and ( :math:`w_i>=0` and :math:`w_j>=0`)). This leads to 5 distinct interactions: (1) AT-AT in the atomistic region, (2) CG-CG in the
CG region, (3) AT-AT between particles in the hybrid region, (4) AT-AT between particles of the
atomistic region with the hybrid region and (5) CG-CG between particles of the CG region with the
hybrid region. This if statement excludes the CG-CG interaction in the hybrid region. 

This is a very straight forward way to implement a new partitioning scheme and utilize a constant weighting function. This solves both parts of the performance problem, the neighborlist search and the generic force kernel which can be simply switch off by switching to the standard interaction scheme implemented in GROMACS. 

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

We tested this new implementation on SPC water with varying system sizes. GROMACS is optimized especially for handling of bio-systems, i.e. GROMACS has the best performance in case of water simulations. We set up a couple of Abrupt GC-AdResS simulations ranging from small 6912 water molecules to 48k water molecules. We used a standard desktop machine (Intel Core i5-4590 CPU @ 3.30GHz x4) and run small 20 ps runs. We can see that our performance is much improved up to a factor of 2.5.

Here is a short manual on how to run the test and set up AdResS simulations in GROMACS:

1) mandatory requirement: a working full atomistic simulation (molecular configuration, force fields and optimal MD input parameter)

2) You need a very well converged NVT run, which can be used as starting point for the coarse grained (CG) and then later the AdResS simulations.

3) You have to generate a coarse grained (CG) potential. We use, for convenience, the inverse Boltzmann iteration provided in the VOTCA package (`<http://www.votca.org/home>`_). The resulting tabulated CG potentials are used in the AdResS simulation. Alternatively you can use WCA potentials or standard Lennard-Jones potentials. The main requirement for the AdResS simulation is that the density in the CG region is the same as in the atomistic (AT) region. 

   NOTE: The method can be used with any potential, which preserves the correct density. If only a SPC/E CG potential is available it can be used for SPC/e water models as well as for a more advanced water model. It is possible to use a WCA potential, which is basically a Lennard-Jones potential. *And* it is possible to switch the CG ptential completely off. That will transform the CG region to a true thermodynamic reservoir with a non-interacting gas.

4) The next step is to create a double resolution configuration and adjust the dependencies (force field, topology, index file, GROMACS input file). Creating the configuration is straight forward (we use `<http://www.votca.org/home>`_).

::

  Example from VOTCA: 
  csg_map --top topol.tpr --cg cg_mapping_scheme --hybrid --trj conf.gro --out conf_hybrid.gro

Of course, if you want to use this configuration in a MD simulation you have to adjust the force field (see example file: *spc.adress.itp*). You have to define a virtual side:  

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
  
  Found 3456 atoms with name WCG

  3 WCG                 :  3456 atoms
  
  > !3
  
  Copied index group 3 'WCG'
  Complemented group: 10368 atoms

  4 !WCG                : 10368 atoms
  > name 4 EXW
  > q


The next step is to adjust the GROMACS input file. AdResS needs the Langevin dynamics, so you have to choose: 

::

  integrator = sd 

Since the system is double resolution, meaning we have the atomistci detals and the virtual particles, we have to define the energygroups:

::

  ; Selection of energy groups 
  energygrps = EXW WCG 
  energygrp_table = WCG WCG

GROMACS  version 5.1.5 is using verlet as standard cutoff-scheme, so we have to change that to *group*:

::

  ; nblist update frequency 
  cutoff-scheme = group 

Furthermore, in our simulations we use:

::

  coulombtype = reaction-field 
  rcoulomb = 1.0 
  vdw-type = user  
  rvdw = 1.0 
  
In case of local thermostat simulations (see `<https://aip.scitation.org/doi/10.1063/1.5031206>`_ or `<https://arxiv.org/abs/1806.09870>`_) we use:

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



  ; AdResS parameters 
  adress = yes ;no 

Here you define the geometry of the atomistic region, either *sphere* (a spherical region anywhere in the simualtion box) or *xsplit* (a cuboid slice of the whole simulation box for the atomistic region, with the transition and coarse grained region on each side). 

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

*adress_interface_correction* defines if you use an external force to correct the density or not. In case of the old AdResS (smooth coupling) that correction simply refined the simulation, as the density difference was not significant. For the Abrupt AdResS, and the method development based on it, and more complex molecules (i.e. polymers) the thermodynamic force is essential. If it is not taken into account the risk to form interfaces between AT and CG is high. Also if particles coming too close (basically overlap) the run can crash. The role of the thermodynamic force, the force cap and the basic theory behind it see `<https://aip.scitation.org/doi/10.1063/1.5031206>`_ or `<https://arxiv.org/abs/1806.09870>`_. For this to work you must have a file e.g. in our example case: *tabletf_WCG.xvg* in the directory, otherwise you have to set:

::

  adress_interface_correction =  off

There is a number of properties you have to check. The first check is always the density and you see if the patch works from the density. If you have no thermodynamic force you have rather pronounced spikes in the density at the interfaces. If you have a converged thermodynamic force the density has to be within +/- 3% off from a comparable full atomistc simulation / experimental data. Then you need further properties to make sure you have an open system. The problem with the simulation is that an "artificial" interface is introduced and checks for the diffusion, the RDF's... (full list see below) ensure that those regions mix and that you have proper particle transfer.

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_

The patch file for Abrupt GC-Adress is:

.. literalinclude:: ./abrupt_adress.patch

To apply the patch: 

1) copy into the main directory (gromacs/)

2) patch < abrupt_adress.patch

..  Remember to change the reference "patch" for something unique in your patch file subpage or you will have
    cross-referencing problems

In this module we also include a test scenario for GROMACS version 5.1.5 with a possible CG potential and all necessary input files. To run it simply run *gmx grompp -f grompp.mdp -c conf.gro -p topol.top -n index.ndx -maxwarn 5; gmx mdrun* using the patched version of GROMACS version 5.1.5 (see above). 

When *gmx mdrun* finished normally (with the above mentioned setup), we have several mandatory checks to see if the simulation was successful or not.
  
0) Easiest check: load the conf.gro and the trajectory file in vmd and check if you see particle diffusion or depleted areas.
  

1) we check the density along the X-direction (*xsplit*: e.g. gmx density -f traj_compt.xtc -d X) or along the radius (*sphere*: e.g. via VOTCA: *csg_density --axis r --rmax <value> --ref [x_ref,y_ref,z_ref] --trj traj_comp.xtc --top topol.tpr --out test.dens.comp*), the density has to be less then 3% different from experimental data or the density from a full atomistic MD simulation. The density of the example is 1000 kg m^-3. 
  

2) static properties: crucial RDF's (e.g. for water the oxygen-oxygen RDF) 

  
3) p(N): It describes the average number of particles in the AT region throughout the simulation.

  
4) the density diffusion for each region (via a very helpful expansion for `<http://www.ks.uiuc.edu/Research/vmd/>`_, the density profile tool see `<https://github.com/tonigi/vmd_density_profile>`_).

  
5) If we only thermalize the transition region, the AT region is NVE-like, which means it is even possible to determine the dynamics of the system.

The files for the water example can be found here:
:download:`spc-example.tar.gz <spc-example.tar.gz>`

