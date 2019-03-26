..  deIn ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    RDF's via Visualize Molecular Dynamics.

  Language
    TCL scripting language.

  Licence
    See http://www.ks.uiuc.edu/Research/vmd/allversions/disclaimer.html

  Documentation Tool
    none
    
  Application Documentation
    http://www.ks.uiuc.edu/Research/vmd/current/docs.html
    
  Relevant Training Material
    http://www.ks.uiuc.edu/Research/vmd/current/docs.html

..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

###########################################
Radial Distribution Functions for GC-AdResS 
###########################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

Purpose of Module
_________________

One purpose of our project is to promote GC-AdResS as method which provides new insights and is not much more complex 
and difficult to use. And a good way is to use existing codes, if they can provide the informations needed. 
In GC-AdResS simulations we introduce artificial interfaces, from atomistic to hybrid and hybrid to coarse grained. To make 
sure that we indeed have an open system we have to check several properties, from structural to dynamic properties. 
Radial distribution functions (RDF'S) are the easiest way to check the structural properties of the 
simulation. This module is dedicated to describe a straight forward and easy way to generate them for the atomistic 
regions in the GC-AdResS simulations. In the current implementation in GROMACS we have two geometric setups. One is radial 
and the other is a slab like structure. We use VMD as the tool sof choice to calculate the RDF's. 

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment


Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The most widespread tool for analysing molecular dynamics simulations is `VMD <http://www.ks.uiuc.edu/Research/vmd>`_). 
The program is based on TCL and Tk scripting language. Documentation and tutorials can be found 
here: `VMD <http://www.ks.uiuc.edu/Research/vmd/current/docs.html>`_ 

The provided inputs (reference coordinates, configuration and trajectory are necessary to run AdResS) can be used for two essential analysis parts for AdResS. The structural part, the radial distribution functions of the simulated system. And the second part with the help of the `Density Profile tool <https://github.com/tonigi/vmd_density_profile>`_ show the diffusion of the molecules during the simulation.


Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_

We basicaly work with the atomselection and use the pre-existing Radial Pair Distribution Function tool in VMD. One has to load the configuration file (standard is conf.gro) and the trajectory file (standard: traj_comp.xtc) via either *vmd conf.gro traj_compt.xtc* or the vmd GUI.  


Example case (for the radial systems), this selection was used with the reference point being defined in the GROMACS input file. 

:: 

   x_ref = x-value of the center of the chosen AdResS region
   y_ref = y-value of the center of the chosen AdResS region
   z_ref = z-value of the center of the chosen AdResS region

   radius: radius of the atomistic region
   name "insert your choice of atom here" and (((x-x_ref)^2 + (y-y_ref)^2 + (z-z_ref)^2) < radius*radius )

For the slab structures:

::

   at_start: start of the atomistic regions along the x axis
   at_end: end of the atomistic regions along the x axis
   hy_start: start of the hybrid regions along the x axis
   hy_end: end of the hybrid regions along the x axis

   name "insert your choice of atom here" and (x>at_start and x<at_end)
   name "insert your choice of atom here" and ((x>hy_start and x<at_start) or (x>at_end and x<hy_end) 


