:orphan:

..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    Thermodynamic Force Calculator for Abrupt AdResS

  Language
    bash
    Python 2.7

  Licence
    MD Simulation:
    See GROMACS web page: `<http://www.gromacs.org/>`_
    
    Analysis tools:
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

.. _adress_tf:

################################################
Thermodynamic Force Calculator for Abrupt AdResS  
################################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

We introduced with the Abrupt AdResS method a new way of coupling the different simulation regions together. That is the basis for easier implementation into other codes. The implementation of smooth coupling GC- AdResS in GROMACS has several performance problems. However, the new Abrupt AdResS presents a very straight forward way to implement a new partitioning scheme, which solves two problems which affect the performance, the neighborlist search and the generic force kernel. Furthermore, we update the implementation to address this in a way that decouples the method directly from the core of any MD code, which does not hinder the performance and makes the scheme hardware independent.
Theory, application and tests see `<https://aip.scitation.org/doi/10.1063/1.5031206>`_ or `<https://arxiv.org/abs/1806.09870>`_. 

The drawback of this method is that a new (as in more direct) way to calculate the thermodynamic force is needed. The theory is still the same, the interpolation has to be adapted.


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

The new Abrupt coupling scheme introduces a density discrepancy which is very much restricted to the interface of the atomistic region and the coarse grained region. The thermodynamic force calculator in VOTCA (implemented up to version 1.3) is designed for the more smooth coupling over a larger region in space. Thus this code cannot be used for the small area of disturbance in this new scheme. 

Here we present a thermodynamic force calculator for the abrupt coupling scheme. It is a mix between bash and phyton and can be applied even to the older smooth coulping scheme.

.. The interface between the regions is more fluctuating and needs a more responsive thermodynamic force but it works reasonably well. 

.. The second piece of the puzzle is the spatial partitioning as we showed at the ESDW8 in Berlin and as Guzman et al. (arXiv:1711.03290v1) published recently it is possible to use a spatial partitioning for GC-AdResS. 


Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

Abrupt AdResS presents a very straight forward way to implement a new partitioning scheme. The drawback is that the particles mix in a very narrow region in space. The distance of the molecules at that interface can be too close, which has to be compensated via a force capping. However the flux of particles at that interface is fast, which leads to a rather localized discrepancy in the density. 

The thermodynamic force calculator in VOTCA (up to version 1.3) is designed for a smooth and not very much disturbed region in space. Thus a new code to calculate the thermodynamic force was needed. The thermodynamic force is calculated by calculating the gradiant of the density in a specific region in space. Thus any code taking this into account can be used. For the detailed discussion of the role and the basic principles behind this force see `<https://aip.scitation.org/doi/10.1063/1.5031206>`_ or `<https://arxiv.org/abs/1806.09870>`_.

The code provided here is designed for easy adjutable and can be used on different computer architectures (knowledge of bash is of an advantage).


Building and Running
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

These three scripts present one way to calculate the thermodynamics force.

::

:download:`TF calculation script: <./TF_calc_water_xplit_sphere.sh>`
:download:`Density interpolation script: <./smooth_dens.sh>`
:download:`Script to call TF calculation: <./run_tf_water_xplsit_sphere.sh>`


The central script is *smooth_dens.sh*. This is a phyton 2.7 script which interpolates the density and generates the gradiant of the density and provides the force as an ascii table. 

*TF_calc_water_xplit_sphere.sh* is controlling the MD run, and which region to interpolate, and builds the tables needed. The commands and the options used are described in `<http://www.gromacs.org/>`_ or if the  spherical geometry for AdResS is used also here: `<http://www.votca.org/Documentation>`_.

*run_tf_water_xplsit_sphere.sh* provides some possible input scenarios for *TF_calc_water_xplit_sphere.sh*. 

To run *run_tf_water_xplsit_sphere.sh* one has to first enter the correct region, which should be interpolated in *TF_calc_water_xplit_sphere.sh*. 

::

  rmin = starting point along a distance with rref as origin
  
  rmax = end point of the interpolation region
  
  rbox = maximal box size (xsplit: x direction; sphere the maximal radius)
  
  rref = is the point defined in the GROMACS input file
  
  lc = defines the binning along the x direction or the radius
  
  prefac = is basically a weighting on the thermodynamic force (small: more iteration, but more careful approach of the target density)
  

Note of caution: in *run_tf_water_xplsit_sphere.sh* and *TF_calc_water_xplit_sphere.sh* the GROMACS and VOTCA version used have to be specifically sourced. Then select which option in *run_tf_water_xplsit_sphere.sh* you want to use and comment the other out and execute:

::

   for a new run without a thermodynamic force to start with:
   
   bash run_tf_water_xplsit_sphere.sh  1 20 1
   
   
   for a start from an existing thermodynamic force:
   
   bash run_tf_water_xplsit_sphere.sh  21 20 2


Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_

:download:`TF calculation script: <./TF_calc_water_xplit_sphere.sh>`
:download:`Density interpolation script: <./smooth_dens.sh>`
:download:`Script to call TF calculation: <./run_tf_water_xplsit_sphere.sh>`
