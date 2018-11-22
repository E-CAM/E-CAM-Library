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
    See GROMACS web page: `<http://www.gromacs.org/>`_

  Documentation Tool

  Application Documentation
    See GROMACS web page: `<http://www.gromacs.org/>`_

  Relevant Training Material
    See GROMACS web page: `<http://www.gromacs.org/>`_


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _example:

#######################################################
Abrupt GC-AdResS: A new and more general implementation  
#######################################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

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
class. MD packages. The current implementation of GC- AdResS in GROMACS has several performance problems. We know that the main performance loss of AdResS simulations in GROMACS is  in the neighboring list search and the generic serial force kernel, linking the atomistic (AT) and coarse grained (CG) forces together via a smooth weighting function. Thus, to get rid of the bottleneck with respect to performance and a hindrance regarding the easy/general implementation into other codes and thus get rid of the not optimized force kernel used in GROMACS we had to change the neighborlist search. This lead to a considerable speed up of the code. Furthermore it decouples the method directly from the core of any MD code, which does not hinder the performance and makes the scheme hardware independent. 

.. The interface between the regions is more fluctuating and needs a more responsive thermodynamic force but it works reasonably well. 

.. The second piece of the puzzle is the spatial partitioning as we showed at the ESDW8 in Berlin and as Guzman et al. (arXiv:1711.03290v1) published recently it is possible to use a spatial partitioning for GC-AdResS. 


Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment
This module presents a very straight forward way to implement a new 
partitioning scheme. And this
solves two problems which affect the performance, the neighborlist 
search and the generic force kernel. 

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

But only with a force capping this scheme works for larger or more complex molecules.

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_

.. .. literalinclude:: ./abrupt_adress.patch
   :language: c
To apply the patch: 
1) copy into the main directory (gromacs/)
2) patch < abrupt_adress.patch

The patch for Abrupt_AdResS can be found here:

.. toctree::
   :glob:
   :maxdepth: 1

   abrupt_adress

..  Remember to change the reference "patch" for something unique in your patch file subpage or you will have
    cross-referencing problems

For the patch see reference :ref:`abrupt_adress`


