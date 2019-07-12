..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    GC-AdResS -Abrupt scheme- Force Capping

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
 _example:

#######################
Abrupt-AdResS: Forcecap
#######################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

The original idea of our proposal was to work on a general implementation 
of grand canonical adaptive resolution simulations (GC-AdResS) in
classical MD packages. The current implementation of GC- AdResS in GROMACS (up to version 5.1.5) has performance problems. The Abrupt GC-AdResS implementation is avoiding those and make AdResS more interesting for other MD developers (especially since we could remove the force interpolation and weighting functions from the force kernel).

This module works in combination with abrupt_AdResS and is at the same time important for a successful simulation. It shows how to avoid particle overlap at the interface of the atomistic and coarse grained regions.

   
Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The implementation of Abrupt GC-AdResS is in itself only working for the smallest and simplest of molecules without problems. For larger and more complex molecules the simulation crashes. This module shows a way to avoid this. 



Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

As studies of ionic liquids and polymer melts have shown for large and complicated molecules even the standard GC-AdResS is not working without additional force capping. The reason for that is when a molecule from the coarse grained region enters the hybrid region the atomistic representations, which are present due to the technically necessary double resolution, interact. It is possible for atoms to be too close together, which results in a too high force and thus in too high velocities of those particles. Since in Abrupt AdResS we can avoid the generic force kernel from GROMACS, the force capping (which was previously implemented at the end of the force calculation) had to be shifted and replaced. We finally looked at the integrator (in our case the stochastic dynamics integrator), which is the place where each force has to be read and handled. 

This implementation of force capping is a rudimentary approach. The basic principle is when two particles are too close together, and thus the force are far higher than the average forces in the simulation, the force on the particles are re-scaled to a given value. That tactic makes sure that the insertion of particles in the atomistic region is introduced at reasonable velocities and temperature. As a side effect, the area of disturbance due to the introduction of a particle is limited.


Building and Testing
____________________

.. Keep the helper ttxt below around in your module by just adding "..  " in front of it, which turns it into a comment


We have used this new addition to the code on two systems: water and ionic liquids. The results have been published in Ref. `<https://aip.scitation.org/doi/10.1063/1.5031206>`_ or `<https://arxiv.org/abs/1806.09870>`_.  All the information about studied systems and the performance can be found there.

The patch provided can be applied alone without the Abrupt AdResS patch, in the main directory of GROMACS. 
The important part of the patch is that it adds an upper force limit in the stochastic dynamics integrator. If that upper limit is triggered the force is re-scaled to the given force cap. The rest is basically to make sure that the integrator is called correctly.
There is a *print* command which is triggered once the force on a particle is higher than a given force cap value. The force capping simulation can in some cases cause lincs warnings. Since we take care of faulty configuration that way, we can disabling those warnings (*export GMX_MAXBACKUP=-1* ; *export GMX_MAXCONSTRWARN=-1*). Otherwise it generates too many files and can crash as well.  


Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_

A note of caution: the chosen force cap trigger has to be a high enough value, otherwise normal interactions (interactions with forces around the average forces in a simulation)  will trigger the force capping. That would change the dynamics and the structure of a system. Also it would decrease the performance of the code. If chosen too high it might run into impossible and unstable configuration, which will result in a program crash.

Recipe for hard coded force capping:

.. literalinclude:: ./forcecap.patch

with:

fc = Chosen upper force limit for the ionic liquids simulations 

fn = force acting on a particle 


The patch provided can be applied in the main directory of GROMACS via:

::

  patch < forcecap.patch




