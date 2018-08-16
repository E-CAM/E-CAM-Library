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
    Implemented in GROMACS version 5.1.0

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

######################
Abrupt_AdResS_forcecap
######################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

The original idea of our proposal was to work on a general implementation 
of grand canonical adaptive resolution simulations (GC-AdResS) in
classical MD packages. The current implementation of GC- AdResS in GROMACS (up to version 5.1.4)
has several performance problems. With the Abrupt GC-AdResS we intent to circumvent those and make AdResS more interesting for other MD developers (especially since we could remove the force interpolation from the force kernel).

This module works in combination with abrupt_AdResS. It shows a way to couple atomistic and coarse grained regions together and how to avoid particle overlapp at the interface of those two regions.

   
Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The implementation of Abrupt GC-AdResS is in itself only working for the smallest and simplest of molecules without problems. For larger and more complex molecules the simulation crashes.
This module shows a way to avoid the crash. 

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

As studies of ionic liquids and polymer melts have shown us for large and complicated molecules even the standard GC-AdResS is not working. The reason for that is when a molecules from the coarse grained region enters the hybrid region the atomistic representations, which are present due to the technically necessary double resolution, interact. It is possible for atoms to be too close together, which results in a too high force and thus in too high velocities of those particles. Since in Abrupt GC-AdResS we can avoid the generic force kernel from GROMACS the force capping (which was previously implemented at the end of the force calculation) had to be shifted. Several trials to put it directly after the force calculation didn't work out. So we finally looked at the one place where each force has to be read and handled, the (in our case) stochastic dynamics integrator. The force capping is rather simple. If the force gets too high it is re-scaled to a given value and therefor does not destroy the molecule and causes the program to crash.


Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

We have tested the new addition to the code on water systems (7k to 48k molecules) and the performance is up to a factor 2.5 faster. Furthermore we run a couple of ionic liquids simulations (1,3-Dimethyl-imidazolium chloride from 1000 ion pairs to 50000 ion pairs) and measured the performance. We can reach a performance up to a factor of 6 faster. 
For the dynamics and structure check we looked at the more complex 1000 ion pair system. We can reproduce the radial distribution functions in the atomistic region, the density in the atomistic, transition ($\Delta$) region is less than 3\% different from full atomistic simulations and the density diffusion profile shows a complete mixing of the different regions. 

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_

A note of caution: One has to chose a high enough force, otherwise normal interactions will trigger the force capping and unnecessary change the interactions. That would change the dynamics and the structure of a system. If chosen too high it might run into impossible and unstable configuration, which will also result in a program crash.

Recipe for hard coded force capping:

.. literalinclude:: ./forcecap.patch

with:
fc = Chosen upper force limit for the ionic liquids simulations 
fn = force acting on a particle 




