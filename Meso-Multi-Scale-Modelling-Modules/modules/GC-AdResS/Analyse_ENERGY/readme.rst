:orphan:

..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    Energy (AT)/Energy(interface) ratio: Necessary condition for AdResS simulations.
    
  Language
    C/C++

  Licence
    none
    
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

###############################################################################
Energy (AT)/Energy(interface) ratio: Necessary condition for AdResS simulations
###############################################################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

Purpose of Module
_________________

One purpose of our project is to promote GC-AdResS as a method. It is an advanced method, thus for people with experience, and once the simulation is done there are several properties and checks to consider to make sure that the simulation was successful.

This module provides the code to check one very important quantity, the interaction energy in the atomistic region compared with the interaction energy in the comparable subregion in a full atomistic simulation. The difference between those energies is the interaction energy at the interface, which has to be much smaller than the interaction energy in the atomistic region.  

The theory  is described in `Ref. <http://iopscience.iop.org/article/10.1088/1367-2630/17/8/083042>`_.  This legacy code is based on that theory and has been developed to check the energy of the local thermostat GC-AdResS simulations presented in the paper cited above.


.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment


Running the code:
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The executable is called *energy*. The options on how to run the analysis:

::

     -h  : help message
     -b  : start frame  
     -e  : end  frame 
     -n  : number of regions 
     -x0 : start transition region 
     -x1 : end transition region (if x1 == 0, use the whole box)       
     -q1 : charge on oxygen
     -q2 : charge on hydrogen
     -sig : sigma      
     -eps : eps
     -beads :  no. of beads in one ring polymer 
     -c  : cut off radius for neighbor list search      
     -f  : the input .xtc file (default: traj.xtc)     
     -o  :the output file


Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_

Files are stored here: `<https://gitlab.e-cam2020.eu/krekeler/analyze.energy>`_. The source code for the energy calculation can be found here: `<https://gitlab.e-cam2020.eu/krekeler/analyze.energy/blob/master/app/energy.cpp>`_.
The installation instruction can be found `<https://gitlab.e-cam2020.eu:10443/krekeler/analyze.energy#installation-instructions>`_.

It is important to have the XDR files and setup in the same directory as they have to be specified in the Makefile. The XDR files can be found via the GROMACS web page, see `<http://www.gromacs.org/Developer_Zone/Programming_Guide/XTC_Library>`_ or `<ftp://ftp.gromacs.org/pub/contrib/xdrfile-1.1.4.tar.gz>`_. 
