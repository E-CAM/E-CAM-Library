..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    Tools for AdResS.

  Language
    C/C++, Phyton, Fortran, BASH, AWK

  Licence
  Opensource

  Documentation Tool
    none
    
  Application Documentation
    none
	
  Relevant Training Material
    none
	
..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

################
Tools for AdResS
################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

Purpose of Module
_________________

One purpose of our project is to promote GC-AdResS as a method. It is an advanced method, for people with experience, and once the simulation is done there are several properties and checks to consider to make sure that the simulation was successful.

This module provides little tools to make working with AdResS easier. 
Content:

1) how to mask the configuration (output from a full atomistic simulation run) to generate the double resolution configuration.

2) Quick and dirty: get the reference coordinate from the GROMACS input file 

3) Checks for the density (for both geometries currently implemented in GROMACS version 5.1.5)

4) Check the temperature on the fly

5) A short fortran code to calculate the distribution of the angles in slab like AdResS simulation.


.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment


Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment


Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_

quick and fast data grab from the configuration file:
::

  tail conf.gro | awk '(NF==3){print $1/2.0,$2/2.0,$3/2.0}' 


how to mask the configuration for setting it up for the AdResS simulation:
::

  csg_map --cg mapping_scheme.xml --hybrid --trj input_file.gro --out output_file.gro --top atomistic_run/topol.tpr


checkout temperature on the fly from the output md.log:

:: 

  #!/bin/bash
  grep -A 1 --no-group-separator Lambda md.log | grep -v Step | awk '{print $1}' >   mdlogging1
  grep -A 1 --no-group-separator Temp md.log | grep -v Temp | awk '{print $2}' > mdlogging2
  paste mdlogging1 mdlogging2 
  paste mdlogging1 mdlogging2 >temperature
  rm mdlogging1 mdlogging2 


  
Quick grab of the density in the xsplit (slab like) configuration:

::

  gmx density -d X -f trajectory_file.xtc -sl 50


Quick grab of the density in the sphere configuration:

::
  
  csg_density -- axis r -- rmax 10. --ref [x_ref,y_ref,z_ref] --trj trajectory_input.xtc --top topol.tpr --out SOL.dens.out 


Collect the p(N) data and combine them in one file:

::

  vmd -dispdev text -e extract_coord.tcl
  grep -B1 "Frame" WCG.xyz > a
  sed '/Frame/ {$!N;d;}' a > column2
  grep -B0 "Frame" WCG.xyz > a
  sed -i s/Frame// a
  sed -i s/--// a
  sed -i s/:// a
  sed '/^$/d' a > column1
  paste column1 column2|awk '{print $1, $2}' > dat.3nm.pn.WCG.dat


And the corresponding extract_coord.tcl

::

  package require topotools 1.2
  mol new conf.gro
  mol addfile traj_comp.xtc type xtc waitfor all first 0 last -1 step 1
  topo writevarxyz WCG.xyz selmod "name WCG and (x>285 and x<315)"
  exit


Files:

:download:`analyse tools source code <analyse.tools.tar.gz>`

It is important to have the XDR files and setup in the same directory as they have to be specified in the Makefile. The XDR files can be found via the GROMACS web page, see `<http://www.gromacs.org/Developer_Zone/Programming_Guide/XTC_Library`_ or  `<ftp://ftp.gromacs.org/pub/contrib/xdrfile-1.1.4.tar.gz>`_.
