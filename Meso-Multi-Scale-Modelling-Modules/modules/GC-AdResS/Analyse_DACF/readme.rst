:orphan:

..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    Dipole-Dipole autocorrelation function for AdResS.

  Language
    C/C++

  Licence
  Opensource

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

#################################################
Dipole-Dipole autocorrelation function for AdResS
#################################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

Purpose of Module
_________________

One purpose of our project is to promote GC-AdResS as a method. It is an advanced method, for people with experience, and once the simulation is done there are several properties and checks to consider to make sure that the simulation was successful.

This module provides the code to run a dipole dipole autocorrelation function on the current geometries available in the Abrupt AdResS implementation. The paper 
`Ref. <http://iopscience.iop.org/article/10.1088/1367-2630/17/8/083042>`_ describes the correlation functions and why they can be used in AdResS. This code is based on that theory and has been developed to check the dynamics of the local thermostat GC-AdResS simulations presented in the paper cited above.


.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment


Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

See :ref:`abrupt_adress`

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_

Usage:

:: 

 cal_dacf
  
  options: 
  -h  print this message
  -b start time 
  -e end time (=number of MD steps)
  --x0  lower bound of the interval
  --x1  upper bound of the interval (--x1 0, use the whole box = atomistic)
  --frame  length of correlation
  --q0 charge on oxygen 
  --q1 charge on hydrogen
  --acc breaks
  --total number of frames
  --tf  Output Frequency (=Delta_t)
  -m  type of simulation to analyze (adress or atom)
  -f  input .xtc file
  -o  output file
  
  
Source code:
Files are stored under `<https://gitlab.e-cam2020.eu/krekeler/analyze.energy>`_. The source code for the dipole autocorrelation function can be found at `<https://gitlab.e-cam2020.eu/krekeler/analyze.energy/blob/master/app/cal_dacf.cpp>`_.
The installation instructions are given at `<https://gitlab.e-cam2020.eu:10443/krekeler/analyze.energy#installation-instructions>`_. 


It is important to have the XDR files and setup in the same directory as they have to be specified in the Makefile. The XDR files can be found via the GROMACS web page, see `<http://www.gromacs.org/Developer_Zone/Programming_Guide/XTC_Library>`_ or `<ftp://ftp.gromacs.org/pub/contrib/xdrfile-1.1.4.tar.gz>`_. 

