********************************************************************************
Comparative Metadynamics
********************************************************************************

.. sidebar:: Software Technical information

    This module facilitates extrapolating free energy surface (FES) feature information from short, non-converged 
    simulations of mutated systems
    
    Language 
        Python (3+)
    
    Licence
        This module is licensed under the terms of the `CC BY-NC-SA 4.0 
        <https://creativecommons.org/licenses/by-nc-sa/4.0/>`_ licence 
    
    Documentation Tool
        `pdoc`_, numpydoc format (ReST)    
    
    Application Documentation
        `Documentation <https://gitlab.e-cam2020.eu:10443/aestheses-public/comparative-metadynamics/-/tree/master/docs>`_

    Relevant Training Material
        See usage example at
        https://gitlab.e-cam2020.eu:10443/aestheses-public/comparative-metadynamics/-/tree/master/examples

    Software Module Developed by
        Zein Jaafar, Shrinath Kumar and Donal MacKernan

..  contents:: :local:

********
Abstract
********

The module performs a long simulation of some given system and then many shorter simulations of mutations of the 
aforementioned system. Using the Free Energy Surface (FES) of the original system as a basepoint allows for meaningful 
information about the impact of a mutation on the system's FES to be extracted from only the short simulations.

**********************
Background Information
**********************

The use of Molecular Dynamics (MD) is highly relevant in nearly all STEM fields. Analysing MD simulations can be done 
by defining Collective Variables (CVs), functions of the positions of some or all of the atoms in a simulation. Then, 
periodically during the course of an MD simulations, the energy of the system is computed alongside all of the defined 
CV's. This allows the construction of a Free Energy Surface (FES) by expressing the free energy as a function of the 
CV's. In order to speed up the exploration of the CV space, a method called Metadynamics may be employed where a 
biasing potential is added to force the system to explore the CV space rather then allowing it to naturally explore 
the entire CV space as in regular MD.

Optimisation through mutation is a process whereby a system is optimised to perform some specific task by mutation, 
which broadly encompasses altering the system in any way. If the system's ability to perform said task can be 
characterised through the use of CV's then its ability to perform this task will manifest in some feature or 
collection of features in the FES. Thus, the process of optimisation through mutation will break down broadly into 
three steps, which are usually repeated many times. 

1. Mutating the system
2. Simulating through Metadynamics
3. Analysing the FES

*****************
Purpose of Module
*****************

The purpose of this module is to speed up the process of optimisation through mutation by quickly classifying roughly 
how much a mutation will optimise the system or not. This quick classification will allow a much wider exploration of 
the possible mutations which might optimise a system.

This is done by using the FES of the original system as a starting point. In order to obtain this FES a well-converged 
simulation for the original system must be conducted. Then a feature of interest on the FES is chosen and potential 
walls are placed around it to limit the exploration of the CV space and further speed up simulation. The system is 
mutated and then a very short metadyanmics run is performed on the mutated system. The key point is that when 
simulating the mutated system with metadynamics, the biasing potential used to generate the original system's FES is 
used as the initial biasing potential for the mutated system.

The reason for this is so that, if run for sufficiently long, metadynamics will gradually alter the profile of the 
original FES until it matches that of the mutated system's FES. Therefore even after a very short simulation which 
has not yet converged, it is possible to compare the original FES to the mutated system's FES and extrapolate what 
effect the mutation had on the FES; In particular it can be inferred whether the mutation has optimised the original 
system or not.

By performing many such mutations and short simulations this module also allows a rough comparison between which 
mutations best optimised the system by comparing which mutations caused the greatest change in the original FES in a 
fixed time interval. Thus, this module allows one to test many mutations and narrow down which ones will best optimise 
their system.

************
Applications
************

This module is particularly relevant to anywhere MD is being used to design systems through an iterative process such 
as chemical or biological labs. However, it can also be applied to areas where one needs to analyse many similar 
systems through MD.

***********
Performance
***********

For a simple water in salt system, when changing the charge on the salt ions a simulation time of 100ps was sufficient 
to analyse the changes that had occurred in the FES. By contrast a full simulation of the system required at least 4ns 
to converge.

**********************
Software Prerequisites
**********************
The core software requirements are:

1. Python 3
    * Numpy

2. Plumed 2.5+

In addition, an MD engine is required needed to run the simulations. To run the example provided the following 
additional software is required:

3. Lammps (MD engine)
4. Moltemplate (To perform mutations)
5. Additional Python 
    * mpi4py
    * matplotlib


*****
Usage
*****

This module mutates a system and then runs a metadynamics simulation of them using lammps. Thus the user needs to 
provide 3 scripts in advance.

These three files are as follows:

1. A plumed data file for performing metadynamics
2. A python file which will mutate their system
3. A python file which will simulate their system

These files should all be stored in the same location as indicator_run.ipynb
Example files are provided which explain how the file should be constructed.
Once these three files are in place indicator_run.ipynb may be run. It will guide the user through any inputs required.

A brief summary of what indicator_run.ipynb does is provided below

1. Simulate the original system and save the metadynamics info into a file
2. Perform some mutations to the system using the user provided script.
3. Run multiple shorter simulations starting off where the initial simulation ended using the user provided script to 
   simulate

4. Save and store the resulting outputs from each simulation in an accessible manner

Once this is complete the user may use the Analysis subfolder to analyse the output of the simulations
In this folder a single plumed data file needs to be created. An example file is provided which may also be used
Then the file analyse.ipynb may be run. Again, this file will guide the user through the necessary steps

A brief summary of what analyse.ipynb does is provided below

1. Reads in all the the data created by indicator_run.ipynb (the COLVAR files mainly)
2. Creates histograms/probability densities from the restarted simulations
3. Runs a function that analyses the histograms which is user defined (e.g. the function might return the difference 
   between the max and min value of the FES)
4. Visualises the resulting data

********
Examples
********

Examples can be found in the examples folder.

.. _pdoc: <https://pdoc3.github.io/pdoc/>
.. _LICENSE:
