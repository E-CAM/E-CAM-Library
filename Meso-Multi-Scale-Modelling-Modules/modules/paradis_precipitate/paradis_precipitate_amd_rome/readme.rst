..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    ParaDiS_Precipitate_HPC


  Language
   C++

  Licence
    This is patch based on the ParaDIS version 2.5.1. The additions are GPL.

  Documentation Tool
    Sphinx
     
  Application Documentation
    http://paradis.stanford.edu/

  Relevant Training Material
    https://version.aalto.fi/gitlab/csm_open/paradis_version_diffs/tree/master/test_run


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _paradis_precipitate_hpc:

######################################################
ParaDiS with precipitates optimized to HPC environment
######################################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

Discrete dislocation dynamics (DDD) simulations usually treat with "pure" crystals and dislocations in them. In reality, there is a need to look at more 
complicated scenarios of impurities interacting with the dislocations and their motion. Effects on a single atom / vacancy level may be 
incorporated by renormalizing the dislocation mobility but in many cases the dislocation dynamics is changed by the presence of clusters or precipitates,
that act as local pinning centers. The consequences of the impurities are multiple: the yield stress is changed, and in general the plastic deformation
process is greatly affected. Simulating these by DDD allows to look at a large number of issues from materials design to controlling the yield stress and
may be done in a multiscale manner by computing the dislocation-precipitate interactions from microscopic simulations or by coarse-graining the DDD 
results for the stress-strain curves on the mesoscopic scale to more macroscopic Finite Element Method (the material model therein).

This module provides 
an extension of the ParaDIS DDD code (LLNL, http://paradis.stanford.edu/) where dislocation/precipitate interactions are included. The extension is for an HPC environment, in which the original code has been optimized for the Mahti cluster running on AMD Rome environment at CSC in Finland in mind. 

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The method is based on extending a recent version of ParaDIS to handle the presence of pinning centers. These work as localized Gaussian potentials that
interact with the near-by dislocations (see A. Lehtinen et al. Phys. Rev. E 93, 013309 (2016)). The "disorder field" is given as an input where the locations
of the precipates are given in 3D, and the interactions are parametrized by the impurity strength (which may vary from precipitate to another) and the range
of the Gaussian potential (which also may vary). The dislocation dynamics is handled as in ParaDIS in general with an additional force terms that accounts for
each dislocation segment for the nearby impurities (a cut-off is applied in the force).

The Module thus allows to study various precipitate fields (density, geometry, strength, interaction range) as desired. In a typical ParaDIS simulation one
does a simulation of the response of a dislocation system to a strain/stress protocol. The starting point is a dislocation system, which has been obtained from
relaxing a random or patterned configuration under zero external stress until the evolution becomes negligible. In the presence of impurities the customary approach 
is to do two relaxation steps: first follow the relaxation of dislocation configuration, then add the disorder field to that and re-relax. In the current version apart from HPC-related parallelization-relevant steps the subroutines SegSegForce (segment-to-segment force calculation) and FMSigma2Core2 (force multipole expansion) are well vectorized, and the code now also uses better multiple threads in their context. 

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The module version is built on the ParaDIS version 2.5.1 which can be obtained from http://paradis.stanford.edu/ and 
following the steps outlined there for obtaining the code.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The version offered is built exactly like the normal ParaDIS; the makefiles etc. are for the local CSC system and should be
modified for the local environment. To test the ParaDiS build, an example case of a constant strain rate simulation of BCC iron with precipitates is included.
The input of the test simulation is in file ParaDiS_test.ctrl, where the output directories and the used number of computational domains need to be defined. 
The initial dislocation structure is contained in the ParaDiS_test.data as usual and the structure of the file is identical to the files used by default ParaDiS. 
In addition, the simulation has ~8500 precipitates which are included in the ParaDiS_test.pdata file. This .pdata file has first some domain variables defined similar to .data file,
and then the precipitates. These are presented one precipitate per line, and the data columns are as follows: [precipitate tag, position x, y and z,
impurity strength, interaction radius, boolean], where the boolean states if the precipitate is active.

The used printing options defined in .ctrl file can be modified. Here, examples of the output property data and restart files are included in run_output folder
and the file called ParaDiS_test.out contains the standard output of the test when the simulation system is run for ~1.5e-9 seconds. The restart files are written 
similarly as in unmodified ParaDiS, except that now the precipitates are also included in corresponding rsXXXX.pdata files. In addition to the property files produced 
by original ParaDiS, the modified ParaDiS writes also files allepsdot and avalanche. Allepsdot contains columns [simulations time, strain rate tensor element 11, stress 
tensor element 11,...], and avalanche columns [time, average velocity, plastic strain, applied stress, total dislocation length, integrated strain rate] where the average
velocity is calculated as a segment weighted average velocity of dislocations.

The test case is illustrated with three files: ParaDiS_test.out, and two plots, which are:
aver_velocity_time.pdf (the resulting average dislocation velocity during the run) and stress_plastic_strain.pdf (yield strain versus applied stres during the run).




Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_



Due to licensing reasons, only the difference between ParaDiS version 2.5.1 files and modified files are submitted, and these files can be found in `<https://version.aalto.fi/gitlab/csm_open/paradis_version_diffs.git>`_. 

