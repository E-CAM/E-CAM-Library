..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    DL_MESO (DPD). 

  Language
    Fortran, CUDA-C.

  Licence
    `BSD <https://opensource.org/licenses/BSD-2-Clause>`_, v. 2.7 or later

  Documentation Tool
    ReST files

  Application Documentation
    See the `DL_MESO Manual <http://www.scd.stfc.ac.uk/SCD/resources/PDF/USRMAN.pdf>`_

  Relevant Training Material
    See `DL_MESO webpage <http://www.scd.stfc.ac.uk/SCD/support/40694.aspx>`_

  Software Module Developed by
    Jony Castagna


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _example:

##################################
Ewald on DL_MESO_DPD (GPU version) 
##################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

The electrostatic force calculation represent usually the main computational costs in systems where even a small amount of charged particles is present ($>1\%$).
The Smooth Particle Ewald Mesh \cite{SPME} splits the electrostatic forces in two parts: a short range, solved in the real space, and a long range, solved in the Fourier space.
An error weight function combines the two contributes. For the long range force the electrical charges are spread on a virtual particle mesh using a B-spline interpolation function.

Porting to GPU the full short and long range interactions allowed to maintain the speedup factor of $x4$ when compared to the a traditional Intel 12-core.

One of the main applications which included electrical charges are the simulations of plasma. 



Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The Ewald summation method above described scales with $N^{1.5}$ at best, where N is the number of charged particles. The SPME allows a better scaling, $N*log(N)$, 
but requires a stencil domain decomposition (i.e. decomposing the domain along one direction only) to allow the FFTW library scaling with more than 1 core.
If this is not used, as in the current master version of DL\_MESO\_DPD, the FFTW becomes rapidly a bottleneck for scaling across several nodes.
On the other side, the porting to a single GPU does not need domain decomposition and the same speedup factor ($x4$ compared to 12-core Intel) is mainteined.



Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment


This module is part of the DL_MESO_DPD code. Full support and documentation is available at:

* http://www.scd.stfc.ac.uk/SCD/support/40694.aspx
* http://www.scd.stfc.ac.uk/SCD/resources/PDF/USRMAN.pdf

To download the DL_MESO_DPD code you need to register at https://ccpforge.cse.rl.ac.uk/gf/. Please contact Dr. Micheal Seaton at Daresbury Laboratory (STFC) for further details.



Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment


The DL_MESO code is developed using git version control. Currently the GPU version is under a branch named "add_gpu_version". After downloading the code, checkout to the GPU branch and look into the "DPD/gpu_version" folder, i.e:

* git clone DL_MESO_repository_path
* cd dl_meso
* git checkout gpu_version
* cd /DPD/gpu_version
* make all

To compile and run the code you need to have installed the CUDA-toolkit and have a CUDA enabled GPU device (see http://docs.nvidia.com/cuda/#axzz4ZPtFifjw).

The current version has been tested ONLY for the Mixture_Large test case available in the DEMO/DPD folder. 
To run the case, compile the code using the "make all" command from the "bin" directory, copy the "FIELD" and "CONTROL" files in this directory and run "./dpd_gpu.exe".
The DL_MESO code is developed using git version control. Currently the GPU version is under a branch named "add_gpu_version". 
After downloading the code, checkout to the GPU branch and look into the "DPD/gpu_version" folder, i.e:




Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

This module has been merged into DL_MESO code. It is composed of the
following commits (you need to be register as developer):

* https://ccpforge.cse.rl.ac.uk/gf/project/dl_meso/scmgit/?action=ScmCommitDetail&scm_commit_id=110906
* https://ccpforge.cse.rl.ac.uk/gf/project/dl_meso/scmgit/?action=ScmCommitDetail&scm_commit_id=111357



.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html

