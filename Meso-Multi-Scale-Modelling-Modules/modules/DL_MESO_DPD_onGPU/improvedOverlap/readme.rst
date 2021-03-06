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

.. _DL_MESO_DPD_onGPU_improvedOverlap:

############################################################################
Improved overlap computation communiction in DL_MESO_DPD (multi-GPU version) 
############################################################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

The following module present an improved overlap between communication and computation for the DL_MESO_DPD package on multi-GPUs.

A binary mixture phase separation test case up to 1.8 billion particles has been used for weak and strong benchmarks. The results
show good scaling in both cases up to 1024 GPUs. 
After that the scaling without improved overlap quickly tails off while the other shows good efficiency (>85%)
up to 4096 GPUs.


Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The previous multi-GPU version of DL_MESO_DPD was not correctly setting the order of the CUDA streams dedicated to computation and
communication. This was preventing their overlap and drastically reduce the overall performance and scalability. The current module
fixes this problem and present weak and strong scaling on the Piz Daint Supercomputer (see https://user.cscs.ch/) using up to 4096 GPUs.
The previous performance is presented for comparison.


Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment


This module is part of the DL_MESO_DPD code. Full support and documentation is available at:

* http://www.scd.stfc.ac.uk/SCD/support/40694.aspx
* http://www.scd.stfc.ac.uk/SCD/resources/PDF/USRMAN.pdf

To download the DL_MESO_DPD code you need to register at https://gitlab.stfc.ac.uk/. Please contact Dr. Micheal Seaton at Daresbury Laboratory (STFC) for further details.



Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment


The DL_MESO code is developed using git version control. Currently the GPU version is under a branch named "multi_GPU_version". After downloading the code, checkout to the GPU branch and look into the "DPD/gpu_version" folder, i.e:

* git clone DL_MESO_repository_path
* cd dl_meso
* git checkout multi_GPU_version
* cd /DPD/gpu_version/bin
* make all

To compile and run the code you need to have installed the CUDA-toolkit, a CUDA enabled GPU device (see http://docs.nvidia.com/cuda/#axzz4ZPtFifjw), a fortran compiler (like GCC gfortran, Intel Fortran, Cray ftn) and MPI library. Moreover, the code uses CUDA_aware_MPI which is part of GPU Direct Technologies. Please make sure your cluster support CUDA_aware_MPI!

The current version has been tested ONLY for the Mixture_Large test case available in the DEMO/DPD folder. 
To run the case, compile the code using the "make all" command from the "bin" directory, copy the "FIELD" and "CONTROL" files in this directory and run "mpirun -np N ./dpd_gpu.exe".
For a the strong scaling test we used 1.8 billion particles keeping the density ratio particles/volume=5. Below is a plot of the strong scaling with and without improved overlap.  


.. image:: ./StrongScaling.png
   :width: 30 %
   :align: center

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

This module has been merged into DL_MESO code. It is composed of the
following commits (you need to be registered as developer):

* https://gitlab.stfc.ac.uk/srb73435/dl_meso/commit/90701a3ad97d53dc0555d0b79862e0db3134f83c



.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html

