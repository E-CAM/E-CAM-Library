..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    DL_MESO 

  Language
    Fortran/CUDA-C

  Licence
    BSD, v. 2.7 or later

  Documentation Tool
    Fortran/C comments

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

.. _dl_meso_gpu_longInteger:

######################################
Long Integer on DL_MESO_DPD multi-GPU
######################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

This module extends the capability of the DL_MESO code to run simulations with more than 1.8 billion particles using long integer arrays.

Purpose of Module
_________________

The current version of DL\_MESO\_DPD on multi GPU cannot run systems larger than 1.8 billion particles due to the ``INTEGER`` type used in Fortran for the particle arrays.

This module addresses this problem replacing, only where needed, the ``INTEGER`` with ``LONG INTEGER`` type in the Fortran arrays. This allows us to run simulations with more than 1.8 billion particles and, as a result, more complex systems.

Background Information
______________________

This module is part of the DL\_MESO\_DPD code. Full support and documentation is available at:

* https://www.scd.stfc.ac.uk/Pages/DL_MESO.aspx
* https://www.scd.stfc.ac.uk/Pages/USRMAN.pdf

To download the DL\_MESO\_DPD code you need to register at https://gitlab.stfc.ac.uk. Please contact Dr. Micheal Seaton at Daresbury Laboratory (STFC) for further details.




Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The DL\_MESO code is developed using git version control. Currently, the multi GPU version is under a branch named ``multi_GPU_version``. After downloading the code, checkout the GPU branch and look into the ``DPD/gpu_version`` folder, i.e:

.. code-block:: bash

  git clone https://gitlab.stfc.ac.uk/dl_meso.git
  cd dl_meso
  git checkout multi_GPU_version
  cd ./DPD/gpu_version/bin
  make all

To compile and run the code you need to have installed the CUDA-toolkit (>=8.0) and have a CUDA enabled GPU device (see http://docs.nvidia.com/cuda/#axzz4ZPtFifjw). For testing, the MPI library the OpenMPI 3.1.0 has been used.

To run the separation test case, copy the ``FIELD`` and ``CONTROL`` files from the "../tests/LargeSeparation" directory and run using ``mpirun -np NP ./dpd_gpu.exe``. The test case consists in simulating a binary mixture of 24 billion particles on 4096 GPUs (tested on PizDaint CSCS supercomputer).



Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

This module has been merged into DL\_MESO code. It is composed of the
following commits (you need to be registered as collaborator):

* https://gitlab.stfc.ac.uk/dl_meso/dl_meso/commit/7f3e7abe7bb1c8010dd6a5baa0de4907ffe2f003

