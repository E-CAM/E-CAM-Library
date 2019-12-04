..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

.. :orphan:

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

.. _dl_meso_gpu_surface:

#####################################################
Surface boundary conditions on DL_MESO_DPD multi-GPUs
#####################################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."


This module implement the solid surfaces boundary conditions on the multi-GPU version of DL\_MESO\_DPD.

Purpose of Module
_________________

The single GPU version contains already the wall surface boundary conditions. The following module is an implementation on the multi GPU version.

Real cases often involve complex geometries and require the implementation of solid walls as boundary conditions. A typical example if the flow in microchannels used for example in the production of Graphene. The interaction between fluid and surface create a different an unique profile of
velocities which has a strong impact on the fluid dynamic, especially in case of non-Newtonian fluids (i.e. where the
shear stress is a non linear function of the velocity gradient) like shampoo and other body care products.

This module will allow to study such phenomena reducing the
computational cost and time and scaling up to larger systems.

Background Information
______________________

This module is part of the DL\_MESO\_DPD code. Full support and documentation is available at:

* https://www.scd.stfc.ac.uk/Pages/DL_MESO.aspx
* https://www.scd.stfc.ac.uk/Pages/USRMAN.pdf

To download the DL\_MESO\_DPD code you need to register at https://gitlab.stfc.ac.uk. Please contact Dr. Micheal Seaton at Daresbury Laboratory (STFC) for further details.




Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

To compile and run the code you need to have installed the CUDA-toolkit (>=8.0) and have a CUDA enabled GPU device (see http://docs.nvidia.com/cuda/#axzz4ZPtFifjw). For the MPI library the OpenMPI 3.1.0 has been used.

The DL\_MESO code is developed using git version control. Currently, the multi GPU version is under a branch named ``multi_GPU_version``. After downloading the code, checkout the GPU branch and move to the ``DPD/gpu_version/bin`` folder. Modify the Makefile to use the correct GPU architecture (sm_XX) and check if the CPP flags are supported (i.e.: -DAWARE_MPI for CUDA\_aware\_MPI support, -DOPENMPI for OpenMPI library, -DMVAPICH for MVAPICH library and -DHWLOC for ``hwloc`` support). Make sure nvcc is installed (or CUDA toolkit module loaded). Then, compile using ``make all``. In short: 

.. code-block:: bash

  git clone https://gitlab.stfc.ac.uk/dl_meso.git
  cd dl_meso
  git checkout multi_GPU_version
  cd ./DPD/gpu_version/bin
  # Modify the Makefile according to your device and libraries
  make all

To run the  test case, copy the ``FIELD`` and ``CONTROL`` files from the "../tests/Poiseuille" directory and run using ``mpirun -np 8 ./dpd_gpu.exe`` on a job partition with 1 GPU available per MPI process. The test case consists in simulating the Poiseuille flow, using 8 GPUs, obtained between two parallel plane surfaces. Being the flow laminar, the solution has to match with the analytic parabolic profile of the velocity field. Compare the ``OUTPUT`` and the ``export`` files to verify your results. Do not worry about the ``problem with total_nbeads`` warning message.



Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

This module has been merged into DL\_MESO code. It is composed of the
following commits (you need to be registered as collaborator):

* https://gitlab.stfc.ac.uk/dl_meso/dl_meso/commit/c10992eb39815029bbe485f18e05230cd098afab




