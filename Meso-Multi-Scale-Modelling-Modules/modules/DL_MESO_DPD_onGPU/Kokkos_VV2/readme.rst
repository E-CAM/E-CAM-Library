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
    Fortran/C++

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

.. _dl_meso_kokkos_VV2:

###############################################
DL_MESO (DPD) on Kokkos: Verlet Velocity step 2
###############################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

This module is related to the implementation in DL_MESO (DPD)
`Kokkos library <https://github.com/kokkos/kokkos>`_ to achieve performance portability. 
It's focus is on the second loop of the Verlet Velocity (VV) scheme for the time marching scheme.

Purpose of Module
_________________
In this module we are porting to DL_MESO (DPD) the second loop of the Verlet Velocity scheme to
`Kokkos <https://github.com/kokkos/kokkos>`_. 
This allows to run DL_MESO on NVidia GPUs as well 
as on other GPUs or architectures (many-core hardware like KNL), allowing performance portability 
as well as separation of concern 
between computational science and HPC.

The VV scheme is made of 3 steps: 1) a first velocity and particle positions integration 
by $\Delta t/2$, 2) a force calculation 
and 3) a second velocity integration by $\Delta t/2$. We have already offloaded the first loop 
in another module (:ref:`dl_meso_kokkos_VV1`). The main difference here is that we are using
a ``parallel_reduce`` loop
rather than ``parallel_for`` as we need to calculate also the stress tensor via reduction operations. 

Note: Kokkos is a C++ library, while DL_MESO (DPD) is in Fortran90 Language. The current 
implementation requires a transfer 
between Fortran and C++, due to the use of Fortran pointers not binded via the ISO_C_BINDING 
standard. This constraint will be removed 
in future versions.


Background Information
______________________
With the advent of heterogeneous hardware, achieving performance portability across 
different architectures is one of the main 
challenges in HPC. In fact, while specific languages, like CUDA, can give best 
performance for the NVidia hardware, they cannot 
be used with different GPU vendors limiting the usage across supercomputers worldwide.

In this module we use Kokkos, developed at Sandia National Laboratories, which consist 
of several C++ templated libraries which provide the capability 
to offload a workload to several different architectures, taking care of the memory layout and 
transfer between host and device.

To install Kokkos follow the instructions at:
`Kokkos Tutorial <https://github.com/kokkos/kokkos/blob/master/BUILD.md>`_. 
This module has been built on a Kokkos installation using the following flags:

.. code-block:: bash

  cmake ../ -CMAKE_CXX_COMPILER=$HOME/Kokkos/kokkos/bin/nvcc_wrapper -DKokkos_ENABLE_CUDA=ON \
    -DKokkos_ENABLE_OPENMP=ON -DKokkos_ENABLE_CUDA_LAMBDA=ON -DCMAKE_INSTALL_PREFIX=$HOME/Kokkos/kokkos

which allows us to translate the Kokkos kernel to CUDA language and run on NVidia GPUs. 

This module is part of the DL_MESO (DPD) code. Full support and documentation is available at:

* https://www.scd.stfc.ac.uk/Pages/DL_MESO.aspx
* https://www.scd.stfc.ac.uk/Pages/USRMAN.pdf

To download the DL_MESO_DPD code you need to register at https://gitlab.stfc.ac.uk. Please contact Dr. Michael
Seaton at Daresbury Laboratory (STFC) for further details.




Building and Testing
____________________
.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

To compile and run the code you need to have installed a Fortran and C++ compiler (GCC>7.x), CMake (>3.11.4) and Kokkos.

The DL_MESO code is developed using git version control. Currently, the Kokkos GPU version is under a branch
named ``Kokkos_version``. After downloading the code, checkout the Kokkos branch and move to the ``DPD`` folder.
Use cmake to build and compile the executable:

.. code-block:: bash

  git clone https://gitlab.stfc.ac.uk/dl_meso.git
  cd dl_meso
  git checkout kokkos_version
  cd ./DPD
  mkdir build
  cd build
  cmake ../
  cmake --build .


Use the files ``FIELD`` and ``CONTROL`` files in DEMO/DPD folders to test your code on different architectures.
Compare the ``OUTPUT`` and the ``export`` files to verify your results.

Performance
___________

We timed the execution for the VV second step kernel using Kokkos and compared to the same loop written 
in CUDA language (see
`DL_MESO GPU version modules <https://e-cam.readthedocs.io/en/latest/Meso-Multi-Scale-Modelling-Modules/index.html>`_) 
using a Volta V100 NVidia card.
For a 5.12 million particles of the Large Mixture test case, we get a 0.00117s (very close to the fist loop, 
despite the 
reduction operations) per kernel execution with both versions, 
which indicate no loss of performance in using Kokkos compared to native CUDA code. However, the data 
transfer between 
host and device currently occurs at every time step in the Kokkos version, taking 0.4689s 
and then with a negative 
impact on the overall performance.
For a fair comparison, this data should be transferred upstream to the time marching loop as 
done in the CUDA version. 



Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

This module has been pushed into DL_MESO git repository. It is composed of the
following commits (you need to be registered as collaborator):

* https://gitlab.stfc.ac.uk/dl_meso/dl_meso/-/commit/4d32671264648b4252f71c2f98d0164ab0843f46
* https://gitlab.stfc.ac.uk/dl_meso/dl_meso/-/commit/457509dc8727d30b49f4bb70a4bec98126866447
