
:orphan:

..  sidebar:: Software Technical Information

  Name
    GPAW CUDA version: build instructions

  Language
    Python, C, CUDA

  Licence
    `GPL <https://opensource.org/licenses/gpl-license>`_

  Documentation Tool
    Code comments. ReST and Sphinx for GPAW documentation.

  Application Documentation
    https://wiki.fysik.dtu.dk/gpaw/

  Software Module Developed by
    Martti Louhivuori (based on work by Samuli Hakala et al.)

.. _gpawcuda:

#####################################
GPAW CUDA version: build instructions
#####################################

..  contents:: :local:

GPAW is a density-functional theory (DFT) program for ab initio electronic
structure calculations using the projector augmented wave method. An
experimental CUDA version is under development that supports running GPAW
on GPUs. This module provides build instructions for the CUDA version and
links to the development effort.


Purpose of Module
_________________

An experimental CUDA version of GPAW is under development with support for
NVIDIA GPGPUS using CUDA, CuBLAS, and PyCUDA. This module provides build
instructions for the CUDA version and links to the development effort.


Background Information
______________________

GPAW is a density-functional theory (DFT) program for ab initio electronic
structure calculations using the projector augmented wave method. It uses a
uniform real-space grid representation of the electronic wavefunctions that
allows for excellent computational scalability and systematic converge
properties.

GPAW is written mostly in Python, but includes also computational kernels
written in C as well as leveraging external libraries such as NumPy, BLAS and
ScaLAPACK. Parallelisation is based on message-passing using MPI. 

To add support for GPUs, an experimental CUDA version of GPAW that uses CUDA,
CuBLAS, and PyCUDA was developed (Samuli Hakala *et al.*, PARA 2012,
https://doi.org/10.1007/978-3-642-36803-5_4). This early effort
was stalled for a while, but has now been continued in order to bring GPU
support to modern GPAW versions. Current version is based on GPAW version
1.5.2, but active development is on-going.


Building and Testing
____________________

In addition to the normal software requirements of GPAW, the GPU version
requires the `CUDA Toolkit <https://developer.nvidia.com/cuda-toolkit>`_ and
the `PyCUDA <https://pypi.org/project/pycuda/>`_ python module.

The only additional step to installing GPAW is that in the CUDA version one
needs to build the CUDA kernels before building the rest of the GPAW. This is
done in the ``c/cuda/`` directory that contains the CUDA kernels. There is a
customisable Makefile (``make.inc``) that can be edited before running the
``make`` command.

So, the steps to install the CUDA version of GPAW are:

1. Edit the Makefile for the CUDA kernels (``c/cuda/make.inc``).

   Modify the default options and paths to match your system. The essential
   parts are the include paths for libraries (MPI, CUDA) and the build options
   for ``nvcc`` to target the correct GPU architecture.

2. Build the CUDA kernels::

     cd c/cuda
     make
     cd -

3. Edit the GPAW setup script (``customize.py``).

   Add correct link and compile options for CUDA (and possibly other
   libraries). The relevant lines for CUDA are e.g.:

   .. code-block:: python

      define_macros += [('GPAW_CUDA', '1')]
      libraries += ['gpaw-cuda', 'cublas', 'cudart', 'stdc++']
      library_dirs += [
              './c/cuda',
              '/path/to/cuda/lib64'
      ]
      include_dirs += [
              '/path/to/cuda/include'
      ]

4. Build and install GPAW::

     python setup.py install --prefix=$TARGET_DIRECTORY


Source Code
___________

An up-to-date development version of GPAW with CUDA support is currently
available at: https://gitlab.com/mlouhivu/gpaw. The current version is based
on GPAW 1.5.2 and is available as
`commit 111567ee <https://gitlab.com/mlouhivu/gpaw/-/tree/111567ee39dd48e106b36b1aab4e6bc1b9961cae>`_.

Once it is merged with the upstream, the CUDA version will be available as a
separate branch called 'cuda' in the main GPAW repository. Status of this work
is tracked in
`Merge Request !580 <https://gitlab.com/gpaw/gpaw/-/merge_requests/580>`_.

To obtain the latest development version of the code, use the following
command::

  git clone -b cuda https://gitlab.com/mlouhivu/gpaw.git

or to get the version based on version 1.5.2, use the following commands::

  git clone -b cuda https://gitlab.com/mlouhivu/gpaw.git
  git checkout 111567ee39dd48e106b36b1aab4e6bc1b9961cae

