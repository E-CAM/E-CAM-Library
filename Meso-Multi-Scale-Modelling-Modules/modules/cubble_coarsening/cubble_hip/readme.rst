..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    Cubble_HIP


  Language
   C++/CUDA and HIP

  Licence
    This software will be released under the GPL licence.

  Documentation Tool
    Sphinx
     
  Application Documentation
    https://version.aalto.fi/gitlab/lankinj5/cuda_bubble

  Relevant Training Material
    https://version.aalto.fi/gitlab/lankinj5/cuda_bubble/wikis/home


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _cubble_hip:

##########################
Cubble: Hip implementation
##########################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

The mechanics of foams exhibit dynamical behavior familiar from other jammed materials, such as granular matter. These are so called yield stress
fluids i.e. their flow requires external stress that exceeds a paricular value, called the yield stress. On the other hand, foams differ from other
materials by their internal structure development, coarsening, where while the gas concentration of the foam remains constant, the gas diffuses
between the bubbles. The larger bubbles grow at the expense of the smaller ones. 

This module provides a recepie and makefile for converting the c++/CUDA implementation of a foam coarsening simulator (https://journals.aps.org/pre/abstract/10.1103/PhysRevE.98.012607) to instead use HIP to allow it to be run on both AMD and Nvidia GPUs.



Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment


The goal was set to be able to run simulations involving up to one million
bubbles reaching a scaling state in systems
with non-periodic boundary conditions in three dimensions, an undoable task
for even the most efficient single core CPU implementation.
The Cubble code demonstrates the power of efficiently used GPU code, and
provides a model implementation strategy for
mesoscale DEM simulators. 



Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The converted code runs on single MI50 and Radeon VII GPUs.



Building and Testing
____________________


Converting the Cubble code to use HIP instead of Cuda enables it to run
on AMD GPUs, in addition to Nvidia GPUs. The conversion process is easy.

First the source code needs to be converted from Cuda to HIP, this is done
by converting everything in the src directory using the ``hipify-perl``
script. All ``.cu`` files are changed to cpp files and ``.cuh`` changed
to ``.h``, we then need to update any include statements to reflect this.

When converting ``Util.h`` the converter will emit the following warnings

::

    warning: old/Util.h:#25 :   cubble::cudaCallAndLog((call), #call, __FILE__, __LINE__)
    warning: old/Util.h:#27 :   cubble::cudaCallAndThrow((call), #call, __FILE__, __LINE__)
    warning: old/Util.h:#125 : inline bool cudaCallAndLog(hipError_t result, const char *callStr,
    warning: old/Util.h:#137 : inline void cudaCallAndThrow(hipError_t result, const char *callStr,
  
This is due to the cubble program using the same naming scheme as cuda
for some functions, i.e the name is cudaSomething and the converter is
warning that it was uable to convert them, however in this case these
are not actual cuda calls and it should not covert these, so it is safe
to ignore the warnings.

The cubble code also uses the NVTX library for better profiling, this
library does not work on AMD hardware and while comparable functionality
exists this will not be automatically converted. The Cubble program
will work without NVTX so in this case we just remove the inclusion
of ``nvToolsExt.h`` and not enable profiling when compiling the
Cubble program.

On some systems we may need to add ``-DENABLE_HIP_PROFILE=0`` to the
compilation flags to suppress errors about some profiling headers not
being found.

After this we should have a version of the code that can be compiled
with the included ``makefile``. Unfortunately this version will not run
with the current version of HIP, tested with version 3.1. The program
will fail with not finding symbols in ``hipGetSymbolAddress`` calls or
complaining it does not have device functions for certain calls. The
cubble code places the majority of its code in the cubble namespace,
unfortunately currently the HIP compiler struggles with device symbols
and functions being in a namespace. The easiest solution in this case
is to move all the code out of the cubble namespace, this will give
you a code that will run on AMD hardware. It is likely this will
improve in the future and this step will no longer be needed.



Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_

The source code used as a base for the conversion is freely available
for download in `Cubble sources <https://github.com/KJLankinen/cubble>`.
In addition the modified ``makefile`` is included in this repository:
:download:`makefile <./makefile>`
