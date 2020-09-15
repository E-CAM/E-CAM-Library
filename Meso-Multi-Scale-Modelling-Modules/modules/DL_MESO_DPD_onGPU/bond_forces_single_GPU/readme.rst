..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

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

.. _dl_meso_dpd_gpu_bond_forces_single_GPU:

#######################################
Bond forces on DL_MESO_DPD (single GPU)
#######################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."


This module add the bond forces to the single GPU version of DL_MESO (DPD). These take in account on the itneractiions between
different chemical species
which allow to create complex molecules more representative of real systems. An example of an application is the
ternary solution where a primary component is bonds interacting with the other two phases.


Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The algorithm used is the same of the DL_MESO serial version, but of course adapted for SIMT (Single Instruction Multiple Threads) architecture.
The module includes also the angle and dihedral forces, all divived according a classical ortogonal domain decomposition.

Considering that in a real case the number of bounds is usually much lower than the total number of particles, different CUDA streams
for the three kernels (``k_findBondForce``, ``k_findAngleForce`` and ``k_findDihedralForce``) are used. This allow to launch them in parallel
to improve the performance of the overall simulation.





.. note::

  If the module is an ingredient for a more general workflow (e.g. the module was the necessary foundation for later
  code; the module is part of a group of modules that will be used to calculate certain property or have certain
  application, etc.) mention this, and point to the place where you specify the applications of the more general
  workflow (that could be in another module, in another section of this repository, an application’s website, etc.).

.. note::

  If you are a post-doc who works in E-CAM, an obvious application for the module (or for the group of modules that
  this one is part of) is your pilot project. In this case, you could point to the pilot project page on the main
  website (and you must ensure that this module is linked there).



Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module is part of the DL_MESO_DPD code. Full support and documentation is available at:

* https://www.scd.stfc.ac.uk/Pages/DL_MESO.aspx
* https://www.scd.stfc.ac.uk/Pages/USRMAN.pdf 

To download the DL_MESO_DPD code you need to register at https://gitlab.stfc.ac.uk/dl_meso/dl_meso. 
Please contact Dr. Micheal Seaton at Daresbury Laboratory (STFC) for further details.


Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The DL_MESO code is developed using git version control. Currently there are a single GPU version and a multi GPU version is under a two different branches. After downloading the code, checkout to the ``single_GPU_version`` branch and look into the ``DPD/gpu_version`` folder, i.e::

  git clone DL_MESO_repository_path
  cd dl_meso
  git checkout single_GPU_version
  cd /DPD/gpu_version
  make all

To compile and run the code you need to have installed the CUDA-toolkit and have a CUDA enabled GPU device (see http://docs.nvidia.com/cuda/#axzz4ZPtFifjw).

To run the case, compile the code using the ``make all`` command from the ``bin`` directory, copy
the ``FIELD`` and ``CONTROL`` files from the ``gpu_version/test/Solvent`` folder in this directory and run ``./dpd_gpu.exe``.



Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

This module has been merged into DL_MESO code. It is composed of the
following commits (you need to be register as developer):

* https://gitlab.stfc.ac.uk/dl_meso/dl_meso/commit/c787c4bc4f56634c2c6c6730b98b75093907ce57
* https://gitlab.stfc.ac.uk/dl_meso/dl_meso/commit/8b3dd9c071a60cbfb6e5fc285e82049efcc603f7
* https://gitlab.stfc.ac.uk/dl_meso/dl_meso/commit/9c6452d2340c53dc68d1eabbee37992d14e071b5

The DL_MESO code is developed using git version control. Currently the GPU version is under a branch named "add_gpu_version". 

.. Here are the URL references used (which is alternative method to the one described above)

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html

