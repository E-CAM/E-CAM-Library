..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

:orphan:

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    ElVibRot Time-dependent MPI

  Language
    Fortran 90

  Licence
    GNU Lesser General Public License (http://www.gnu.org/licenses/)

  Documentation Tool
    Doxygen

  Application Documentation
    See documents for `ElVibRot <https://github.com/lauvergn/ElVibRot-TnumTana/tree/master/doc/>`_ and `Tnum <http://pagesperso.lcp.u-psud.fr/lauvergnat/ElVibRot/Tnum-manual-v24.4-09_09_2013.pdf>`_

  Relevant Training Material
    Not currently available

  Software Module Developed by
    David Lauvergnat, Ahai Chen


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _ElVibRot Time-dependent MPI:

###############
ElVibRot_TD_MPI
###############

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

..  The E-CAM library is purely a set of documentation that describes software development efforts related to the project. A
..  *module* for E-CAM is the documentation of the single development of effort associated to the project.In that sense, a
..  module does not directly contain source code but instead contains links to source code, typically stored elsewhere. Each
..  module references the source code changes to which it directly applies (usually via a URL), and provides detailed
..  information on the relevant *application* for the changes as well as how to build and test the associated software.

..  The original source of this page (:download:`readme.rst`) contains lots of additional comments to help you create your
..  documentation *module* so please use this as a starting point. We use Sphinx_ (which in turn uses ReST_) to create this
..  documentation. You are free to add any level of complexity you wish (within the bounds of what Sphinx_ and ReST_ can
..  do). More general instructions for making your contribution can be found in ":ref:`contributing`".

.. Remember that for a module to be accepted into the E-CAM repository, your source code changes in the target application
..  must pass a number of acceptance criteria:

..  * Style *(use meaningful variable names, no global variables,...)*

..  * Source code documentation *(each function should be documented with each argument explained)*

..  * Tests *(everything you add should have either unit or regression tests)*

..  * Performance *(If what you introduce has a significant computational load you should make some performance optimisation
..  effort using an appropriate tool. You should be able to verify that your changes have not introduced unexpected
..  performance penalties, are threadsafe if needed,...)*


Purpose of Module
_________________

..  Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

..  Give a brief overview of why the module is/was being created, explaining a little of the scientific background and how
..  it fits into the larger picture of what you want to achieve. The overview should be comprehensible to a scientist
..  non-expert in the domain area of the software module.

..  This section should also include the following (where appropriate):

..  * Who will use the module? in what area(s) and in what context?

..  * What kind of problems can be solved by the code?

..  * Are there any real-world applications for it?

..  * Has the module been interfaced with other packages?

..  * Was it used in a thesis, a scientific collaboration, or was it cited in a publication?

..  * If there are published results obtained using this code, describe them briefly in terms readable for non-expert users.
..  If you have few pictures/graphs illustrating the power or utility of the module, please include them with
..  corresponding explanatory captions.


..  If the module is an ingredient for a more general workflow (e.g. the module was the necessary foundation for later
  code; the module is part of a group of modules that will be used to calculate certain property or have certain
  application, etc.) mention this, and point to the place where you specify the applications of the more general
  workflow (that could be in another module, in another section of this repository, an application’s website, etc.).


..  If you are a post-doc who works in E-CAM, an obvious application for the module (or for the group of modules that
  this one is part of) is your pilot project. In this case, you could point to the pilot project page on the main
  website (and you must ensure that this module is linked there).

..  If needed you can include latex mathematics like
.. :math:`\frac{ \sum_{t=0}^{N}f(t,k) }{N}`
..  which won't show up on GitLab/GitHub but will in final online documentation.

..  If you want to add a citation, such as [CIT2009]_, please check the source code to see how this is done. Note that
..  citations may get rearranged, e.g., to the bottom of the "page".

The ElVibRot time-dependent MPI (ElVibRot-TD-MPI) module is a parallelized time-dependent quantum simulation program. It is a part of the `ElVibRot <https://github.com/lauvergn/ElVibRot-TnumTana>`_ package designed for general quantum dynamics simulation using curvilinear coordinates. There is no built-in limitation on the degrees of freedom for the target system. The code employed a numerical but exact kinetic energy operator with Tnum [Tnum]_. The Smolyak algorithm [Smo]_ is applied to avoid the direct-product basis sets and grids in the simulation. 


Background Information
______________________

..  Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

..  If the modifications are to an existing code base (which is typical) then this would be the place to name that 
..  application. List any relevant urls and explain how to get access to that code. There needs to be enough information
..  here so that the person reading knows where to get the source code for the application, what version this information is
..  relevant for, whether this requires any additional patches/plugins, etc.

..  Overall, this module is supposed to be self-contained, but linking to specific URLs with more detailed information is
..  encouraged. In other words, the reader should not need to do a websearch to understand the context of this module, all
..  the links they need should be already in this module.

The quantum dynamics simulation has provided powerful insight into the underlying mechanism of chemical reactions, laser molecular interaction, etc. The simulation, with the conventional expansion on the direct-product basis, is limited by the exponential growth of computational cost with the increase of degrees of freedom. The multi-configuration time-dependent Hartree, a well-known package developed with the aim of generality, expands the wavefunction as a sum of Hartree products with single-particle functions, leading to a very efficient wavepackage propagation. The quantum diffusion Monte Carlo and the Feynman path integral approaches get around the problem by avoiding expending the wavefunction on a basis set. The variational multi-configuration Gaussian applies on-the-fly quantum chemical calculation of the potential energy to approach the quantum effects in the photochemistry. However, the Smolyak method provides another way to deal with high-dimensional problems, without losing accuracy and universality. The application of Smolyak algorithm enables the simulation of large systems (> 12 degrees of freedom) as the wavefunction is expanded as a weighted sum of small  Smolyak wavefunction contributions. MPI is implemented depends on this framework. The module is designed to works on different levels of clusters. Three MPI schemes are provided in accord with a series of well-known propagation methods, including the Chebyshev, Runge-Kunta, short iterative Lanczos and Taylor expansion, etc. The three MPI schemes correspond to the simulation with the mode of most efficiency, memory saving, and massive cluster parallelization, respectively. The default setting will automatically choose the scheme according to the balance of resource consumed and the parallelization efficiency.  


Applications of the Module
__________________________

This module is intended to provide a parallel program for general wavepackage propagation. The general capability of the simulation could be up to tens of degrees of freedom. The propagation time could be up to hundreds of picosecond with general computation time, according to the selected propagation method. The code has been applied for the simulation of Pyrazine (:math:`C_4H_4N_2`), which is of 24 degrees of freedom. This module could be a practical tool for general quantum molecular simulation, supporting the further study of molecular dynamics in chemical reactions, ultrafast process, etc.


Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

.. Provide the build information for the module here and explain how tests are run. This needs to be adequately detailed,
.. explaining if necessary any deviations from the normal build procedure of the application (and links to information
..  about the normal build process needs to be provided).

The code is compatible with gfortran, mpifort, ifort, pgf90, etc. Building the program requires OpenMPI v2.0 or above. OpenMPI should be built as 64-bit for the simulation of very large system. 

* build with MPI

set makefile: ::

  F90=mpifort
  MPICORE=gfortran ! gfortran or ifort according to the compiler for MPI


other main options:

::
 
  F90=gfortran    ! compile with gfortran
  F90=ifort       ! compile with ifort
  F90=pgf90       ! compile with pgf90
  parallel_make=1 ! enable parallel make with -j argument
  OMP=1           ! enable openMP
  OPT=1           ! enable code optimization
  INT=4           ! 4 or 8 for 32-bits or 64-bits integer
  LAPACK=1        ! enable LAPACK
  ARPACK=1        ! enable ARPACK
  QML=1           ! enable QMLib


To build:

.. code-block:: c
  :linenos:

  make

To test:

.. code-block:: c
  :linenos:

  make test 

To clean test files

.. code-block:: c
  :linenos:

  make cleantest

Three MPI schemes will be tested for 12 and 24 degrees of freedom systems. In directory 

::
  
  ./Working_tests/MPI_tests

check folders 12D_propagation_* and 24D_propagation_* for examples. For more details, see `ElVibRot <https://github.com/lauvergn/ElVibRot-TnumTana>`_.

Source Code
___________

See the `MPI branch <https://github.com/lauvergn/ElVibRot-TnumTana/tree/MPI_working>`_ of ElVibRot  



References
==========

.. [Tnum] D. Lauvergnat, A. Nauts, *Phys. Chem. Chem. Phys.* **12** (2010) 8405-8412 `DOI: 10.1039/C001944E <http://dx.doi.org/10.1039/C001944E>`_
.. [Smo]  S. A. Smolyak, *Dokl. Akad. Nauk SSSR* **148** (1963) 1042–1045 `<http://mi.mathnet.ru/eng/dan27586>`_


