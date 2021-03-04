..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

:orphan:

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

.. sidebar:: Software Technical Information

  The information in this section describes *n2p2* and *LAMMPS* as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Name
    n2p2, LAMMPS

  Language
    C++

  Licence
    `GPL-3.0-or-later <https://www.gnu.org/licenses/gpl.txt>`__ (n2p2),
    `GPL-2.0 <https://www.gnu.org/licenses/old-licenses/gpl-2.0.txt>`__ (LAMMPS)

  Documentation Tool
    `Doxygen <http://www.doxygen.nl/>`__,
    `Sphinx <http://www.sphinx-doc.org>`__

  Application Documentation
    http://compphysvienna.github.io/n2p2/ (n2p2)
    https://lammps.sandia.gov/ (LAMMPS)

  Relevant Training Material
    http://compphysvienna.github.io/n2p2/ (n2p2)
    https://lammps.sandia.gov/ (LAMMPS)

  Software Module Developed by
    Andreas Singraber


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _n2p2_improved_link_hpc:

#######################################
n2p2 - Improved link to HPC MD software
#######################################

..  Let's add a local table of contents to help people navigate the page

.. contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

This module documents efforts to improve the interaction of *n2p2* with existing
HPC software, in particular the molecular dynamics (MD) software package `LAMMPS
<https://lammps.sandia.gov/>`__.

.. The E-CAM library is purely a set of documentation that describes software development efforts related to the
   project. A *module* for E-CAM is the documentation of the single development of effort associated to the project.In
   that sense, a module does not directly contain source code but instead contains links to source code, typically
   stored elsewhere. Each module references the source code changes to which it directly applies (usually via a URL),
   and provides detailed information on the relevant *application* for the changes as well as how to build and test the
   associated software.

.. The original source of this page (:download:`readme.rst`) contains lots of additional comments to help you create
   your documentation *module* so please use this as a starting point. We use Sphinx_ (which in turn uses ReST_) to
   create this documentation. You are free to add any level of complexity you wish (within the bounds of what Sphinx_
   and ReST_ can do). More general instructions for making your contribution can be found in ":ref:`contributing`".

.. Remember that for a module to be accepted into the E-CAM repository, your source code changes in the target
   application must pass a number of acceptance criteria: * Style *(use meaningful variable names, no global
   variables,...)*
   
   * Source code documentation *(each function should be documented with each argument explained)*
   
   * Tests *(everything you add should have either unit or regression tests)*
   
   * Performance *(If what you introduce has a significant computational load you should make some performance
     optimisation effort using an appropriate tool. You should be able to verify that your changes have not
     introduced unexpected performance penalties, are threadsafe if needed,...)*

Purpose of Module
_________________

Although *n2p2* was already shipped with source files for patching LAMMPS before,
the build process required manual intervention of users. To avoid this in
future versions of *LAMMPS* a pull request was created to include the
*n2p2*/*LAMMPS* interface by default as a `user package
<https://lammps.sandia.gov/doc/Packages_user.html>`__. In order to conform with
*LAMMPS* `contribution guidelines
<https://lammps.sandia.gov/doc/Modify_contribute.html>`__ multiple issues were
resolved, triggering these changes/additions to *LAMMPS* and *n2p2*:

*  Modify the traditional build process (via makefiles) to include *n2p2*
*  Modify the CMake build process to search and include *n2p2*
*  Create additional documentation about the build settings
*  Adapt documentation of the LAMMPS ``pair_style nnp`` command
*  Create a suitable example which can be shipped with LAMMPS
*  Change *n2p2* to conform with *LAMMPS* ``bigbig`` settings (see `here
   <https://lammps.sandia.gov/doc/Build_settings.html#size>`__)
*  Change the source files ``pair_nnp.(cpp/h)`` to conform with the *LAMMPS* coding
   style

Furthermore, the *n2p2* build system was adapted to allow for multiple
interfaces to other software packages, with an option to select only
those of interest to the user.  This can be achieved by providing the
``INTERFACES`` variable in the build stage, e.g. use

.. code-block:: bash

   make libnnpif INTERFACES="LAMMPS CabanaMD"

to build both the ``LAMMPS`` and the ``CabanaMD`` interface and include it in
the ``libnnpif`` library.

As a first application, the user contributed `CabanaMD
<https://github.com/ECP-copa/CabanaMD>`__ `interface
<https://github.com/CompPhysVienna/n2p2/pull/49>`__ was integrated in the new
build process. *CabanaMD* is an `ECP proxy application
<https://proxyapps.exascaleproject.org/>`__ which makes use of the `Kokkos
<https://github.com/kokkos/kokkos>`__ performance portability library and *n2p2*
to port neural network potentials in MD simulations to GPUs and other HPC
hardware.

.. Keep the helper text below around in your module by just adding "..  " in
   front of it, which turns it into a comment

.. Give a brief overview of why the module is/was being created, explaining a little of the scientific background and
   how it fits into the larger picture of what you want to achieve. The overview should be comprehensible to a scientist
   non-expert in the domain area of the software module.
   
   This section should also include the following (where appropriate):
   
   * Who will use the module? in what area(s) and in what context?
   
   * What kind of problems can be solved by the code?
   
   * Are there any real-world applications for it?
   
   * Has the module been interfaced with other packages?
   
   * Was it used in a thesis, a scientific collaboration, or was it cited in a publication?
   
   * If there are published results obtained using this code, describe them briefly in terms readable for non-expert
     users. If you have few pictures/graphs illustrating the power or utility of the module, please include them
     with corresponding explanatory captions.

.. .. note::
   
     If the module is an ingredient for a more general workflow (e.g. the module was the necessary foundation for later
     code; the module is part of a group of modules that will be used to calculate certain property or have certain
     application, etc.) mention this, and point to the place where you specify the applications of the more general
     workflow (that could be in another module, in another section of this repository, an application’s website, etc.).

.. .. note::
   
     If you are a post-doc who works in E-CAM, an obvious application for the module (or for the group of modules that
     this one is part of) is your pilot project. In this case, you could point to the pilot project page on the main
     website (and you must ensure that this module is linked there).

.. If needed you can include latex mathematics like
  :math:`\frac{ \sum_{t=0}^{N}f(t,k) }{N}`
  which won't show up on GitLab/GitHub but will in final online documentation.

.. If you want to add a citation, such as [CIT2009]_, please check the source code to see how this is done. Note that
   citations may get rearranged, e.g., to the bottom of the "page".

.. .. [CIT2009] This is a citation (as often used in journals).

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

.. If the modifications are to an existing code base (which is typical) then this would be the place to name that
   application. List any relevant urls and explain how to get access to that code. There needs to be enough information
   here so that the person reading knows where to get the source code for the application, what version this information
   is relevant for, whether this requires any additional patches/plugins, etc.

.. Overall, this module is supposed to be self-contained, but linking to specific URLs with more detailed information
   is encouraged. In other words, the reader should not need to do a websearch to understand the context of this module,
   all the links they need should be already in this module.

This module is based on *n2p2*, a C++ code for generation and application of
neural network potentials used in molecular dynamics simulations. The source
code and documentation are located here:

* *n2p2* documentation: http://compphysvienna.github.io/n2p2/
* *n2p2* source code: http://github.com/CompPhysVienna/n2p2

In addition the source files for the LAMMPS patch are based on *LAMMPS*, a C++
code for massively parallelized molecular dynamics simulations. The source code
and documentation are located here:

* *LAMMPS* documentation: https://lammps.sandia.gov/
* *LAMMPS* source code: http://github.com/lammps/lammps


Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

.. Provide the build information for the module here and explain how tests are run. This needs to be adequately
   detailed, explaining if necessary any deviations from the normal build procedure of the application (and links to
   information about the normal build process needs to be provided).

.. important::

   By the time of reading these instructions the packages were most likely
   developed further and it cannot be guaranteed that the procedures given below
   will give the desired results. To retrieve the state of each software at the
   time of writing these lines please uncomment and use the lines with ``git
   checkout <commit-hash>``.

LAMMPS user package
"""""""""""""""""""

To test whether the *LAMMPS* user package ``USER-NNP`` works together with
*n2p2* as expected we have to download *LAMMPS* from the ``pair-style-nnp``
feature branch and use the current ``master`` version of *n2p2*. First, to get
and compile *n2p2*:

.. code-block:: bash

   git clone https://github.com/CompPhysVienna/n2p2
   cd n2p2/src
   # git checkout 428db3ee61f9943feaeedfaaeb5e096289983d46
   make libnnpif -j
   cd ../..

Next we retrieve the *LAMMPS* feature branch:

.. code-block:: bash

   git clone -b pair-style-nnp --single-branch https://github.com/singraber/lammps
   cd lammps
   # git checkout ed53e2bbff2465dd05ba015a05843b2bb328360c

and compile the code with the ``USER-NNP`` package enabled using the CMake
build approach:

.. code-block:: bash

   mkdir build
   cd build
   cmake -D PKG_USER-NNP=yes -D N2P2_DIR=<path-to-n2p2> ../cmake
   make -j

Alternatively, we could also use the traditional build process using makefiles:

.. code-block:: bash

   cd src
   make yes-user-nnp
   make N2P2_DIR=<path-to-n2p2> mpi -j

In either case the *LAMMPS* binary should be created (``lammps/build/lmp`` or
``lammps/src/lmp_mpi``) and we can test if works correctly with the provided
example:

.. code-block:: bash

   cd ../examples/USER/nnp
   # Binary from CMake build process:
   mpirun -np 4 ../../../build/lmp -in in.nnp
   # or from the traditional build process:
   # mpirun -np 4 ../../../src/lmp_mpi -in in.nnp

CabanaMD interface
""""""""""""""""""

While the *n2p2* build process for the *CabanaMD* interface is trivial (it
requires only the collection of some header files) the compilation steps on the
CabanaMD side are not trivial. Furthermore, testing requires a suitable GPU with
a compatible compiler environment. Hence it is not feasible to provide general
build instructions for testing here. However, the *n2p2* documentation offers an
example build procedure for a specific hardware setup `here
<https://compphysvienna.github.io/n2p2/misc/cabanamd_build_example.html>`__.


Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

.. Here link the source code *that was created for the module*. If you are using Github or GitLab and the `Gitflow
   Workflow <https://www.atlassian.com/git/tutorials/comparing-workflows#gitflow-workflow>`_ you can point to your
   feature branch.  Linking to your pull/merge requests is even better. Otherwise you can link to the explicit commits.
   
   * `Link to a merge request containing my source code changes
     <https://github.com/easybuilders/easybuild-easyblocks/pull/1106>`_
   
   There may be a situation where you cannot do such linking. In this case, I'll go through an example that uses a patch
   file to highlight my source code changes, for that reason I would need to explain what code (including exact version
   information), the source code is for.
   
   You can create a similar patch file by (for example if you are using git for your version control) making your
   changes for the module in a feature branch and then doing something like the following:

.. Don't forget the white space around the "literal block" (a literal block keeps all spacing and is a good way to
   include terminal output, file contents, etc.)

.. ::

..   [adam@mbp2600 example (master)]$ git checkout -b tmpsquash
     Switched to a new branch "tmpsquash"

..   [adam@mbp2600 example (tmpsquash)]$ git merge --squash newlines
     Updating 4d2de39..b6768b2
     Fast forward
     Squash commit -- not updating HEAD
      test.txt |    2 ++
      1 files changed, 2 insertions(+), 0 deletions(-)

..   [adam@mbp2600 example (tmpsquash)]$ git commit -a -m "My squashed commits"
     [tmpsquash]: created 75b0a89: "My squashed commits"
      1 files changed, 2 insertions(+), 0 deletions(-)

..   [adam@mbp2600 example (tmpsquash)]$ git format-patch master
     0001-My-squashed-commits.patch


.. To include a patch file do something like the following (take a look at the source code of this document to see the
   syntax required to get this):

..  Below I am telling Sphinx that the included file is C code, if possible it will then do syntax highlighting. I can
    even emphasise partiuclar lines (here 2 and 9-11)

.. .. literalinclude:: ./simple.patch
      :language: c
      :emphasize-lines: 2,9-11
      :linenos:


..  I can't highlight the language syntax of a patch though so I have to exclude
    :language: c

.. .. literalinclude:: ./simple.patch
      :emphasize-lines: 2,9-11
      :linenos:

.. If the patch is very long you will probably want to add it as a subpage which can be done as follows

.. .. toctree::
      :glob:
      :maxdepth: 1
   
      patch

..  Remember to change the reference "patch" for something unique in your patch file subpage or you will have
    cross-referencing problems

.. you can reference it with :ref:`patch`

Changes in LAMMPS
"""""""""""""""""

The easiest way to view the source code changes in LAMMPS covered by this module
is to use the `GitHub pull request page
<https://github.com/lammps/lammps/pull/2626>`__. There, use the *Files
changed* `tab <https://github.com/lammps/lammps/pull/2626/files>`__ to
review all changes.

Changes in n2p2
"""""""""""""""

The following commits collect all changes required to follow the *LAMMPS*
contribution guidelines:

*  `Updated makefiles for new LAMMPS build process
   <https://github.com/CompPhysVienna/n2p2/commit/4b5c50030300f2060ba1cf214ca13c868c346d4b>`__
*  `Add flag information when n2p2 runs
   <https://github.com/CompPhysVienna/n2p2/commit/a3b3dadc75be445b80b9e3737f6176b14a98ad06>`__
*  `Changed build flags prefix from NNP_ to N2P2_
   <https://github.com/CompPhysVienna/n2p2/commit/d489a2491f6fdeb5dc39278418f21efc6341b289>`__
*  `Changed Atom::(Neighbor::)tag to int64_t
   <https://github.com/CompPhysVienna/n2p2/commit/428db3ee61f9943feaeedfaaeb5e096289983d46>`__

The commits which restructured the makefiles to allow multiple selectable
interface library parts can be found here (they are part of the `CabanaMD pull
request <https://github.com/CompPhysVienna/n2p2/pull/49>`__):

*  `Restructured interface library
   <https://github.com/CompPhysVienna/n2p2/pull/49/commits/e084cde64f4946c3885ab02e367ce9ad29343e37>`__
*  `Renamed source files and updated docs
   <https://github.com/CompPhysVienna/n2p2/pull/49/commits/887ba87cdbcf723aeeac80292c56d89307a6d123>`__

The *CabanaMD* example build instructions were added to the *n2p2* documentation
in this commit:

*  `Add CabanaMD build example docs for reference
   <https://github.com/CompPhysVienna/n2p2/commit/995f0b593615cb0270063c491226c9ee94ab5f2a>`__

.. Here are the URL references used (which is alternative method to the one described above)

.. .. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. .. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html
