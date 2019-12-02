.. sidebar:: Software Technical Information

  Name
    ESL Demonstrator

  Language
    Fortran 2003

  Licence
    `Mozilla Public License v2.0`_

  Documentation Tool
    Doxygen_

  Application Documentation
    *Not currently available*

  Relevant Training Material
    *Not currently available*


####################
The ESL Demonstrator
####################

.. contents:: :local:

The ESL Demonstrator is a basic atomic-scale simulation software illustrating
how to use and bring together the various available components of the
`Electronic Structure Library <https://esl.cecam.org/>`_ (ESL). It is meant to
be used as a concrete implementation example for both end-users and
developers. For users, it evidences and explains the typical operations and
building blocks of an electronic structure code. For developers, it shows how
to bring together the different ESL components in a consistent way. Although
it is not expected to produce production-grade results, the ESL Demonstrator
can be helpful for beginners who want to discover the field of
electronic-structure calculations.


Purpose of Module
_________________

Since 2014, researchers, engineers and developers from all over the world have
regularly gathered to design, coordinate and develop software libraries and
tools of common interest for the electronic-structure community. In 2017, the
available modules reached a sufficient level of usability and completeness to
be used widely within the whole community. However, documenting every single
module properly so that developers of electronic-structure software can
integrate them seamlessly into their own codes would have been a daunting
task. The challenge was two-fold:

- How do we provide usable and comprehensive documentation and keep it
  accurate, while all the ESL projects are evolving asynchronously, each at
  its own pace?
- How do we make the process efficient enough, so that a small number of
  volunteers can continue focus mostly on their own projects, while the rest
  of the community benefits from relevant information and guidelines on how to
  use these projects?

The ESL Demonstrator, aka esl-demo_, addresses this issue by providing a
concrete and evolving example of a minimalistic electronic-structure program
entirely based on ESL components. It constitutes a global "executable
documentation" for the ESL. It is itself documented in a standard way, using
Doxygen, to provide relevant explanations about how to use each ESL component
in the appropriate context. In this case, such an approach is much more
suitable than traditional documentation, mainly because instead of having to
document between 10 and 20 components separately, the ESL developers only have
to take care of one meta-component, therefore:

- it requires less effort from less people;
- it can be put into action by anyone with a working build environment;
- it provides feedback to developers across the whole ESL about the possible
  side effects their changes may produce;
- ESL components are built and used together, which provides a proof that
  they are indeed compatible and interoperable;
- API changes are automatically detected, even if they have not been
  communicated or published;
- defects and incompatibilities are easily made obvious and can be discussed
  around a concrete occurrence of the problems and side effects they may
  cause.


Background Information
______________________

The `esl-demo`_ program is able to perform simple ground-state calculations
using plane-wave (PW) or atom-centered (AC) basis sets, as well as
norm-conserving pseudopotentials.

Its architecture is made of 3 logical blocks, spanning 3 levels of execution,
as illustrated in the following table:

  +------------------+---------------------+------------------------+
  | Plane Waves (PW) | Atom-Centered (AC)  | Basis-Independent (BI) |
  +==================+=====================+========================+
  | Self-Consistent Field                                           |
  +------------------+---------------------+------------------------+
  | Eigensolvers     | Eigensolvers        | Smearing               |
  +------------------+---------------------+ Exchange-Correlation   +
  | HΨ               | Hamiltonian Builder | Poisson Solver         |
  |                  |                     | Mixing                 |
  +------------------+---------------------+ Ion-Ion Interaction    +
  | Plane-Wave Basis | Atom-Centered Basis |                        |
  |                  |                     |                        |
  +------------------+---------------------+------------------------+
  | I/O: FDF, ESCDF - Pseudos: Pspio, PSML - FFT Wrappers           |
  +------------------+---------------------+------------------------+

Column-wise, one block takes care of plane-wave-related data and processes,
another one focuses on atom-centered aspects, and the remaining one handles
everything independent from the basis sets. At the lowest level, the program
interacts with the computer hardware, operating system and system libraries
available, as well as imports/exports data related to the current calculation.
In the middle layer, itself divided into 3 sub-levels, it implements the
quantum-mechanical equations in the framework of Density-Functional Theory
(DFT). At the top level, it drives the operations of the lower layers and
applies completion criteria. All cells of the table but the Self-Consistent
Field correspond to the use of one or more ESL components.

`esl-demo`_ is available from the `E-CAM Gitlab Repository`_ and mirrored on
GitHub_. It can be downloaded with Git. Please note that only the E-CAM
version is guaranteed to be up-to-date.


Building and Testing
____________________

.. warning::
   Should make a link to the ESL Bundle once the module is available.

The recommended way to get started with the `esl-demo`_ module is first to
download it from the E-CAM repository with `Git <https://git-scm.org/>`_::

    git clone https://gitlab.e-cam2020.eu/esl/esl-demo.git
    cd esl-demo

Before continuing, please read the README.rst file of `esl-demo`_ carefully and
make sure you have installed all the prerequisites on your computer.

The `esl-demo`_ module uses `Cmake <https://cmake.org/>`_ as its build system.
Here is a typical sequence to follow to build the code::

    mkdir my_build
    cd my_build
    cmake ..
    make -j8

To run `esl-demo`_, you will need at least a pseudopotential and a FDF input
file. Some examples are provided in the `tests/` subdirectory of the source
tree.

.. note::

   The information contained in the *Installation* and *Testing* sections are
   likely to work with the latest version of the source code from the
   repository. If this is not the case, you can go back to the commit where
   this information is guaranteed to work after the download is complete::

       git checkout de3dac2


Source Code
___________

`esl-demo`_ is an original ESL product created from scratch. Its source code is
available from the `E-CAM Gitlab Repository`_ under the esl-demo_ project.


.. _Doxygen: https://www.doxygen.org/
.. _`E-CAM Gitlab Repository`: https://gitlab.e-cam2020.eu/
.. _esl-demo: https://gitlab.e-cam2020.eu/esl/esl-demo
.. _GitHub: https://github.com/ElectronicStructureLibrary/esl-demo
.. _`Mozilla Public License v2.0`: https://www.mozilla.org/en-US/MPL/2.0/
