..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    MaZe for OF-DFT (HPC version).

  Language
    ``C``.

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_.

  Documentation Tool
    `Doxygen <https://www.doxygen.nl/>`_.

  Application Documentation
    `GitLab <https://gitlab.e-cam2020.eu/acoretti/shake-dft>`_.

  Relevant Training Material
    Not currently available.

  Software Module Developed by
    Alessandro Coretti, Marco Ferrarotti, Sergio Decherchi, Rodolphe Vuilleumier, Sara Bonella


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _maze_ofdft_hpc:

########################################################################################
Mass-Zero Constrained Dynamics for Orbital Free Density Functional Theory (HPC Version).
########################################################################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The program performs Orbital-Free Density Functional Theory Molecular Dynamics (OF-DFT-MD) using the Mass-Zero (MaZe)
constrained molecular dynamics approach as discussed in [BONELLA2020b]_.
The method is based on an extended Lagrangian and the dynamics enforces, at each timestep, the Born-Oppenheimer
condition that the system relaxes instantaneously to the ground state through the formalism of holonomic constraints
of zero mass.
The adiabatic separation between the degrees of freedom is enforced rigorously, while the numerical
algorithm is exactly symplectic and time-reversible in both physical and additional set of degrees of freedom.
Mathematical details about the implementation of the methods are discussed at length in Alessandro Coretti's
Ph.D. thesis.
The computation of the electronic density is carried on in reciprocal space through a plane-waves expansion so that
the mass-zero degrees of freedom are represented by the Fourier coefficients of the electronic density field.
The evolution of the ions is performed using Velocity-Verlet algorithm, while the SHAKE algorithm is used for
evolution of the additional degrees of freedom.

The code is intended for condensed matter physicists and for material scientists and it can be used for various purposes
related to the subject.
Even though some analysis tool is included in the package, the main goal of the software is to produce particles
trajectories to be analyzed in post-production by means of external software.

In computing trajectories, MaZe is intended to stand in the middle between force-field based MD and Kohn-Sham MD in
terms of efficiency and accuracy.
Indeed, while the forces are computed on-the-fly at each timestep, the optimization is done on
the electronic density field instead of the Kohn-Sham orbitals. This feature avoids the need for satisfying the
orthonormality constraint among orbitals and allows the
computational complexity of the code to scale linearly with the dimensionality of the system.
On the other hand, no information on the
orbitals is available. The accuracy of the simulation relies on the choice of the kinetic energy functional, which has
to be provided in terms of the electronic density alone.

Performance Evaluation
______________________

This version of the module has been optimized by the IIT in Genova through the following steps:

* Improved FFTW usage:

   - single plan creation (reuse of same FFTW plan for all FFT/iFFT);
   - use FFTW patient planning;
   - memory aligned allocation of FFT/iFFT vectors to exploit FFTW simd implementation;

* Async FFT/iFFT execution via pthread threadpool (`C-Thread-Pool <https://github.com/Pithikos/C-Thread-Pool>`_);

* ComputeForcesFromStructureFactor / ComputeStructureFactor loops parallelization through OpenMP;

The proposed optimizations on the FFT/iFFT routines allow a reduction of the execution time on all the GGA test cases
by roughly 50%.

The parallelized ``for`` loops, tested on a compute node with 24 cores, allow to reduce by roughly 60% the execution
time on the LDA test cases (with the exception of the ones using b-splines that needs further work).

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The module is standalone and only relies on the libraries discussed in the next section.

Installation
____________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The execution of the code depends on the following libraries:

* `FFTW <http://www.fftw.org>`_: a library for computing the discrete Fourier transform;
* `Libxc <https://www.tddft.org/programs/Libxc/>`_: a library of exchange-correlation functionals for
  density-functional theory;
* `BLAS <https://www.netlib.org/blas/>`_: a library that provides standard building blocks for
  performing basic vector and matrix operations;
* `Argp <https://www.gnu.org/software/libc/manual/html_node/Argp.html>`_: an interface for parsing
  unix-style argument vectors;

On macOS, `Homebrew <https://brew.sh>`_ is strongly recommended to install compiler and dependencies.

The installation is based on a Makefile.
A few machine dependent variable must be defined in the file './config.mk' prior to invoking the ``make`` utility.
Examples can be found in the './configuration_files/' folder.
The structure of the './config.mk' file is as follows:

.. code-block:: bash

   #Compiler Configuration
   #Compiler command
   CC     =
   #Compiler options
   CFLAGS =
   #Includefiles linkers
   IFLAGS =
   #Libraries linkers
   LIBS   =

   #Test configuration
   #Python command
   TEST     =

The command ``make`` will then build the executables.
The command ``make clean`` cleans the files resulting from the compilation.
Detailed documentation can be build using `Doxygen`_ through the command ``make documentation``.
The whole suite of regression tests can be run through the command ``make tests``.

Testing
_______

Tests for the code and for regressions are launched through a python script which can be found in './tests/'.
Move into this folder and run ``python regression_tests.py -s MaZe``.
The scripts can take other options in order to launch different suites of tests.
Default is 'all' which can take up to 20 minutes.
Run ``python regression_tests.py --help`` for more information on regression tests script.

By default the script tests an MD simulation of solid Sodium using different parameters:

* **Pseudopotential**: 'Gaussian (Gauss)' pseudopotential and 'Topp and Hopfield (Topp)' pseudopotential;
* **Jacob's ladder rung**: 'LDA' for Local Density Approximation and 'GGA' for Generalized Gradient Approximation.
  The approximation refers only to the kinetic functional which is 'Thomas-Fermi (TF)' for LDA and 'Thomas-Fermi
  plus von Weiszaecker correction (TFvW)' and 'Perrot' functional for GGA;
* **Kinetic functional**: As above 'Thomas-Fermi (TF)', 'Thomas-Fermi plus von Weiszaecker correction (TFvW)'
  and 'Perrot' functionals;
* **Explicit enforcing of additional constraint**: When the suffix '_additional_constraint' appears in the name of the
  text, the conservation of the number of electrons is explicitly enforced as discussed in [BONELLA2020b]_.

All the simulation in the tests are run using a Slater exchange functional and no correlation functional.

The subfolders inside './tests' can also be conveniently used as examples and references for the format
of the input file 'runtime.inpt' and of the configuration file 'configuration.inpt'.

Source Code
___________

The source code is available from the `E-CAM Gitlab <https://gitlab.e-cam2020.eu/>`_ under the
`MaZe <https://gitlab.e-cam2020.eu/acoretti/shake-dft/>`_
project. The HPC version of the code can be found on the branch **HPC**.

The repository contains the following directories:

* **./source/:** contains the source code. The subfolder './source/headers/' contains the modules'
  headers, while the subfolder './source/obj/' is used for compilation file outputs;
* **./tests/:** contains regression tests;
* **./scripts/:** contains useful python scripts to run simulations over different sets of parameters;
* **./documentation/:** contains the documentation generated with Doxygen together with the wiki of the project;
* **./configuration_files/:** contains examples of configuration files to generate the executable on different machines;

References
__________

.. [BONELLA2020b] Phys. Chem. Chem. Phys., 2020, 22, 10775-10785
