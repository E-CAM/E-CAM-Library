.. _SinglePath:

#######################################################################
Trotter Based Quantum Classical Surface Hopping Propagator - Single Path
#######################################################################

..  sidebar:: Software Technical Information

  Language
    C++ (GNU 2011 or higher)

  Licence
    MIT licence (MIT)

  Documentation Tool
    Doxygen
    
  Application Documentation
    `Documentation <https://gitlab.e-cam2020.eu/Quantum-Dynamics/Surface-Hopping/blob/master/Doc/html/index.html>`_

  Relevant Training Material
    Not currently available   

  Software Module Developed by
    Sean Kelly, Athina Lange, Philip McGrath, Shrinath Kumar and Donal MacKernan

..  contents:: :local:

Abstract
________
The present module is a highly refactored version of a code based on a highly cited algorithm published by 
D. Mackernan, G.Ciccotti and R. Kapral [Mackernan]_.  
The module software has been entirely refactored in modern C++ (GNU 2011 or higher) so as to: (a) run with high efficiency on massively parallel platforms 
under openmp or mpi; and (b) be at the core of additional software modules  aimed at addressing important issues such as improving the speed of convergence of 
estimates using correlated sampling, and much more realistic treatment of the classical bath, and connecting to other problems such as constant pH simulation 
through an effective Hamiltonian.

Purpose of Module
_________________
Quantum rate processes in condensed phase systems are
often computed by combining quantum and classical descriptions of
the dynamics including non-adiabatic coupling, using propagators which
amount to quantum path integrals in a partial Wigner phase space representation, such as
the mixed quantum-classical Dyson equation and variants thereof, or the Trotter decomposition of the quantum-classical propagator.  


Background Information
_____________________
An understanding of the dynamical properties of condensed phase
quantum systems underlie the description of a variety of quantum
phenomena in chemical and biological systems. These phenomena
include, among others, nonadiabatic chemical rate processes
involving electronic, vibrational or other degrees of freedom,
decoherence in open quantum systems and quantum transport
processes. Quantum effects underlie the study of ultra-fast rate
processes in solution. The development of schemes for the efficient and
accurate simulation of the quantum dynamics of such systems is an
an active area of research in chemical
physics, and is essential if problems of chemical interest involving
complex molecular species in the condensed phase are considered.

In investigations of the dynamical properties of quantum
statistical mechanical systems, one is often interested in the
average value of some operator when the system evolves from a
given initially prepared distribution described by the density
matrix :math:`\hat{\rho}(0)`. In such cases the quantum mechanical
average value of an operator :math:`\hat{B}` is given by
:math:`\overline{B(t)}= Tr \hat{B} \hat{\rho}(t)=  Tr\hat{B}(t) \hat{\rho}(0)`. Here,
:math:`\hat{B}(t)` evolves in time through the Heisenberg equation of motion.
In many applications, it is useful to partition the system into a subsystem and
a bath. A phase space description of the bath can be obtained by
taking a partial Wigner transform over the bath coordinate :math:`\{Q\}` representation
of the full quantum system. In this partial Wigner representation the expectation value of math:`\hat{B}(t)` takes the

.. math::
   \overline{B(t)}=  Tr' \int dR dP\;  {B}_W(R,P,t) {\rho}_W(R,P)

where the prime on the trace indicates a trace over the subsystem
degrees of freedom. 

The software module developed here is based on a  Trotter-based scheme for simulating
quantum-classical Liouville dynamics in terms of an ensemble of surface-hopping trajectories. The method can be used to compute the dynamics for longer times with fewer trajectories than the
sequential short-time propagation (SSTP) algorithm, which is also based on surface-hopping trajectories. The full derivation of the algorithm is given in the J.Chem Paper cited above. Here the software focus is to refactor the original code which until now was a purely serial so that it can be used efficiently on massively parallel machines. For mathematical details, we refer the reader to eq.30-35 of the paper.

Applications
____________
*** Description of Applications here ***


Algorithms and Software Implementation
______________________________________
The current Single Path code has three main advantages over the original version. First it is separated into files based on function for better readability.
For example the 'transition_matrix.cpp' file is where the transition matrix and associated functions are defined, etc. Secondly input parameters are read from
an Input file, so the code no longer needs to be recompiled to adjust these parameters. And finally the code has been altered to run in parallel which allows for 
a significant reduction in runtime.


Compiling
_________

OpenMP version:

With the GNU compiler:

::

	Compile command;
	g++ -o run main.cpp bath_setup.cpp density.cpp propagation.cpp transition_matrix.cpp opt_parser.cpp -lgsl -lgslcblas -lm -fopenmp -std=c++11

	Run command:
	export OMP_NUM_THREADS=[number of OpenMP threads]; ./run Input


With the Intel compiler:

::

	Compile command;
	icpc -o run main.cpp bath_setup.cpp density.cpp propagation.cpp transition_matrix.cpp opt_parser.cpp -lgsl -lgslcblas -lm -qopenmp -std=c++11

	Run command:
	export OMP_NUM_THREADS=[number of OpenMP threads]; ./run Input

-----------------------------------

MPI version:

::

	Compile command;
	mpic++ -o run main.cpp bath_setup.cpp density.cpp propagation.cpp transition_matrix.cpp opt_parser.cpp -lgsl -lgslcblas -lm -std=c++11

	Run command:
	mpirun -n [number of MPI processors] ./run Input



Checking for accuracy
__________________________________________
The original serial code was run 1000 times to generate an expected output and variance. These can be found in the ./Regression_testing sub-directory. 
A regression test is built into both the OpenMP and MPI versions which checks if their output is within five standard deviations
of the expected output (given a specific set of input parameters). If any part of the output goes outside that limit the regression test will fail. 
(Note: To run a test 'Regression_test=1' must be set in the Input file along with a standard set of parameters. All of this is specified in the Input file).


Testing, Performance and Scaling
_______________________
Testing was performed on the Fionn supercomputer from ICHEC. Fionn consistes of a large amount of 'nodes' each of which contains 24 processing cores. The OpenMP 
version was tested on up to 24 cores (1 node) and demonstrated perfect scaling with the number of cores. The MPI version was tested on up to 96 cores (4 nodes).
It again demonstrated perfect scaling up to 24 cores and good scaling up to 96 (reducing in efficiency as the number of nodes increased). 

These tests were performed by simply comparing the runtimes between codes using 1, 4, 8, ... 24 cores.

Source Code
___________

The source codes for the OpenMP and MPI versions of the code are: 
`OpenMP <https://gitlab.e-cam2020.eu/Quantum-Dynamics/Surface-Hopping/tree/master/Code_Parallel_Omp>`_, 
`MPI <https://gitlab.e-cam2020.eu/Quantum-Dynamics/Surface-Hopping/tree/master/Code_Parallel_MPI>`_.


Source Code Documentation
_________________________

The source code documentation is given at https://gitlab.e-cam2020.eu/Quantum-Dynamics/Surface-Hopping/tree/master/Doc.
These documentation files can be updated by executing the ``make`` command in the ``Doc`` directory.


References
__________

.. [Mackernan] D.Mackernan, G.Ciccotti, R.Kapral, `Trotter-Based Simulation of Quantum-Classical Dynamics`_, *J. Phys. Chem. B*, **2008**, 112 (2), pp 424-432.

.. _Trotter-Based Simulation of Quantum-Classical Dynamics: http://dx.doi.org/10.1021/jp0761416