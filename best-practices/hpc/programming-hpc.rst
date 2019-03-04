.. _programming_paradigms:

Programming for HPC
-------------------

C++17
~~~~~
C++17 is the name for the most recent revision of the `ISO/IEC 14882 <https://en.wikipedia.org/wiki/ISO/IEC_14882>`_
standard for the C++ programming language.

The previous C++ versions show very limited parallel processing capabilities when using multi/many core architectures.
This situation will change with the C++17, in which the parallelised version of `Standard Template Library
<https://en.wikipedia.org/wiki/Standard_Template_Library>`_ is
included. The STL is a software library for C++ programming which has 4 components: Algorithms, Containers, Functors
and Iterators. `"Parallel STL advances the evolution of C++, adding vectorization and parallelization capabilities
without resorting to nonstandard or proprietary extensions, and leading to code modernization and the development of
new applications on modern architectures." <https://insidehpc.com/2017/05/parallel-stl/>`_

A `multi-threading programming model for C++ <https://en.wikipedia.org/wiki/C%2B%2B11#Multithreading_memory_model>`_ is
supported since C++11.

Fortran 2015
~~~~~~~~~~~~

`Fortran 2015 <http://fortranwiki.org/fortran/show/Fortran+2015>`_ is a minor revision of Fortran 2008 (which was when
Fortran became a Partitioned Global Address Space (PGAS) language with the introduction of `coarrays
<https://en.wikipedia.org/wiki/Coarray_Fortran>`_). The revisions mostly target additional parallelisation features and
increased interoperability with C.

Most Fortran-based software E-CAM sees in practice is implemented in Fortran 95 and there appears to be little awareness
of the parallel features of the latest Fortran standards. E-CAM is considering organising a workshop that addresses
this lack of awareness (similar to the "`Software Engineering and Parallel Programming in Modern Fortran
<https://www.cranfield.ac.uk/courses/short/aerospace/software-engineering-and-parellel-programming-in-modern-fortan>`_"
held at the Cranfield University).

It should be noted that `compiler support for the latest Fortran standards is limited
<http://fortranwiki.org/fortran/show/Compiler+Support+for+Modern+Fortran>`_. This is most likely due to the fact
that Fortran is not widely used outside of the scientific research (limiting its commercial scope).

The (potential) role of Python
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given that it is an interpreted language (i.e., it is only compiled at runtime), Python is not usually discussed much
in the HPC space since there is limited scope for control over many factors that influence performance. Where we
are observing a lot of growth is where applications are being written in languages like C++ under the hood but are
intended to be primarily used via their Python interfaces.

This is a valuable, and user friendly, development model that allows users to leverage Python for fast prototyping while
maintaining the potential for high performance application codes.

A warning to would be users: `Python 2 will stop being developed in 2020 <https://pythonclock.org/>`_ so please make
sure that your code is Python3 compliant.

Open Standards
~~~~~~~~~~~~~~

We describe here some of the open standards that are most likely to be leveraged on next generation HPCresources.

**MPI**

Now more than 25 years old, Message Passing Interface (MPI) is still with us and remains the de facto standard for
internode communication (though it is not the only option, alternatives such as `GASNet <https://gasnet.lbl.gov/>`_
exist). `MPI-3.1 <http://mpi-forum.org/docs/mpi-3.1/mpi31-report.pdf>`_ was approved
by the MPI Forum on June 4, 2015. It was mainly an errata release for MPI 3.0 which included some important enhancements
to MPI:

* Nonblocking collectives
* Sparse and scalable irregular collectives
* Enhancements to one-sided communication (very important for extreme scalability)
* Shared memory extensions (on clusters of SMP nodes)
* Fortran interface

Maintaining awareness of the `scope of past and future updates to the MPI standard
<https://www.lrz.de/services/compute/courses/x_lecturenotes/Parallel_Programming_Languages_Workshop/MPI.pdf>`_ is
important since it is the latest features that target the latest architectural developments.

**OpenMP**

OpenMP is also 20 years old and remains the most portable option for on-node workloads. The standard has introduced
new features to deal with increasing node-level heterogeneity (device offloading, such as for the GPU, in
particular) and varied workloads (task level parallelism).

From GCC 6.1, OpenMP 4.5 is fully supported for C and C++ (with Fortran support coming in the GCC 7 series). The
`level of OpenMP support among other compilers <http://www.openmp.org/resources/openmp-compilers/>`_ varies
significantly.

**OpenACC**

`OpenACC <https://www.openacc.org/>`_ (for open accelerators) is a programming standard for parallel computing
developed by Cray, CAPS, Nvidia
and PGI. The standard is designed to simplify parallel programming of heterogeneous CPU/GPU systems. Since the
paradigm is very similar to the latest OpenMP specs, a future merger into OpenMP is not unlikely.
It should be noted that CUDA (with the `nvcc compiler
<http://docs.nvidia.com/cuda/cuda-compiler-driver-nvcc/index.html>`_) is still the most commonly used (and highest performing)
library for programming NVIDIA GPUs.

**OpenCL**

Open Computing Language (OpenCL) is a framework for writing programs that execute across heterogeneous platforms
consisting of central processing units (CPUs), graphics processing units (GPUs), digital signal processors (DSPs),
field-programmable gate arrays (FPGAs, see Section 2.4 for the extreme relevance of this) and other processors or
hardware accelerators.

OpenCL 2.2 brings the OpenCL C++ kernel language into the core specification for significantly enhanced parallel
programming productivity. When releasing OpenCL version 2.2, the Khronos Group announced that OpenCL would
be merging into `Vulkan <https://en.wikipedia.org/wiki/Vulkan_(API)>`_ (which targets high-performance realtime 
3D graphics applications) in the future, leaving some uncertainty as to how this may affect the HPC space.

Runtime System Approaches
~~~~~~~~~~~~~~~~~~~~~~~~~

As noted already, programming paradigm standards are moving forward to adapt to the technologies that we see in the
market place. The complexity of the hardware infrastructure necessarily brings complexity to the implementation of the
programming standards.

There are number of programming models that leverage runtime systems under development. They promise to abstract
away hardware during the development process, with the proviso that tuning at runtime may be required. Our
experience to date with these systems is limited so we simply provide a list of three such systems here (which is certainly
not exhaustive) in no particular order:

* `HPX <https://github.com/STEllAR-GROUP/hpx>`_, a C++ Standard Library for concurrency and parallelism. The goal of the HPX project is to create a high
  quality, freely available, open source implementation of `ParalleX <http://stellar.cct.lsu.edu/pubs/icpp09.pdf>`_ concepts for conventional and future systems
  by building a modular and standards conforming runtime system for SMP and distributed application environments.
  (Most recent release: v1.0, April 2017)
* `Kokkos <https://github.com/kokkos/kokkos>`_ implements a programming model in C++ for writing performance portable applications targeting all
  major HPC platforms. For that purpose it provides abstractions for both parallel execution of code and data
  management. Kokkos is designed to target complex node architectures with N-level memory hierarchies and
  multiple types of execution resources. It currently can use OpenMP, Pthreads and CUDA as backend programming
  models. (Most recent release: v2.04.04, 11 Sept 2017)
* `OmpSs <https://pm.bsc.es/ompss>`_ is an effort to integrate features from the StarSs programming model developed at Barcelona Supercomputing
  Centre (BSC) into a single programming model. In particular, the objective is to extend OpenMP with
  new directives to support asynchronous parallelism and heterogeneity (devices like GPUs). However, it can
  also be understood as new directives extending other accelerator based APIs like CUDA or OpenCL. The OmpSs
  environment is built on top of BSCs Mercurium compiler and Nanos++ runtime system. (Most recent release:
  v17.06, June 2017)

Feedback for software developers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Awareness of the latest standards and the status of their implementations are critical at all times during application
development. The adoption of new features from standards are likely to have large impact on the scalability of application
codes precisely because it is very likely that these features exist to target the scalability challenges on modern
systems. Nevertheless, you should be aware that there can be very long gaps between the publication of a standard
and the implementation in compilers (which is frequently also biased by who is pushing which standard and why: Intel
pushes OpenMP because of their Xeon Phi line, NVIDIA who now own PGI pushes OpenACC because of their GPUs,
AMD pushed OpenCL for their own GPUs to compete with CUDA). The likelihood of there being a single common (set
of ) standards that performs well on all architectures is not high in the immediate future. For typical developers that
we see in E-CAM, MPI+OpenMP remains the safest bet and is likely to perform well, as long as the latest standards are
used.

More disruptive software technologies (such as GASNet) are more likely to gain traction if they are used by popular
abstraction layers (which could be PGAS langauages, runtime systems or even domain specific languages) "under
the hood". This would make the transition to new paradigms an implementation issue for the abstraction layer. Indeed,
given the expected complexity of next generation machines, new programming paradigms that help remove the
performance workload from the shoulders of scientific software developers will gain increasing importance.

As you may have noticed in the previous discussion, the computer scientists developing these abstractions are working
mostly in C++, and the implementation of new standards in compilers is also seen first for C++. From a practical
perspective this has some clear implications: if you want to access the latest software technologies then you had better
consider C++ for your application. This may appear harsh given that the Fortran standard has clear capabilities in this
space, but it is a current reality that cannot be ignored. Also, given that the vast majority of researchers will eventually
transition to industry (because there simply aren’t enough permanent jobs in academia) it is more responsible to
ensure they have programming expertise in a language that is heavily used in the commercial space. Finally, the ecosystem
surrounding C++ (IDEs, testing frameworks, libraries,. . . ) is much richer because of it’s use in industry and
computer science.

Taking all of the above into consideration, if you are starting out with an application we would distil the discussion into
the following advice: prototype your application using Python leveraging the Python APIs to the libraries you need;
write unit tests as you go; and, when you start doing computationally intensive work, use C++ with Python interfaces
to allow you to squeeze out maximal performance using the latest software technologies.

