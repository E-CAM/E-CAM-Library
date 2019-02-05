.. _knl:

Intel Many-core
---------------

The 2nd Generation Intel Xeon Phi platform, known as Knights Landing (KNL), has been released on the market in
Q2 of 2016. The chip, based on a 14nm lithography, contains up to 72 cores @1.5GHz with a maximum memory
bandwidth of 115.2 GB/s. One of the main features is the increased AVX512 ISE (Instruction Set Extensions) which
includes SSE (Streaming SIMD Extensions) and AVX (Advanced Vector Extensions). More details are available at `Intel
Knights Landing <https://ark.intel.com/products/codename/48999/Knights-Landing>`_. The same component (Intel Xeon Phi
7250-F) is available in the JURECA Booster as part of the `DEEP
<http://www.deep-project.eu/deep-project/EN/Home/home_node.html>`_ and DEEP-ER projects.

Knights Hill is the codename for the third-generation MIC architecture and it will be manufactured in a 10 nm process.
Intel announced the first details at SC14, however since then no further details have been released and the Aurora
project from the DoE delayed (see `Some Surprises in the 2018 DoE Budget for Supercomputing
<https://www.nextplatform.com/2017/05/23/surprises-2018-doe-budget-supercomputing/>`_).

`Knights Mill <https://www.servethehome.com/intel-knights-mill-for-machine-learning>`_ is Intel’s codename for a Xeon
Phi product specialised in deep learning. It is expected to support reduced variable precision which have been used to
accelerate machine learning in other products, such as half-precision floating-point variables in Nvidia’s Tesla.

Feedback for software developers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Based on the latest hardware developments specified above (and the AVX512 instruction set used by this hardware),
we strongly advise the software developer to take in consideration the importance of enhancing performance through
vectorization both from numerical algorithm point of view and at the compiler level. Intel provides very good tools to
achieve this through compiler flags (which allow you to have a full report about the vectorization efficiency) or more
sophisticated software like `Intel Advisor <https://software.intel.com/en-us/intel-advisor-2017-user-guide-linux>`_.

At node-level the recommended parallelism is by shared memory. In this case `OpenMP <http://www.openmp.org/>`_ is the
de facto standard and Intel provides good tools like `VTune
<https://software.intel.com/en-us/intel-vtune-amplifier-xe>`_.

Many training courses and documents are available on line (see `Intel Advisor training
<https://software.intel.com/en-us/intel-advisor-xe-support/training>`_ and `VTune training
<https://software.intel.com/en-us/intel-vtune-amplifier-xe-support/training>`_).
