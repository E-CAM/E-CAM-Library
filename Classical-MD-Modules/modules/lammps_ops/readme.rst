.. _ops_lammps:

########################################
Integrating LAMMPS with OpenPathSampling
########################################

.. sidebar:: Software Technical Information

  The information in this section describes OpenPathSampling as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Language
    Python (2.7)

  Documentation Tool
    Sphinx, numpydoc format (ReST)

  Application Documentation
    http://openpathsampling.org

  Relevant Training Material
    http://openpathsampling.org/latest/examples/

  Licence
    LGPL, v. 2.1 or later

.. contents:: :local:

Authors: Jony Castagna

This module shows how LAMMPS can be used as Molecular Dynamic (MD) engine in OpenPathSampling (OPS)
and it also provide a benchmark for the impact of OPS overhead over the MD engine.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

OpenPathSampling uses OpenMM as default engine for calculating the sampled trajectories.
Other engines as GROMACS and LAMMPS can be used (despite not yet available in the official release) 
allowing to exploit different computer architectures like hybrid CPU-GPU and to simulate more complex problems.

In general OPS gathers a frame (i.e. a state of the physical system at a point in time, typically consists of coordinates, velocities, and periodic cell vectors) after a defined number of time steps. 
The MD engine has to produce the sequence of frames and wait for OPS to provide new input values. This generate of course an overheads which has a negative impact on the overall performance of the simulation. 

In this module we present the source code for the integration of OPS with LAMMPS as well as a benchmark for of a simple test case to show the impact on the performance due to OPS overhead.      

The integration with LAMMPS has been developed by Jan Hendrick and consists of a Python script where the number of time steps per frame has to be specified (see below for the link to the source file).  


.. references would be nice here...

Background Information
______________________

This module builds on OpenPathSampling, a Python package for path sampling
simulations. To learn more about OpenPathSampling, you might be interested in
reading:

* OPS documentation: http://openpathsampling.org
* OPS source code: http://github.com/openpathsampling/openpathsampling

Details about how to use OPS with LAMMPS are provided in the:

* iPython notebook for LAMMPS: https://github.com/jhprinz/openpathsampling/blob/1235c472217d32b26011cdd6db0ac6287c994ab2/examples/misc/introduction_lammps.ipynb 

Testing
_______

.. IF YOUR MODULE IS IN OPS CORE:

The script which integrate LAMMPS with OPS can be applied to any case running in LAMMPS. For example, the Lennard-Jones test (32K atoms) case presented in the ECAM deliverable D7.2 has been used to benchmark the OPS overhead when using LAMMPS as presented in next section. 

.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY

.. The tests for this module can be run by downloading its source code, 
.. installing its requirements, and running the command ``nosetests`` from the
.. root directory of the repository.

Examples
________

The table shows the performance of LAMMPS with OPS for the Lennar-Jones (32K atomes) test case using 100 time steps per frame (more frequent queries) and 1000 time steps (less frequent queries) for a total of 100K time steps. The MD engine time (i.e., LAMMPS only time) and the total time (OPS + LAMMPS) using different number of nodes (with 24 cores per node) is presented.

Results have been obtained using the JURECA (http://www.fz-juelich.de/ias/jsc/EN/Expertise/Supercomputers/JURECA/JURECA_node.html) supercomputer.

==================== ===== ================== ===============
Steps between frames  Nodes Md Engine Time [s] Total time [s]
==================== ===== ================== ===============
100                  1     106.25             108.48 
100                  2     61.40              62.31
100                  4     36.46              37.28
==================== ===== ================== ===============
1000                 1     100.93             102.48
1000                 2     53.56              54.45
1000                 4     29.71              30.67
==================== ===== ================== ===============
Using 100 or 1000 time steps per frame, the overhead due to the OPS is within a maximum of 3%. However, one should note that when increasing the number of time steps per frame to 1000, the time spent in the MD engine decreases due to less overhead from stopping and starting the engine. A suggested improvement to OPS has been to allow the engine to continue the trajectory while the frame is being evaluated by OPS, which should help eliminate this overhead. The OPS overhead remains relatively static and there is little discernible difference between the overheads for the two measurements. Given that OPS is effectively a serialisation point for the calculation, more intensive trajectories should also, therefore, lead to improved scalability results since they will reduce this ratio of serial to parallel workload. 


Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

The source code for integrating LAMMPS with OPS can be found at: 

* https://github.com/openpathsampling/openpathsampling/pull/697 

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

