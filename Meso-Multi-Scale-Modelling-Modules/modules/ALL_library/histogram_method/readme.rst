..  sidebar:: Software Technical Information

  Name
    A Load Balancing Library (ALL)

  Language
    C++, Fortran interfaces available

  Licence
    `BSD 3-Clause <https://choosealicense.com/licenses/bsd-3-clause/>`_

  Documentation Tool
    No tool used in source code, repo documentation written in `Markdown <https://en.wikipedia.org/wiki/Markdown>`_

  Application Documentation
    See `ALL repository <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing>`_

  Relevant Training Material
    None available

  Software Module Developed by
    Rene Halver

.. _ALL_histogram:

#########################
ALL Tensor-Product method
#########################

..  contents:: :local:

A Load-Balancing Library (ALL) library aims to provide an easy and portable way to include dynamic domain-based load balancing
into particle based simulation codes. The library is developed in the Simulation Laboratory Molecular Systems of the
Juelich Supercomputing Centre at Forschungszentrum Juelich.

Purpose of Module
_________________

This module provides an additional method to the `ALL library <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing>`_ , up-to-date descriptions of the methods in the library can
be found in the `ALL README file <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/blob/master/README.md>`_.

The *histogram-based staggered-grid* scheme
results in the same grid as the staggered-grid scheme (see :ref:`ALL_staggered`), this scheme uses
the cumulative work function in each of the three cartesian directions in
order to generate this grid. Using histograms and the previously defined
distribution of process domains in a cartesian grid, this scheme generates
in three steps a staggered-grid result, in which the work is distributed as
evenly as the resolution of the underlying histogram allows. In contrast to
the other schemes this scheme depends on a global exchange of
work between processes.

Background Information
______________________

See :ref:`ALL_background` for details.

Building and Testing
____________________

ALL uses the `CMake <https://cmake.org/runningcmake/>`_ build system, specific build and installation requirements can
be found in the `ALL README file <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/blob/master/README.md>`_.

There are 3 tests of available for the Tensor-Product method in the `ALL GitLab repository examples <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/tree/master/example>`_ . Namely: (1) Simple Wye-shape biosystem; (2) Heterogeneous polymer melt and (3) A rotated version of the Wye-shaped biosystem. They can be run by executing the commands below:

.. code:: bash

  export ALL_INSTALLATION=/path/to/my/loadbalancing/install
  cmake .. -DCMAKE_INSTALL_PREFIX=$ALL_INSTALLATION -DCM_ALL_FORTRAN=ON
  make -j
  make install
  cd example/jube/
  ln -s $ALL_INSTALLATION/bin/ALL_test
  # JUBE must be available and configured in the environment
  jube run ALL_benchmark.xml --tag Polymer --type 4

Within `ALL_benchmark.xml <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/blob/refactor/example/jube/ALL_benchmark.xml>`_ you can find different options depending on the method and test you would like to run. For example the tests can be chosen by the ``--tag`` and using:

  1. ``Y``,
  2. ``Polymer``
  3. ``rot_Y``

In addition, the ``--type`` flag is used to choose the method ``4`` for the histogram-based staggered grid method. Note that the second example is the most illustrative and hence recommended example to understand how the borders of the cartesian planes are adjusted.


Source Code
___________

The implementation of the method in ALL can be found in `ALL_Histogram.hpp <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/blob/master/include/ALL_Histogram.hpp>`_.

The source code to the ALL library is available as a git repository at https://gitlab.version.fz-juelich.de/SLMS/loadbalancing . To obtain a copy of the repository you can use 

.. code:: bash

  git clone https://gitlab.version.fz-juelich.de/SLMS/loadbalancing.git
  
However, please note that the source code is currently under embargo until an associated paper is published, if you would like to be obtain a copy of the code, please contact Prof. Godehard Sutmann at ``g.sutmann@fz-juelich.de``.

