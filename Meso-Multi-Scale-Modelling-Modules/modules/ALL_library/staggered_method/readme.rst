:orphan:

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

.. _ALL_staggered:

#########################
ALL Staggered Grid Method
#########################

..  contents:: :local:

A Load-Balancing Library (ALL) library aims to provide an easy and portable way
to include dynamic domain-based load balancing into particle based simulation
codes. The library is developed in the Simulation Laboratory Molecular Systems
of the Juelich Supercomputing Centre at Forschungszentrum Juelich.

Purpose of Module
_________________

This module provides an additional method to the `ALL library <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing>`_,
up-to-date descriptions of the methods in the library can be found in the
`ALL README file <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/blob/master/README.md>`_.

In the *staggered-grid* scheme, a 3-step hierarchical approach is applied,
where:

* work over the cartesian planes is reduced, before the borders of these planes
  are adjusted;
* in each of the cartesian planes the work is reduced for each cartesian column.
  These columns are then adjusted to each other to homogenize the work in each
  column;
* the work between neighboring domains in each column is adjusted.

Each adjustment is done locally with the neighboring planes, columns or domains
by adjusting the adjacent boundaries.

Background Information
______________________

See :ref:`ALL_background` for details.

Building and Testing
____________________

ALL is a C++ header only library using template programming, strictly speaking
there is no need to install the library, you simply include the header files in
your application. In order to provide examples, ALL uses the
`CMake <https://cmake.org/runningcmake/>`_ build system, specific build and
installation requirements can be found in the
`ALL README file <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/blob/master/README.md>`_.
If you wish to use/test the topological mesh scheme, you will need an MPI-enabled
installation of the `VTK <https://vtk.org/>`_ package.

To build ALL, begin in the root directory of the package and use

.. code:: bash

  export ALL_INSTALLATION=/path/to/my/loadbalancing/install
  mkdir build
  cd build
  cmake .. -DCMAKE_INSTALL_PREFIX=$ALL_INSTALLATION -DCM_ALL_VTK_OUTPUT=ON -DCM_ALL_VORONOI=ON
  make -j
  make install
  cd ..
  
This will create an installation of ALL in the path pointed to by
``ALL_INSTALLATION``. ``ALL_test`` (in the ``bin`` folder) is the binary that
performs the tests. If you omit the option ``-DCM_ALL_VTK_OUTPUT=ON`` you will
not require  the VTK dependency (but cannot use the unstructured mesh method).

In the ``example/jube/input`` subdirectory there are 3 test data sets available,
namely:

1. Simple Wye-shape biosystem;
2. Heterogeneous polymer melt and
3. A rotated version of the Wye-shaped biosystem.

These data sets are in raw ascii format and need to be translated into a format
that can be consumed by ``ALL_test``. A utility ``ASCII2MPIBIN`` is provided to
do the conversion, with the command line options:

.. code:: bash

  ASCII2MPIBIN <in_file (ASCII)> <out_file (binary)> <n_x> <n_y> <n_z>
  
where ``n_x``, ``n_y``, ``n_z`` are the number of (MPI) processes (in the X, Y
and Z directions) that will be used.

``ALL_test`` takes a number of options,

.. code:: bash

  ALL_test <Method> <Number of iterations> <gamma> <weighted> <input file> <system size: x, y, z> <domain layout: x, y, z>

``Method`` (integer) is the load-balancing scheme to use, there are 5 options:

.. code:: bash

  0 : Tensor
  1 : Staggered
  2 : Unstructured
  3 : Voronoi
  4 : Histogram
  
, ``gamma`` (double) is a relaxation which controls the convergence of the
load-balancing methods, ``weighted`` (boolean) indicates whether points should
be assigned a weight. The system size and domain layout are provided in the
output of the call to ``ASCII2MPIBIN``.


An example execution using the polymer melt data set on 125 processors looks
like

.. code:: bash

  ASCII2MPIBIN globalBlockCoordsPolymer.txt input.bin 5 5 5
  mpirun -n 125 ALL_test 1 50 8.0 0 input.bin 80 80 450 5 5 5


Source Code
___________

The implementation of the method in ALL can be found in `ALL_Staggered.hpp <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/blob/master/include/ALL_Staggered.hpp>`_.

The source code to the ALL library is available as a git repository at https://gitlab.version.fz-juelich.de/SLMS/loadbalancing . To obtain a copy of the repository you can use 

.. code:: bash

  git clone https://gitlab.version.fz-juelich.de/SLMS/loadbalancing.git
  
However, please note that the source code is currently under embargo until an associated paper is published, if you would like to be obtain a copy of the code, please contact Prof. Godehard Sutmann at ``g.sutmann@fz-juelich.de``.

