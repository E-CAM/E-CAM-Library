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

  Polymer melt test is provided by
    Dr. Horacio V. Guzman
 
  Module Committed by
    Dr. Horacio V. Guzman


.. _ALL_tensor_method:

#########################
ALL Tensor-Product method
#########################

..  contents:: :local:

A Load-Balancing Library (ALL) library aims to provide an easy and portable way to include dynamic domain-based load balancing
into particle based simulation codes. The library is developed in the Simulation Laboratory Molecular Systems of the
Juelich Supercomputing Centre at Forschungszentrum Juelich.

Purpose of Module
_________________

This module provides an additional method to the ALL library, up-to-date descriptions of the methods in the library can
be found in the `ALL README file <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/blob/refactor/README.md>`_.

For the Tensor-Product method, the work on all processes (subdomains) is reduced over the cartesian planes in the systems. This work
is then equalized by adjusting the borders of the cartesian planes.

Background Information
______________________

See :ref:`ALL_background` for details.

Building and Testing
____________________

ALL uses the `CMake <https://cmake.org/runningcmake/>`_ build system, specific build and installation requirements can
be found in the `ALL README file <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/blob/refactor/README.md>`_.

There are 3 tests of available for the Tensor-Product method in the `ALL GitLab repository examples <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/tree/refactor/example>`_ . Namely: (1) Simple Wye-shape biosystem; (2) Heterogeneous polymer melt and (3) A rotated version of the Wye-shaped biosystem. They can be run by tying the commands below:

export ALL_INSTALLATION=/path/to/my/loadbalancing/install
cmake .. -DCMAKE_INSTALL_PREFIX=$ALL_INSTALLATION -DCM_ALL_FORTRAN=ON
make -j
make install
cd example/jube/
ln -s $ALL_INSTALLATION/bin/ALL_test
# JUBE must be available in the environment
jube run ALL_benchmark.xml --tag Polymer --type 1

within `ALL_benchmark.xml <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/blob/refactor/example/jube/ALL_benchmark.xml>`_ you can find different options depending on the method and test you would like to run. For examle the tests can be chosen by the --tag and using (1) 'Y', (2) 'Polymer' and (3) 'rot_Y', correspondingly. In addition, the --type flag is used to choose the method 1 for the Tensor Prodcut. Note that example (2) is the most illustrative and hence recommended example to understand how the borders of the cartesian planes are adjusted.


Source Code
___________

The implementation of the Tensor-Product method in ALL can be found in `ALL_Tensor.hpp <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/blob/refactor/include/ALL_Tensor.hpp>`.

The source code to the ALL library is available as a git repository at https://gitlab.version.fz-juelich.de/SLMS/loadbalancing . To obtain a copy of the repository you can use `git clone https://gitlab.version.fz-juelich.de/SLMS/loadbalancing.git`). However, please note that the source code is currently under embargo until an associated paper is published, if you would like to be obtain a copy of the code, please contact Prof. Godehard Sutmann at g.sutmann@fz-juelich.de.

