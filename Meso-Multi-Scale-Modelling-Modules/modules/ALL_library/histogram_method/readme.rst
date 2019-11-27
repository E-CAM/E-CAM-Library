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

#########################################
ALL Histogram-based Staggered Grid Method
#########################################

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

See :ref:`ALL_testing` for details.

Source Code
___________

The implementation of the method in ALL can be found in
`ALL_Histogram.hpp <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/blob/master/include/ALL_Histogram.hpp>`_.

The source code to the ALL library is available as a git repository at https://gitlab.version.fz-juelich.de/SLMS/loadbalancing . To obtain a copy of the repository you can use 

.. code:: bash

  git clone https://gitlab.version.fz-juelich.de/SLMS/loadbalancing.git
  
However, please note that the source code is currently under embargo until an associated paper is published, if you would like to be obtain a copy of the code, please contact Prof. Godehard Sutmann at ``g.sutmann@fz-juelich.de``.

