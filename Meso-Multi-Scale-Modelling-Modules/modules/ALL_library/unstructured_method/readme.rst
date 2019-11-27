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

.. _ALL_unstructured:

############################
ALL Unstructured Mesh Method
############################

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

In contrast to :ref:`ALL_tensor` and :ref:`ALL_staggered`, this method adjusts
domains not by moving boundaries but vertices, i.e. corner points, of
domains. For each vertex a force, based on the differences
in work of the neighboring domains, is computed and the
vertex is shifted in a way to equalize the work between these
neighboring domains.

Background Information
______________________

See :ref:`ALL_background` for details.

Building and Testing
____________________

See :ref:`ALL_testing` for details.

Source Code
___________

The implementation of the method in ALL can be found in `ALL_Unstructured.hpp <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/blob/master/include/ALL_Unstructured.hpp>`_.

The source code to the ALL library is available as a git repository at https://gitlab.version.fz-juelich.de/SLMS/loadbalancing . To obtain a copy of the repository you can use 

.. code:: bash

  git clone https://gitlab.version.fz-juelich.de/SLMS/loadbalancing.git
  
However, please note that the source code is currently under embargo until an associated paper is published, if you would like to be obtain a copy of the code, please contact Prof. Godehard Sutmann at ``g.sutmann@fz-juelich.de``.

