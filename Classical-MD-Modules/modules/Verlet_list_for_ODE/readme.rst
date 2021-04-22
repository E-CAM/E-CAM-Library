
..  sidebar:: Software Technical Information

  Name
    Verlet_list_for_ODE

  Language
    C/C++ and `Open-Dynamics-Engine software <http://ode.org>`_ API

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    Doxygen

  Application Documentation
    `Doxygen documentation <https://gitlab.com/pcarrivain/fibre_ode/-/blob/master/latex/refman.pdf>`_

  Relevant Training Material
    None
  
  Software Module Developed by
    Pascal Carrivain


.. _Verlet_list_for_ODE:

####################################
E-CAM ``Verlet_list_for_ODE`` module
####################################

..  contents:: :local:

The ``Verlet_list_for_ODE`` module introduces
`Verlet-list <https://en.wikipedia.org/wiki/Verlet_list>`_
for the rigid-body dynamics
`Open-Dynamics-Engine software <http://ode.org>`_.

Purpose of Module
_________________

Rigid-body dynamics is useful for mechanical articulated systems.
In addition, the tool allows the user to simulate complex shape
and resolve excluded volume constraints.
It is used in the industry of video games to accurately reproduce physics.
However, a software like `Open-Dynamics-Engine <http://ode.org>`_
computes pairwise overlap every time-step.
The engine starts with a partition of the space and then
loops over all the blocks of partition.
For each blocks it runs nested loops to check the overlaps
between the objects inside the block.

The module implements external functions that can be used to compute the
`Verlet-list <https://en.wikipedia.org/wiki/Verlet_list>`_.
Therefore, the user does not call the pairwise overlap check every time-step.
He only needs to loop over
the `Verlet-list <https://en.wikipedia.org/wiki/Verlet_list>`_
with the pairwise objects within a given cut-off distance.
However, the `Verlet-list <https://en.wikipedia.org/wiki/Verlet_list>`_
has to be updated according to the displacement length of the objects.

The module can be used to speed-up
the `Open-Dynamics-Engine <http://ode.org>`_
simulation of polymers and complex objects system.

We test the module with two examples: chromatin fiber and bacterial circular DNA.
Chromatin fiber is an assembly of DNA wrapped around
`nucleosomes <https://en.wikipedia.org/wiki/Nucleosome>`_ that compact the genome.
We model the DNA at the scale of :math:`10.5` base-pair (one helix turn)
as an articulated system.
The nucleosomes is built with complex shape.
We run a `Langevin dynamics <https://en.wikipedia.org/wiki/Langevin_dynamics>`_
and check that our `Verlet-list <https://en.wikipedia.org/wiki/Verlet_list>`_
implementation gives the same results
`Open-Dynamics-Engine <http://ode.org>`_ would give.

Background Information
______________________

You can find a detailed description on the
`Verlet_list_for_ODE GitLab repository <https://gitlab.com/pcarrivain/fibre_ode>`_.

Building and Testing
____________________

First of all you need to download and build the 0.16
version of `Open-Dynamics-Engine <http://ode.org>`_.
You can find the steps on the
`Verlet_list_for_ODE GitLab repository <https://gitlab.com/pcarrivain/fibre_ode>`_.

In order to compile the two examples (tests) I provide a template ``Makefile``
you can find at the same location that the source code.
You need C++11 in order to use pseudo-random number generator.

It has `OpenMP <https://www.openmp.org>`_ acceleration,
edit the ``Makefile`` to enable it. The example uses threads
from ``std`` as well.

Before the compilation you can clean the
previous build with ``make mrproper`` command.
The `Verlet-list <https://en.wikipedia.org/wiki/Verlet_list>`_
implementation returns
the number of collisions that can be confronted
with the result returns by `Open-Dynamics-Engine <http://ode.org>`_.

Source Code
___________

The source code and more information can be find at
`Verlet_list_for_ODE GitLab repository <https://gitlab.com/pcarrivain/fibre_ode>`_.
