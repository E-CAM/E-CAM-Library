..  sidebar:: Software Technical Information

  Name
    Verlet_list_for_ODE

  Language
    C/C++ and *Open-Dynamics-Engine* API

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    sphynx

  Application Documentation
    `doxygen documentation <https://gitlab.e-cam2020.eu/carrivain/verlet_list_for_ode/blob/master/refman.pdf>`_

  Relevant Training Material
    `pdf documentation <https://gitlab.e-cam2020.eu/carrivain/verlet_list_for_ode>`_

  Software Module Developed by
    Pascal Carrivain


.. _Verlet_list_for_ODE:

################################
E-CAM Verlet_list_for_ODE module
################################

..  contents:: :local:

The Verlet_list_for_ODE introduces `Verlet-list <https://en.wikipedia.org/wiki/Verlet_list>`_ for the rigid body dynamics `Open-Dynamics-Engine software <http://ode.org/>`_.

Purpose of Module
_________________

The rigid-body-dynamics is useful for mechanical articulated system.
In addition to that the tool allows the user to simulate complex shape and resolve excluded volume constraint.
It is used in the industry of video games to accurately reproduce physics.
However, a software like *Open-Dynamics-Engine* compute pairwise overlap every time-step.
The engine starts with a partition of the space and then loop over all the blocks of partition.
For each blocks it runs a nested loops to check the overlaps between the objects inside the block.
The module implements external functions that can be used to compute `Verlet-list <https://en.wikipedia.org/wiki/Verlet_list>`_.
Therefore, the user does not call the pairwise overlap check every time-step.
He only needs to loop over the `Verlet-list <https://en.wikipedia.org/wiki/Verlet_list>`_ with the pairwise of objects within a given cut-off distance.
However, the `Verlet-list <https://en.wikipedia.org/wiki/Verlet_list>`_ has to be updated according to the displacement length of the objects.

The module can be used to speed-up the *Open-Dynamics-Engine* simulation of polymers and complex objects system.

We test the module with two examples : chromatin fiber and bacterial circular DNA.
Chromatin fiber is an assembly of DNA wrapped around nucleosomes (histones core) that compact the genome.
We model the DNA at the scale of 10.5 base-pair (one helix turn) as an articulated system.
The nucleosomes is built with complex shape.
We run a Langevin dynamics and check that our `Verlet-list <https://en.wikipedia.org/wiki/Verlet_list>`_ implementation gives the same results *Open-Dynamics-Engine* would give.

Background Information
______________________

You can find a detailed description on the `Verlet_list_for_ODE GitLab repository <https://gitlab.e-cam2020.eu/carrivain/verlet_list_for_ode>`_.

Building and Testing
____________________

First of all you need to download and build the 0.16 version of *Open-Dynamics-Engine*.
You can find the steps on the `Verlet_list_for_ODE GitLab repository <https://gitlab.e-cam2020.eu/carrivain/verlet_list_for_ode>`_.
In order to compile the two examples (tests) I provide a simple make file you can find at the same location that the source code.
You need C++11 in order to use pseudo-random number generator.
It has `OpenMP <https://www.openmp.org>`_ acceleration. Edit the make file to enable it.
The example uses threads from std as well.
Before the compilation you can clean the previous build with *make mrproper* command.
The `Verlet-list <https://en.wikipedia.org/wiki/Verlet_list>`_ implementation returns the number of collisions that can be confronted with the result returns by *Open-Dynamics-Engine*.

Source Code
___________

The source code and more informations can be find at `Verlet_list_for_ODE GitLab repository <https://gitlab.e-cam2020.eu/carrivain/verlet_list_for_ode>`_.
