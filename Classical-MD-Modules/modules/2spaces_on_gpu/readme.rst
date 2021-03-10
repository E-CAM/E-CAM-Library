:orphan:

..  sidebar:: Software Technical Information

  Name
    2spaces_on_gpu

  Language
    C, `OpenCL <https://www.khronos.org/opencl>`_

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    doxygen

  Application Documentation
    'https://gitlab.com/pcarrivain/2spaces_gpu/-/blob/master/latex/refman.pdf'

  Relevant Training Material
    not available yet

  Software Module Developed by
    Pascal Carrivain

.. _2spaces_on_gpu:

###########################
E-CAM 2spaces_on_gpu module
###########################

..  contents:: :local:

The 2spaces_on_gpu module implements the 2-spaces
algorithm on GPU (see Background information section).
This algorithm is designed to move one-half of the polymer
in one Monte-Carlo iteration.
It also preserves the excluded volume constraints.
I wrote a `OpenCL <https://www.khronos.org/opencl>`_
implementation to be used on CPUs or GPUs.

Purpose of Module
_________________

Polymer of size L is supposed to reach equilibrium
after a time like :math:`L^3`.
Therefore, it could be difficult to study the equilibrium
properties of large polymers.
The 2-spaces algorithm already improves the efficient
of each Monte-Carlo by moving half of the polymer.
We can use the GPU units to take care one of the sub-move
among st the Monte-Carlo step.

It is used in a scientific collaboration (ENS Lyon).

Background Information
______________________

Please consider reading the two research articles
`Massively Parallel Architectures and Polymer Simulation <https://www.semanticscholar.org/paper/Massively-Parallel-Architectures-and-Polymer-Ostrovsky-Smith/f79694076e40eca0fae9b35a381e43b7abfa029c>`_
and
`Cellular automata for polymer simulation with application to polymer melts and polymer collapse including implications for protein folding <https://www.sciencedirect.com/science/article/pii/S0167819100000818>`_
for details about the method.

Building and Testing
____________________

I provide a simple make file as well as `OpenCL <https://www.khronos.org/opencl>`_
kernel and main source code to run the model.
You need C++11 in order to use pseudo-random number generator.
Before the compilation you can clean the previous build
with "make mrproper" command.
Details about build, test and run are available on the
`2spaces_on_gpu GitLab repository <https://gitlab.com/pcarrivain/2spaces_gpu>`_.

Source Code
___________

The source code and more information can be find on the
`2spaces_on_gpu GitLab repository <https://gitlab.com/pcarrivain/2spaces_gpu>`_.
