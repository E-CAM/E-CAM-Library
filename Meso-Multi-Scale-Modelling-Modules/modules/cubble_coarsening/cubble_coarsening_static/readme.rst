..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    Cubble_coarsening_static


  Language
   C++/CUDA

  Licence
    This software will be released under the GPL licence.

  Documentation Tool
    Sphinx
     
  Application Documentation
    https://version.aalto.fi/gitlab/lankinj5/cuda_bubble

  Relevant Training Material
    https://version.aalto.fi/gitlab/lankinj5/cuda_bubble/wikis/home


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _cubble_coarsening_static:

########################################################
Cubble: Static foam coarsening simulator using c++/CUDA
########################################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

The mechanics of foams exhibit dynamical behavior familiar from other jammed materials,
such as granular matter. These are so called yield stress
fluids i.e. their flow requires external stress that exceeds a particular value, called
the yield stress. On the other hand, foams differ from other
materials by their internal structure development, coarsening, where while the gas
concentration of the foam remains constant, the gas diffuses
between the bubbles. The larger bubbles grow at the expense of the smaller ones. 

This module provides a c++/CUDA implementation of a foam coarsening simulator
(https://journals.aps.org/pre/abstract/10.1103/PhysRevE.98.012607)
designed from the bottom to be run in a GPU environment.



Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment


The goal was set to be able to run simulations involving up to one million bubbles
reaching a scaling state in systems
with non-periodic boundary conditions in three dimensions, an undoable task for even
the most efficient single core CPU implementation.
The Cubble code demonstrates the power of efficiently used GPU code, and provides a
model implementation strategy for
mesoscale DEM simulators. 



Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The code runs as a standalone simulation on a cluster (``triton.aalto.fi``) environment.
It is developed mostly on NVidia's Tesla P100 and V100 GPUs.


Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment


The binary is built using a build automation tool Make. The dimensionality of the
simulation is controlled from within the
``makefile``. Each ``make`` target is built into a separate directory: ``final``,
``default`` or
``debug``. Each of these directories has its
own ``makefile`` and all the targets are built/cleaned in the same way:

.. code-block:: bash

  make
  make clean

* ``final`` target is the one that should be run when doing simulations. It's the fastest
  and most optimized.
* ``default`` target is built with ``-O2`` flag so it's quite fast, but some internal debug
  capabilities are still on and it's significantly
  slower than the final target. Mostly for testing some new capabilities.
* ``debug`` is built with ``-O0`` and debug capabilities, only meant for debugging and therefore
  it is very slow.

In addition to the options above, there are some extra parameters in the ``makefile``
which can be used to e.g. turn profiling on/off.


The program can be run by typing

.. code-block:: bash

  make run

or by manually writing the path to the executable and the io files, e.g.

.. code-block:: bash

  final/bin/cubble input_parameters.json output_parameters.json

We include a set of reference parameters in
:download:`input_parameters.json <./input_parameters.json>`

The program runs until a certain amount of bubbles is left. After this, the program
writes one final data file and returns.
The parameter that controls the amount of bubbles (called ``MinNumBubbles``) should always
be larger than the number of bubbles
in one cell multiplied by ``3^NumDim``. In other words, if the number of bubbles in a
cell is 32 and the dimensionality of the
program is 2 (2D simulation), then the minimum number of bubbles should be larger than
``32 * 3^2 = 32 * 3 * 3 = 288``. For 3D
this would be 864. 300 and 900 are nice round numbers for ``MinNumBubbles``.

The reason for this is that the neighbor search is done in a manner that assumes at
least 3 cells in each dimension. If there
are less than 3 cells per dimension, some cells are searched through more than once,
leading to bubbles having the same bubble
as a neighbor multiple times. The implementation should and could be improved to
circumvent this, but "in the mean time" just
follow the above rule.


Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_


The source code is freely available for download at
`Cubble sources <https://github.com/KJLankinen/cubble>` and the module
specifically refers to the commit 3216d46c7e523e7885d12607197390496b379597.
