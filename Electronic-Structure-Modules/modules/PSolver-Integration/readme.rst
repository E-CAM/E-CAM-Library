..  sidebar:: Software Technical Information

  Name
    psolver_integration

  Language
    Fortran 95, with YAML I/O.

  Licence
    `GPL <https://opensource.org/licenses/gpl-license>`_

  Documentation Tool
    Doxygen

  Application Documentation
    `The Solver Package page on the BigDFT Wiki <http://bigdft.org/Wiki/index.php?title=The_Solver_Package>`_

  Relevant Training Material
    `The Solver Package page on the BigDFT Wiki <http://bigdft.org/Wiki/index.php?title=The_Solver_Package>`_

  Software Module Developed by
    BigDFT Developers, SIESTA Developers, Octopus Developers.


.. _psolver_integration:

############################################
Integration of PSolver in SIESTA and Octopus
############################################

..  contents:: :local:

A Poisson solver is an efficient tool to determine electromagnetic fields produced by an electric charge distributed in
space. The integration of PSolver into SIESTA and Octopus has opened the way for these software programs to access more
complex physical systems.
The PSolver library allows solving the Poisson equation in much more general ways than using Fourier Transforms.


Purpose of Module
_________________

The PSolver library solves the Poisson equation using wavelets. With this approximation one can more easily take into
account certain boundary conditions such as molecules (no boundaries), wires (periodic along 1 direction) and slabs
(periodic along 2 directions). This is in contrast to Fourier transforms which assumes periodic boundary conditions
along all lattice vectors. Additionally it allows cavities for different dielectric constants. 

This implementation integrates the PSolver library into the DFT codes SIESTA and Octopus such that they may be used
for end-users who require the functionalities.

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

Users of the SIESTA code have always been using the Fourier transforms for solving the Poisson equation. However, a
great deal of users are dealing with, in particular, slab systems given the advent of graphene, 2D materials and
surface calculations.
This integration allows users to control the boundaries in a very strict way without any approximations.
The latest PSolver library (shipped with BigDFT 1.9.0) will work.

Additional tests have been added to SIESTA to ensure that everything works.

The OCTOPUS code has various options to solve the Poisson equations. Amongst others were the ISF library,
which is a predecessor of the PSolver library. In the later OCTOPUS versions, an older version of PSolver 
was packaged with the OCTOPUS sources. For the recently released OCTOPUS 10, the interface to PSolver has 
been updated, so that  both the old and the new API of PSolver can be used.
This also prepares OCTOPUS to use the GPU version of PSolver, once it becomes available. The configure scripts of
OCTOPUS have been adapted to correctly detect and configure an installed PSolver library, and tests using the
library have been added to the OCTOPUS buildbot.


Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

To compile SIESTA with PSolver users should add this to their ``arch.make``

.. code-block:: make

  LIBS += -L<build-dir>/install/lib -lPSolver-1 -latlab-1 -lfutile-1 -ldicts -lfmalloc-1 -lyaml
  INCFLAGS += -I<build-dir>/install/include
  FPPFLAGS += -DSIESTA__PSOLVER

After building there are two tests, ``h2o_psolver`` and ``si2x1h-psolver`` which can be compared with ``h2o`` and
``si2x1h``, respectively. They should be comparable.


In order to compile OCTOPUS with the PSolver library, add the options ``--with-psolver-prefix`` and
``--with-futile-prefix`` to the `configure` command of OCTOPUS:

.. code-block:: sh

  ./configure --with-psolver-prefix=<PSolver-top-dir> --with-futile-prefix=<Futile-top-dir> ... 


The OCTOPUS test ``components/16-hartree_3d_psolver.test`` is testing the correct functionality of the PSolver
library in OCTOPUS.


Source Code
___________

* `PSolver in SIESTA <https://gitlab.com/siesta-project/siesta/-/merge_requests/10>`_
* `PSolver in OCTOPUS <https://gitlab.com/octopus-code/octopus/-/merge_requests?scope=all&utf8=%E2%9C%93&state=merged&search=psolver>`_


.. Here are the URL references used (which is alternative method to the one described above)

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html

