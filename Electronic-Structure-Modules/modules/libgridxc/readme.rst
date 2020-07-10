..  We allow the template to be standalone, so that the library maintainers add it in the right place

..  sidebar:: Software Technical Information

  Name
    LibGridXC

  Language
    Fortran

  Licence
    `BSD 3-Clause <https://opensource.org/licenses/BSD-3-Clause>`_

  Documentation Tool
    SIESTA Documentation Specifications (document available soon).

  Application Documentation
    Source code can be browsed on `GitLab <https://gitlab.com/siesta-project/libgridxc>`_.

  Relevant Training Material
    Not currently available.

  Software Module Developed by
    J.M. Soler, A. Garcia, M. Oliveira, Y. Pouillon, C. Balbás and N. R. Papior


.. _libgridxc:

###############################################################################
LibGridXC - Exchange-correlation energies and potentials in radial and 3D grids
###############################################################################

..  contents:: :local:

This module allows the calculation of electronic exchange and
correlation energies and potentials on simulation grids, both on
serial and parallel computers.


Purpose of Module
_________________

LibGridXC provides routines to calculate the exchange and correlation energy
and potential in spherical (i.e. an atom) or periodic systems, using a variety
of LDA and GGA functionals, as well as a variety of van der Waals DFT
functionals [DION2004]_ [KLIMES2009]_ [LEE2010]_ [VYDROV2010]_, implemented as
described by Román-Pérez and Soler [ROMAN2009]_.

.. [DION2004] Dion et al., Phys. Rev. Lett. 92, 246401 (2004).
.. [LEE2010] Lee et al., Phys. Rev. B 82, 081101 (2010).
.. [KLIMES2009] Klimes et al., J. Phys. Cond. Matt. 22, 022201 (2009).
.. [VYDROV2010] Vydrov, VanVoorhis, J. Chem. Phys. 133, 244103 (2010).
.. [ROMAN2009] Román-Pérez, Soler, Phys. Rev. Lett. 103, 096102 (2009).


Background Information
______________________

LibGridXC was originally developed within SIESTA, under the name SiestaXC, and
then extracted as a stand-alone module for the Electronic Structure Library,
to be shared with other codes than SIESTA. The development efforts carried out
to make it a module include the design and implementation of an
Autotools-based build system compatible with the one of SIESTA, as well as the
migration to Git for version control and the setting up of a Continuous
Integration (CI) process.


Building and Testing
____________________

LibGridXC provides an Autotools-based build system. Its build procedure is
relatively straightforward:

    cd libgridxc-x.y.x
    mkdir my_build_dir
    cd my_build_dir
    ../configure --prefix=/my/install/dir
    make
    make check
    make install

where ``x.y.z`` is the version of LibGridXC you want to install,
``my_build_dir`` is the build directory where you will compile the library,
and ``/my/install/dir`` is the absolute path where you want to install it.

Build parameters can be adjusted by providing options to the configure script.
To get a list of available options, you can use the ``--help`` option of the
configure script, e.g. run:

    ./configure --help

from the top source directory of LibGridXC, or:

    ../configure --help

from your build directory. By using the ``--enable-multiconfig`` option of
configure, you will be able to install both a serial and a MPI-aware version
of LibGridXC with the same install prefix.

For more information about the Autotools, please consult `the Autotools Mythbuster <https://autotools.io/index.html>`_.


Source Code
___________

The source code of LibGridXC is hosted on `GitLab <https://gitlab.com/siesta-project/libgridxc>`_.

