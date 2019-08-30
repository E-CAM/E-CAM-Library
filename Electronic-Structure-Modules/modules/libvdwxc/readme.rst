..  sidebar:: Software Technical Information

  Name
    libvdwxc

  Language
    C, with Fortran and Python interfaces.

  Licence
    `GPL <https://opensource.org/licenses/gpl-license>`_

  Documentation Tool
    Inline text comments for now. Doxygen will be used in the future.

  Application Documentation
    `Home page of libvdwxc <https://libvdwxc.org/>`_

  Relevant Training Material
    Not currently available.

  Software Module Developed by
    Mikael Kuisma, Ask Hjorth Larsen


.. _libvdwxc:

################################################################
libvdwxc - A library for vdW-DF exchange-correlation functionals
################################################################

..  contents:: :local:

This module allows the description of long-range electronic interactions
between atoms and molecules.


Purpose of Module
_________________

libvdwxc is a general library for evaluating energy and potential for
exchange-correlation (XC) functionals from the vdW-DF family that can be used
with various density functional theory (DFT) codes. This work was inspired by
the success of libXC, a library for local and semilocal XC functionals. At the
moment, libvdwxc provides access to the vdW-DF1, vdW-DF2, and vdW-CX
functionals. It provides interfaces for GPAW and Octopus. The library has been
tested with respect to the S22 test set, various bulk properties of metals and
semiconductors, and surface energies.

In a previous effort Marques et al. created a c-library called libxc. This
library consists of 150 different local and semi-local functionals, and it is
linked by 18 different codes. In other words, when using this library,
scientists have over 1500 different functional and code combinations to choose
from. The publication process of a functional becomes easier, because it needs
just to be added into one place. In a similar spirit, libvdwxc is intended as
a library that enables calculations of a particular family of functionals,
which cannot be readily added to libxc.


Background Information
______________________

All relevant information about libvdwxc, as well as downloadable packages, can
be found at:

- `The home page of libvdwxc <https://libvdwxc.org/>`_
- `The Git repository of libvdwxc <https://gitlab.com/libvdwxc/libvdwxc>`_


Building and Testing
____________________

libvdwxc provides an Autotools-based build system. Its build procedure is
relatively straightforward:

    cd libvdvwxc-x.y.x
    mkdir my_build_dir
    cd my_build_dir
    ../configure --prefix=/my/install/dir
    make
    make check
    make install

where x.y.z is the version of libvdwxc you want to install, my_build_dir is
the build directory where you will compile the library, and /my/install/dir is
the absolute path where you want to install it.

Build parameters can be adjusted by providing options to the configure script.
To get a list of available options, you can use the ``--help`` option of the configure script, e.g. run:

    ./configure --help

from the top source directory of libvdwxc, or:

    ../configure --help

from your build directory.

For more information about the Autotools, please consult `the Autotools Mythbuster <https://autotools.io/index.html>`_.


Source Code
___________

The source code of libvdwxc is hosted on `Gitlab <https://gitlab.com/libvdwxc/libvdwxc`_.

