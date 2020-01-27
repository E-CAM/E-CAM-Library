..  sidebar:: Software Technical Information

  Name
    ESHDF5Interface

  Language
    C++

  Licence

  Documentation Tool
    Doxygen

  Relevant Training Material
    Not currently available

  Software Module Developed by
    Michele Ruggeri, Raymond C. Clay III

.. _ESHDF5Interface:

###############
ESHDF5Interface
###############

..  contents:: :local:

Purpose of Module
_________________

To obtain accurate results with ground state Quantum Monte Carlo methods (such as Variational and Diffusion Monte Carlo) an accurate trial wave function is essential.
Such a wave function for an electron system will be typically given by the product of two factors: (1) a Jastrow term :math:`J`  describing electronic correlations and (2) a Slater determinant of suitable single particle orbitals :math:`\phi_i`

.. math::

  \Psi({\bf R}) = J({\bf R}) \cdot \text{Det}(\phi_i({\bf r}_j))

where :math:`R` is the vector containing the position of all electrons and :math:`r_i` is the position of the :math:`i`-th electron.
While there is great freedom in the definition of the Jastrow term, that can then be variationally optimized, the single particle orbitals have to be computed in using Density Functional Theory.

The ESHDF5Interface module provides a derived class of ESInterfaceBase to generate single particle orbitals for QMC simulations performed using QMCPack from a suitable HDF5 file.

Background Information
______________________

QMCPack is available from the github repository `<https://github.com/QMCPACK/qmcpack>`_,
and the documentation can be found at the QMCPack website `<https://qmcpack.org/documentation>`_.

Building and testing
____________________

The EHDF5Interface module can be found in the QMCQEPack branch of the QMCPack git repository 
`<https://github.com/michruggeri/qmcpack/tree/QMCQEPack>`_.
After cloning and getting to the QMCQEPack branch with

``git clone https://github.com/michruggeri/qmcpack.git``

``git checkout QMCQEPack``

one can proceed to build the QMCPack software, as
detailed in the official QMCPack documentation `<https://qmcpack.org/documentation>`_, or in the manual available
in the ``manual`` subdirectory in the main QMCPack directory.

To use the interface one must use the ``interfaceh5`` keyword in the ``determinantset`` block in a QMCPack input file; further information can be found in Section 22.5.2 of the QMCPack manual, that can be compiled with the files in the ``manual`` directory.

The tests for this code are part of the deterministic unit tests for QMCPack, that can be run with the command

``ctest -R deterministic``

Source Code
___________

The source code is available available from `<https://github.com/michruggeri/qmcpack/tree/QMCQEPack>`_ in the `QMCQEPack` branch. Specifically relevant files for this module include:

* `src/Interfaces/ESHDF5/ESHDF5Interface.cpp`
* `src/Interfaces/ESHDF5/ESHDF5Interface.h`

and for the tests:

* `src/Interfaces/tests/test_interface_HDF5.cpp`
* `src/Interfaces/tests/O.BFD.upf`

.. Here are the URL references used (which is alternative method to the one described above)

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html

