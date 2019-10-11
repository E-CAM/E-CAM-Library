:orphan:

..  sidebar:: Software Technical Information

  Name
    QMCQEPack

  Language
    C++, Fortran

  Licence

  Documentation Tool
    Doxygen

  Relevant Training Material
    Not currently available

  Software Module Developed by
    Michele Ruggeri
    Raymond Clay III

.. _ESBaseInterface:

####################
ESBaseInterface
####################

..  contents:: :local:

QMCQEPack is an interface between the QMCPack software for Quantum Monte Carlo simulations of electronic structure
and the DFT software Quantum Espresso, that allows the DFT computation on the fly of single electron orbitals
to be included in trial wave functions for QMC simulations 

Purpose of Module
_________________

To obtain accurate results with ground state Quantum Monte Carlo methods (such as Variational and Diffusion Monte Carlo) an accurate
trial wave function is essential. Such a wave function for an electron system will be typically
given by the product of two factors: (1) a Jastrow term describing inter electron correlations and (2) a Slater determinant of suitable single
particle orbitals. While there is great freedom in the definition of the Jastrow term, that can then be variationally 
optimized, the single particle orbitals have to be computed in using Density Functional Theory.

The ESBaseInterface module provides a base class for a general interface to generate single particle orbitals for
QMC simulations performed using QMCPack;  implementations of specific implementations as derived classes of
ESInterfaceBase are available as separate modules.

Background Information
______________________

QMCPack is available from the github repository `<https://github.com/QMCPACK/qmcpack>`_,
and the documentation can be found at the QMCPack website `<https://qmcpack.org/documentation>`_.

Building and testing
____________________

The ESBaseInterface module can be found in the QMCQEPack branch of the QMCPack git repository 
`<https://github.com/michruggeri/qmcpack/tree/QMCQEPack>`_.
After cloning and getting to the QMCQEPack branch with

``git clone https://github.com/michruggeri/qmcpack.git``

``git checkout QMCQEPack``

Once the repository in cloned one can proceed to build the QMCPack software, as
detailed in the official QMCPack documentation `<https://qmcpack.org/documentation>`_.

The tests for this code are part of the deterministic unit tests for QMCPack, that can be run with the command

``ctest -R deterministic``

Source Code
___________

The source code is available available from `<https://github.com/michruggeri/qmcpack/tree/QMCQEPack>`_. 

.. Here are the URL references used (which is alternative method to the one described above)

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html

