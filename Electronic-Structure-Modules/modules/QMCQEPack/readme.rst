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

.. _QMCQEPack:

####################
QMCQEPack
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

The QMCQEPack module allows to compute on the fly the single particle orbitals with Quantum Espresso, and
to use them in QMC simulations with the QMCPack software. The main feature that distinguish QMCQEPack from other orbital
converters is that all the computations are made on the fly, without having to write any output on the filesystem, 
allowing great efficiency and a considerable speed up in the overall workflow.

Being able to efficiently recompute single particle orbitals during a single simulation allows to move ions during the QMC procedure,
allowing for example geometry optimisation, relaxation and the implementation of the Coupled Electron Ion method or 
the computation of forces for Molecular Dynamics.

Background Information
______________________

QMCPack is available from the github repository `https://github.com/QMCPACK/qmcpack <https://github.com/QMCPACK/qmcpack>`_,
and the documentation can be found at the QMCPack website `https://qmcpack.org/documentation <https://qmcpack.org/documentation>`_.

Quantum Espresso is available from the github repository `https://github.com/QEF/q-e <https://github.com/QEF/q-e>`_, and the
documentation can be found in `http://www.quantum-espresso.org/Doc/user_guide/ <http://www.quantum-espresso.org/Doc/user_guide/>`_.

Building and Testing
____________________

To be done...

Source Code
___________

The source code will be available from the `E-CAM Gitlab <https://gitlab.e-cam2020.eu/>`_ under the QMCQEPack project.

.. Here are the URL references used (which is alternative method to the one described above)

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html

