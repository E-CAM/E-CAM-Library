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

QMCPack is available from the github repository `<https://github.com/QMCPACK/qmcpack>`_,
and the documentation can be found at the QMCPack website `<https://qmcpack.org/documentation>`_.

Quantum Espresso is available from the github repository `<https://github.com/QEF/q-e>`_, and the
documentation can be found in `<http://www.quantum-espresso.org/Doc/user_guide/>`_.

Building and testing
____________________

The first step to install QMCQEPack is cloning the QMCQEPack branch of the QMCPack git repository 
`<https://github.com/michruggeri/qmcpack/tree/QMCQEPack>`_.
After cloning and getting to the QMCQEPack branch with

``git clone https://github.com/michruggeri/qmcpack.git``

``git checkout QMCQEPack``

one has to install, patch and compile the required Quantum Espresso libraries.
This can be done executing the ``QMCQEPack_download_and_patch_qe.sh`` script found in ``qmcpack/external_codes/quantum_espresso``.
Once the patching process is complete one just has to run the ``configure`` script in the resulting folder q-e-qe5.3 and finally
build the ``libpwinterface.so`` library with

``make pw``

Note that in order to build this library the FFTW3 library must be compiled as position independent code (PIC);
if that is not the case it is possible to use the internal version of FFTW library changing the ``__FFTW3`` flag to ``__FFTW`` in
``make.sys``.

Once the ``libpwinterface.so`` library is compiled and included/linked to the library path one can procede to build the QMCPack software, as
detailed in the official QMCPack documentation `<https://qmcpack.org/documentation>`_.

To test the code one can run the tests in the directory ``qmcpack/QMCQEPack_test``; here there are folders with input files for 
some simulation examples each with their one ``Reference`` directory to check the results. A more detailed
``Readme`` file is also present there.

It should be noted that the current version of this module only supports serial runs; a parallel version is currently in developement.

Source Code
___________

The source code is available available from `<https://github.com/michruggeri/qmcpack/tree/QMCQEPack>`_. 

.. Here are the URL references used (which is alternative method to the one described above)

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html

