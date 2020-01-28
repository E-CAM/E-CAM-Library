..  sidebar:: Software Technical Information

  Name
    ESPWSCFInterface

  Language
    C++

  Licence

  Documentation Tool
    Doxygen

  Relevant Training Material
    Not currently available

  Software Module Developed by
    Michele Ruggeri, Raymond C. Clay III

.. _ESPWSCFInterface:

################
ESPWSCFInterface
################

..  contents:: :local:

Purpose of Module
_________________

To obtain accurate results with ground state Quantum Monte Carlo methods (such as Variational and Diffusion Monte Carlo) an accurate trial wave function is essential.
Such a wave function for an electron system will be typically given by the product of two factors: (1) a Jastrow term :math:`J`  describing electronic correlations and (2) a Slater determinant of suitable single particle orbitals :math:`\phi_i`

.. math::

  \Psi({\bf R}) = J({\bf R}) \cdot \text{Det}(\phi_i({\bf r}_j))

where :math:`R` is the vector containing the position of all electrons and :math:`r_i` is the position of the :math:`i`-th electron.
While there is great freedom in the definition of the Jastrow term, that can then be variationally optimized, the single particle orbitals have to be computed in using Density Functional Theory.

The ESPWSCFInterface module provides a derived class of ESInterfaceBase to generate single particle orbitals for QMCPack via a DFT computation performed with Quantum Espresso.

Background Information
______________________

QMCPack is available from the github repository `<https://github.com/QMCPACK/qmcpack>`_,
and the documentation can be found in the QMCPack website `<https://qmcpack.org/documentation>`_.

Quantum Espresso can be installed using the module :doc:`../QMCQEPack_qepatch/readme`, and the documentation
can be found in the Quantum Espresso website `<https://www.quantum-espresso.org/resources/users-manual>`_.

Building and testing
____________________

The ESPWSCFInterface module can be found in the ``QMCQEPack`` branch of the QMCPack git repository 
`<https://github.com/michruggeri/qmcpack/tree/QMCQEPack>`_.
After cloning the repository and checking out the ``QMCQEPack`` branch with

``git clone https://github.com/michruggeri/qmcpack.git``

``git checkout QMCQEPack``

one can proceed to download Quantum Espresso and build the ``libpwinterface.so`` library
using the :ref:`QMCQEPack_qepatch` module.

Once the library is built one can proceed to build and compile  QMCPack, as
detailed in the official QMCPack documentation `<https://qmcpack.org/documentation>`_, or in the manual available
in the ``manual`` subdirectory in the main QMCPack directory. Note that to use the Quantum Espresso interface the 
``cmake`` options ``QE_INTERFACE`` must be used, typically with

``cmake -DQE_INTERFACE=1 -DQMC_COMPLEX=1 <QMCPack base directory>``

before compiling with ``make``.

To use the interface one must use the ``qmcqepack`` keyword in the ``determinantset`` block in a QMCPack input file; further information can be found in Section 22.5.3 of the QMCPack manual, that can be compiled with the files in the ``manual`` directory.

The tests for this code are part of the deterministic unit tests for QMCPack, that can be run with the command

``ctest -R interface``

Note that the code is tested using the GCC compiler and OpenMPI.

Source Code
___________

The source code is available available from `<https://github.com/michruggeri/qmcpack/tree/QMCQEPack>`_ in the ``QMCQEPack`` branch. Specifically relevant files for this module include:

* ``src/Interfaces/PWSCF/ESPWSCFInterface.cpp``
* ``src/Interfaces/PWSCF/ESPWSCFInterface.h``
* ``src/Interfaces/PWSCF/pwinterface.h``

and for the tests:

* ``src/Interfaces/tests/pwscf.in``
* ``src/Interfaces/tests/test_interface_PWSCF.cpp``

.. Here are the URL references used (which is alternative method to the one described above)

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html

