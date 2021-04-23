..  sidebar:: Software Technical Information

  Name
    elsi_rci_in_dftb+

  Language
    Fortran, C++.

  Licence
    BSD 3-clause license <https://opensource.org/licenses/BSD-3-Clause>`_

  Documentation Tool
    Doxygen

  Application Documentation
    `ELSI-RCI USer Manual <https://git.elsi-interchange.org/elsi-devel/elsi_rci/-/blob/master/doc/elsi_rci_manual.pdf>`_,
    `DFTB+ Documentation <https://dftbplus.org/documentation/>`_

  Relevant Training Material
    Not currently available.

  Software Module Developed by
    ELSI-RCI Developers, DFTB+ Developers.


.. _elsi_rci_in_dftb_plus:

################################
Integration of ELSI-RCI in DFTB+
################################

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

DFTB+ is a software package for carrying out fast quantum mechanical atomistic calculations based on the
Density Functional Tight Binding method. ELSI-RCI provides and enhances open-source software
packages which solve mathematical equations related to the simulation
of materials and molecules at the atomic scale.


Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

Integrating the ELSI library into DFTB+ makes the
the additional ELPA, OMM, PEXSI and NTPoly solvers available. These solvers are particularly useful for
large scale systems.


Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module is developed in connection with the Extended Software Development Workshop
`"Integration of ESL modules into electronic-structure codes" <https://www.e-cam2020.eu/event/integration-of-esl-modules-into-electronic-structure-codes/>`_
held in Lausanne in February 2020.

An associated paper which includes a description of the ELSI integration is DFTB+ has also been published [DFTB]_. 

.. [DFTB] B. Hourahine, B. Aradi, V. Blum, F. Bonafé, A. Buccheri, C. Camacho, C. Cevallos, M. Y. Deshaye, T. Dumitrică, A. Dominguez, S. Ehlert, M. Elstner, T. van der Heide, J. Hermann, S. Irle, J. J. Kranz, C. Köhler, T. Kowalczyk, T. Kubař, I. S. Lee, V. Lutsker, R. J. Maurer, S. K. Min, I. Mitchell, C. Negre, T. A. Niehaus, A. M. N. Niklasson, A. J. Page, A. Pecchia, G. Penazzi, M. P. Persson, J. Řezáč, C. G. Sánchez, M. Sternberg, M. Stöhr, F. Stuckenberg, A. Tkatchenko, V. W.-z. Yu, and T. Frauenheim , "DFTB+, a software package for efficient approximate density functional theory based atomistic simulations" , The Journal of Chemical Physics 152, 124101 (2020) https://doi.org/10.1063/1.5143190


Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

ELSI support is available in the latest releases of DFTB+. Full installation and testing documentation is
available in the
`Install.rst <https://github.com/dftbplus/dftbplus/blob/20.2.1/INSTALL.rst>`_
file of the DFTB+ release.

Specifically to enable the ELSI support, one would require the CMake option ``-DWITH_ELSI`` (and also ``-DWITH_PEXSI``
if the PEXSI solver is also to be supported). 

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

* `Link to a merge request containing relevant source code changes
  <https://github.com/dftbplus/dftbplus/pull/175>`_

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html

