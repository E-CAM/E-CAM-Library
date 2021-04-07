

..  sidebar:: Software Technical Information

  Name
    Caesar

  Language
    Fortran

  Licence
    `GNU Lesser General Public License version 3 <https://www.gnu.org/licenses>`_

  Documentation Tool
    `Ford <https://github.com/Fortran-FOSS-Programmers/ford>`_

  Application Documentation
    See the `Caesar repository <https://github.com/veryreverie/caesar>`_

  Software Module Developed by
    Mark Johnson


.. _Caesar_Anharmonic:

#####################################
Caesar Anharmonic Calculation Library
#####################################

..  contents:: :local:


Purpose of Module
_________________

The :ref:`Caesar <Caesar>` anharmonic calculation library aims to provide an efficient
method for calculating vibrational properties beyond the harmonic approximation; under
the vibrational self-consistent harmonic approximation (VSCHA) [Errea_ea1]_ or using
vibrational self-consistent field theory (VSCF) [Christiansen1]_.

.. [Errea_ea1] Anharmonic free energies and phonon dispersions from the stochastic self-consistent harmomic approximation: Application to platinum and palladium hydrides. https://doi.org/10.1103/PhysRevB.89.064302
.. [Christiansen1] Vibrational structure theory: new vibrational wave function methods for calculation of anharmonic vibrational energies and vibrational contributions to molecular properties. https://doi.org/10.1039/B618764A


Theory
______

Fitting the Potential Energy Surface
------------------------------------

:ref:`Caesar <Caesar>` models the nuclear potential energy surface (PES) using a
truncated Taylor expansion in normal-mode coordinates. Constructing and fitting this
model happens over several steps:

 - Firstly, a set of symmetry-invariant basis functions are generated, using the
   crystal symmetries as calculated by `spglib <https://github.com/spglib>`_.
 - Secondly, a set of nuclear coordinates :math:`\mathbf{r}` are generated at which
   the PES will be sampled.
 - Thirdly, electronic structure calculations are performed at each coordinate, using
   the :ref:`Caesar_Electronic_Interface`.
 - Finally, the results of the electronic structure calculations, including calculated
   energies, forces and other information, are used to calculate the basis function
   coefficients.

As with the harmonic calculation, the anharmonic calculation uses the non-diagonal
supercell method [Lloyd-Williams_Monserrat1]_ to reduce the total computational cost of
the electronic structure calculations where possible.

.. [Lloyd-Williams_Monserrat1] Lattice dynamics and electron-phonon coupling calculations using nondiagonal supercells. https://doi.org/10.1103/PhysRevB.92.184301


The Vibrational Self-Consistent Harmonic Approximation
------------------------------------------------------

VSCHA approximates the eigenstates of the system as those which diagonalise an
effective harmonic potential :math:`V^\text{effective}`. The effective harmonic
potential :math:`V^\text{effective}` implemented by `Caesar <Caesar>`_ has the same
functional form and normal modes :math:`\{u_j\}` as the harmonic potential
:math:`V^\text{harmonic}`, but has a different set of frequencies :math:`\{\omega_j\}`.

The frequencies :math:`\{\omega_j\}` are calculated as those which minimise the free
energy of the anharmonic PES with respect to the VSCHA eigenstates [Errea_ea1]_.

Vibrational Self-consistent Field Theory
----------------------------------------

Traditional VSCF separates the PES :math:`V(\mathbf{u})` into a sum of single-mode
effective potentials :math:`\{V_j(u_j)\}`, each of which is the expectation of
:math:`V` with respect to all modes other than :math:`u_j`. The Hamiltonian
corresponding to each mode is then constructed in the VSCHA eigenbasis, and this is
diagonalised to give the single-mode VSCF eigenstates :math:`\{|\psi_{jk}>\}`. This
process can be written as two equations,

.. math::
  
  (T+V_j)|\psi_{jk}> = E_{jk}|\psi_{jk}>

and

.. math::
  
  V_j = <V>_{j'\neq j}

These equations are solved self-consistently, using a Pulay scheme [Pulay]_.

The VSCF method implemented by Caesar differs from traditional VSCF methods in that
rather than separating the PES single-mode potentials, the PES is instead separated
into single-subspace potentials, where each subspace contains a complete set of modes
whose frequencies are degenerate as a result of symmetry. This implementation of VSCF
is symmetry invariant, unlike the single-mode methods.

.. [Pulay] Convergence acceleration of iterative sequences. The case of scf iteration.https://doi.org/10.1016/0009-2614(80)80396-4

Performing Calculations
_______________________

Prior to performing anharmonic calculations, a harmonic calculation must be performed.
This can be done using the :ref:`Caesar_Harmonic`, or the Hessian matrix of the
undisplaced structure can be read using the :ref:`Caesar_Electronic_Interface`.

Like running the :ref:`Caesar_Harmonic`, running the :ref:`Caesar_Anharmonic` is a
four-stage process.

 - Firstly, ``caesar setup_anharmonic`` parses the input data and reads the output of
   the harmonic calculation. It then generates a directory structure containing
   directories in which all the necessary electronic structure calculations must be run.
 - Secondly, ``caesar run_anharmonic`` performs the electronic structure calculations,
   using the :ref:`Caesar_Electronic_Interface`. There is no connection between the
   separate electronic structure calculations, so they can be run sequentially, in
   parallel, or across multiple computers as desired.
 - Thirdly, ``caesar calculate_potential`` uses the results of the electronic structure
   calculations to fit the anharmonic potential.
 - Finally, ``caesar calculate_anharmonic_observables`` calculates the vibrational
   properties of the crystal under VSCHA and VSCF.

The calculated properties are written to an ``anharmonic_observables`` directory. These
can be visualised using the various ``caesar_plot_`` utilities.

As with the harmonic stages, each anharmonic stage has its own helptext, which can be
accessed through the ``caesar`` executable by calling ``caesar --help``.

Source Code
___________

The source code for Caesar anharmonic library is available from the ``src/anharmonic``
directory of the `Caesar repository <https://github.com/veryreverie/caesar>`_

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html
