
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


.. _Caesar_Harmonic:

###################################
Caesar Harmonic Calculation Library
###################################

..  contents:: :local:


Purpose of Module
_________________

The :ref:`Caesar <Caesar>` harmonic calculation library aims to provide an efficient
method for calculating vibrational properties under the harmonic approximation
[Hoja_ea1]_. This can be done using a range of electronic structure codes, using the
:ref:`Caesar_Electronic_Interface`.

.. [Hoja_ea1] First-principles modelling of molecular crystals: structures and stabilities, temperature and pressure. https://doi.org/10.1002/wcms.1294

Theory
______

The Harmonic Approximation
--------------------------

The nuclear potential energy surface :math:`V` is a function of the
:math:`3n`-dimensional nuclear coordinate :math:`\mathbf{r}`. Under the harmonic
approximation [Hoja_ea1]_, this function is approximated as quadratic in the difference
between :math:`\mathbf{r}` and the value of :math:`\mathbf{r}` for the undisplaced
structure, :math:`\mathbf{r}^{(0)}`. Formally, this is

.. math::

  V(\mathbf{r}) = (\mathbf{r}-\mathbf{r}^{(0)})\cdot H \cdot(\mathbf{r}-\mathbf{r}^{(0)})

where :math:`H` is the Hessian matrix.

By making a coordinate transform from :math:`\mathbf{r}=\sum_ir_i\hat{\mathbf{r}}_i`
to :math:`\mathbf{r}=\sum_ju_j\hat{\mathbf{u}}_j`, the Hessian matrix can be
diagonalised, to give

.. math::
  
  V(\mathbf{u}) = \sum_j\frac{1}{2}N\omega_j^2u_j^2

where each :math:`u_j` is a normal mode of the system, each :math:`\omega_j` is the
corresponding frequency of that mode, and :math:`N` is the size of the supercell
(defined as the ratio of the volume of the supercell to the volume of the primitive
cell).

Provided that every value of :math:`\omega_j` is real, the simple form of :math:`V`
means that the Hamiltonian can be diagonalised analytically, and so the free energy
and other properties can be calculated analytically [Hoja_ea1]_.

Calculating the Hessian Matrix
------------------------------

:ref:`Caesar <Caesar>` calculates the Hessian matrix using a finite difference
method. This involves calculating the forces on the atoms in atomic configurations
where the atomic displacement :math:`\mathbf{r}-\mathbf{r}^{(0)}` is small. Forces
must be calculated by an electronic structure code, via the
:ref:`Caesar_Electronic_Interface`.

:ref:`Caesar <Caesar>` minimises the total computational cost of these electronic
structure calculations by exploiting crystal symmetry, as calculated by the
`spglib <https://github.com/spglib>`_ crystal symmetry library, and by using the
non-diagonal supercell method [Lloyd-Williams_Monserrat]_.

.. [Lloyd-Williams_Monserrat] Lattice dynamics and electron-phonon coupling calculations using nondiagonal supercells. https://doi.org/10.1103/PhysRevB.92.184301

Supercells
----------

Most vibrations in crystals cause atoms to move in ways which break translational
symmetry. This means that it is not sufficient to calculate vibrational properties
in the primitive unit cell alone. Instead, properties must be calculated in a supercell
containing multiple copies of the primitive unit cell. Properties should be calculated
in a number of different supercells, and the size of the supercell should be increased
until the results of the calculations converge.

Performing Calculations
_______________________

Running the :ref:`Caesar_Harmonic` is a four-stage process.

 - Firstly, ``caesar setup_harmonic`` parses the input data, calls
   `spglib <https://github.com/spglib>`_ to calculate the crystal symmetries, and
   generates a directory structure containing directories in which all the necessary
   electronic structure calculations must be run.
 - Secondly, ``caesar run_harmonic`` performs the electronic structure calculations,
   using the :ref:`Caesar_Electronic_Interface`. There is no connection between the
   separate electronic structure calculations, so they can be run sequentially, in
   parallel, or across multiple computers as desired.
 - Thirdly, ``caesar calculate_normal_modes`` uses the results of the electronic
   structure calculations to calculate the Hessian matrix and the normal modes of the
   crystal.
 - Finally, ``caesar calculate_harmonic_observables`` calculates the vibrational
   properties of the crystal under the harmonic approximation.

The stages are separated so that the potentially computationally costly
``run_harmonic`` can be run on a separate computer or computers as needed, and so
that ``calculate_harmonic_observables`` can be run repeatedly to calculate different
observables as required.

The calculated properties are written to a ``harmonic_observables`` directory. These
can be visualised using the various ``caesar plot_`` utilities.

Each stage of the calculation has its own helptext, which can be accessed through
the ``caesar`` executable by calling ``caesar --help``.

Source Code
___________

The source code for the Caesar harmonic library is available from the ``src/harmonic``
directory of the `Caesar repository <https://github.com/veryreverie/caesar>`_

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html
