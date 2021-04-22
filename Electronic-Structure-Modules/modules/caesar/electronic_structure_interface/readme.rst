

..  sidebar:: Software Technical Information

  Name
    Caesar electronic structure interface

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


.. _Caesar_Electronic_Interface:

#####################################
Caesar electronic structure interface
#####################################

..  contents:: :local:

Purpose of Module
_________________

Calculating vibrational properties requires a mapping of the nuclear potential energy
surface (PES) :math:`V(\mathbf{r})`, where :math:`\mathbf{r}` is the collective
coordinate describing the locations of the nuclei. :ref:`Caesar <Caesar>` maps the PES
by sampling it at a number of nuclear configurations :math:`\mathbf{r}_i`.

In software terms, each PES sample represents a single electronic structure calculation,
where the electronic structure code is given the nuclear configuration
:math:`\mathbf{r}_i`, and calculates the value of the PES at that configuration,
:math:`V(\mathbf{r}_i)`, optionally along with other quantities such as the forces
:math:`\mathbf{f}(\mathbf{r}_i)=-\frac{\partial}{\partial \mathbf{r}}V|_{\mathbf{r}_i}`
and the Hessian matrix
:math:`H(\mathbf{r}_i) = \frac{\partial}{\partial \mathbf{r}}\frac{\partial}{\partial \mathbf{r}}V|_{\mathbf{r}_i}`.

The Caesar electronic structure interface enables :ref:`Caesar <Caesar>` to be used
with a wide range of electronic structure codes, by treating each electronic structure
calculation as a black box.

The Electronic Structure Run Script
___________________________________

:ref:`Caesar <Caesar>` generates a nested directory structure, with the input file for
each configuration :math:`\mathbf{r}_i` written to its own calculation directory. A
user-provided run script is then called repeatedly, once for each calculation
directory. This script is expected to read any required parameters from the root
directory, read the input file from the calculation directory, and then call the
electronic structure code and write the electronic structure results to an output
file in the calculation directory.

The calculation input and output files can be in a number of formats used by existing
electronic structure codes, or they can be in a simple plain-text format.

The plain-text input file, :download:`structure.dat` is formatted as

.. literalinclude:: structure.dat
   :language: none

where:

- :math:`L` is the supercell lattice matrix, whose rows are the lattice vectors of
  the supercell in which the electronic structure calculation should be performed.
- :math:`L'` is the reciprocal supercell lattice matrix of the supercell, defined
  as :math:`L'=L^{-T}`.
- :math:`S` is the supercell matrix, which relates the supercell lattice matrix
  :math:`L` to the primitive cell lattice matrix :math:`L_p` as :math:`L=SL_p`.
- :math:`S'` is the reciprocal supercell matrix, defined as :math:`S'=S^{-T}`.
- :math:`z_i` and :math:`\mathbf{r}_i` are the species label and cartesian
  coordinate of the :math:`i`'th atom.
- :math:`\{R_i\}` are the R-vectors of the primitive cell which are contained within
  the supercell.
- :math:`\{G_i\}` are the G-vectors of the reciprocal supercell which are contained
  within the reciprocal primitive cell.
- Subscripts :math:`x`, :math:`y` and :math:`z` denote cartesian components.

The plain-text output file, :download:`electronic_structure.dat` is formatted as

.. literalinclude:: electronic_structure.dat
   :language: none

where:

- :math:`V` is the energy of the supercell, normalised to energy per supercell.
- :math:`f_i` is the force on the :math:`i`'th atom. The number and order of atom
  labels must match those in the input file (i.e. be :math:`1` to :math:`n`).
- :math:`H_{ij}` is the block of the Hessian matrix corresponding to atoms :math:`i`
  and :math:`j`, i.e.
  :math:`\frac{\partial}{\partial \mathbf{r}_i}\frac{\partial}{\partial\mathbf{r}_j}V`.
- :math:`\sigma` is the stress tensor.
- Subscripts :math:`x`, :math:`y` and :math:`z` denote cartesian components.

All sections but ``Energy`` are optional. All values must be given in atomic units.

This file is parsed by the ``ElectronicStructureData`` class, and the documentation
for this class should be consulted for the full specification of this file. Each line
of the file is split into tokens by whitespace, so the exact whitespace on each line
does not matter as long as there is some whitespace between each token.

Interfaces to Other Electronic Structure Interfaces
___________________________________________________

Rather than interfacing with an electronic structure code directly,
:ref:`Caesar <Caesar>` can instead be interfaced with an external package which in turn
interfaces with the electronic structure code. :ref:`Caesar <Caesar>` has interfaces to
two such packages: `QUIP <https://libatoms.github.io/QUIP/>`_ and
`The Atomic Simulation Environment (ASE) <https://wiki.fysik.dtu.dk/ase/>`_.

The `QUIP <https://libatoms.github.io/QUIP/>`_ interface needs to be linked at compile
time, as detailed in the
`Caesar README.txt file <https://github.com/veryreverie/caesar>`_. This is achieved by
setting the `CMake <https://cmake.org/runningcmake>`_ configuration option
``LINK_TO_QUIP`` to true.

The `ASE <https://wiki.fysik.dtu.dk/ase/>`_ interface uses a
`python <https://www.python.org>`_ script and does not require additional compilation.
An example script is provided as ``doc/input_files/example_ase_run_script.py`` in
the `Caesar repository <https://github.com/veryreverie/caesar>`_, and this script is
intended to serve as a template for an interface with any of the electronic structure
codes which `ASE <https://wiki.fysik.dtu.dk/ase/>`_ can interface with.

Source Code
___________

The source code for the Caesar electronic structure interface is available from
the ``src/common/electronic_structure`` directory of the
`Caesar repository <https://github.com/veryreverie/caesar>`_

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html
