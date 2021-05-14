..  sidebar:: Software Technical Information
   
   This module is a python port of particle insertion suite of codes
   It also extends and generalizes the apporoch.

    Language 
        Python (3.6+)
    
    Licence
        The software for this specific module is licensed under `BSD-3-Clause
        <https://opensource.org/licenses/BSD-3-Clause>`_

    Documentation Tool
        `pdoc`_, numpydoc format (ReST)    
    
    Application Documentation
        `Documentation <https://gitlab.com/aestheses/insertion_utils/-/tree/master/docs>`_

    Relevant Training Material
        Usage instructions in this document and 
        usage examples `here
        <https://gitlab.com/aestheses/insertion_utils/-/tree/master/examples>`_

    Software Module Developed by
        Shrinath Kumar, Zein Jaafar and Donal MacKernan


PI-auto-utility
===============


..  local table of contents
..  contents:: :local:

This module ports the already existing modules :ref:`Particle_Insertion_core` 
and :ref:`Particle_Insertion_hydration` 
from the LAMMPS scripting language to Python. It allows to apply the method described in 
those modules to a larger variety of systems. Additionally it also generalizes the method, 
thereby allowing use of different forcefields. 


Purpose of module
-----------------

This module performs particle insertion/deletion of any type of particle in dilute or dense conditions
in a variety of thermodynamic ensembles via a novel perturbative
approach using `LAMMPS <https://lammps.sandia.gov/>`__ and
`PyLammps <https://lammps.sandia.gov/doc/Python_pylammps.html>`__, a
python interface to LAMMPS. This will be extended to other MD engines such as GROMACS at a later stage.

This type of alchemical insertion and deletion is useful in a whole host of situations, where one would like to compute
the free energy changes associated with adding or removing particle/molecule from a complex. 
Common applications would include:

- Computing the binding energy of ligands to proteins
- Computing the binding energy of protein-protein complexes
- Computing the free energy change associated with increasing or decreasing solvent ( hydration/dehydration )
- Computing the free energy change associated with mixing solvents

The main advantage of this type of alchemical free energy calculation is that it does not use soft-core potential as
many of the approaches to date do. As such, there are less alchemical pathways to compute as the electrostatic and VdW 
interactions can be switched along with all other types of interactions. This results in being able to compute the free 
free energy differences faster with less simulation time. The other main advatange is that due to the mathematical
form of rescaling used, `the singularity of insertion <https://doi.org/10.1080/00268979300102371>`_ can be avoided.

Background Information
----------------------

Please see `PIhydration background information <https://gitlab.e-cam2020.eu/e-cam/E-CAM-Library/-/tree/master/Classical-MD-Modules/modules/PIhydration#id3>`_. 
The only difference in this module is that the functional form of the scaling parameter lambda can be
chosen freely by the user.

Usage instructions
------------------

Prerequisites
~~~~~~~~~~~~~

For this module a custom fork of lammps is required. You can obtain it
from `here <https://gitlab.com/aestheses/lammps>`__. Clone the repository and follow
the standard lammps installation procedure. This fork extends lammp's
python interface to provide some important additional functionality. The
module **will not** work with just the standard lammps installation.

Additionally you also need
`pymbar <https://github.com/choderalab/pymbar>`__ and
`numpy <https://numpy.org/>`__ if you wish to compute
free energies using mbar. You may install this through conda or pip.

.. code:: bash

   # conda
   conda install -c omnia pymbar 
   conda install numpy
   # pip
   pip install pymbar numpy 

Using python with lammps
~~~~~~~~~~~~~~~~~~~~~~~~

A LAMMPS simulation can use python (and this module) in one of two ways:

-  Using python to wrap lammps through the its library interface or
   using one of the provided wrappers. This then allows for a python
   script to create one or more instance of LAMMPS and launch
   simulations.
-  Calling python from a lammps input script using an embedded
   interpreter. For more details see
   `here <https://lammps.sandia.gov/doc/Python_call.html>`__.

This module can be used both ways but when using the embedded
interpreter, care must be taken to ensure that your python script/module
can be found on the search path for imports. The interactive version of
Python will add the current directory to the search path for convenience
but this is not done automatically when embedded.

Using this module
~~~~~~~~~~~~~~~~~

The implementation in this module includes:

-  An ``InsertionManager`` class for encapsulating all the information
   regarding coordinates and topology for the system of particles to be
   inserted or deleted. Instances of this class can be used to store
   templates of molecules. Which can then be used to repeatedly insert
   particles. This class also provides some basic functionality to
   change coordinates and topology of the system to be inserted. It is
   by no means fully comprehensive and it is usually just easier to
   create a new data file if there are drastic changes to be made.
-  Functions ``insert`` and ``delete``, which operate on instances of
   the ``InsertionManager`` class. As their name implies, they perform
   the insertion and deletion of particles into another system.
-  A utility class and function named ``MbarWriter`` and
   ``compute_mbar_fe`` which allow for computing free energies of
   insertion and deletion using MBAR. This uses choderalab's
   `pymbar <https://github.com/choderalab/pymbar>`__ implementation.


Setting up the simulation
^^^^^^^^^^^^^^^^^^^^^^^^^

Before running a simulations you must ensure that your lammps simulation
has allocated enough space for all the types in your existing system
plus the types in the system to be inserted or deleted. If you already
have a data file this is most eaisly done using the
*extras/<interaction_name>/types* argument of the *read_data* command
when you use it for the first time.

**Note**: If the system you are inserting contains interactions that are
not present in the original system you also need to use the
*extras/<interaction_name>/per/atom* argument of the *read_data* command
to leave space for for the number of interactions per atom. Consult the
`read_data <https://lammps.sandia.gov/doc/read_data.html>`__
documentation for more information.

Once you have your system setup you can begin to use this module by
importing the ``pyinsertion`` module in your python code. This contains
the top level classes and functions that perform insertion and deletion
of particles.

Inserting particles
^^^^^^^^^^^^^^^^^^^

To start, you require two files for corresponding to the system of
particles to be inserted.

1. A file that contains the coordinates and topology of of the system.
   This file should be in a format that Lammps's
   `read_data <https://lammps.sandia.gov/doc/read_data.html>`__ command
   can accept. However this file should **not** contain any force-field
   information, such as *pair_coeffcients*, despite the fact that
   including such information is perfectly legal according to the
   *read_data* command.
2. The force-field information for the system to be inserted should be
   in a separate file, the format of which is described below.

   -  Comments begin with the '#' character. They may appear at the
      start of a line or at the end of a line.
   -  The file must contain one or more force-field sections
      corresponding to standard interaction types (like
      pairs,angles,dihedrals,bonds,impropers).
   -  A section begins by enclosing its name in square brackets, like so
      ``[pair]``.
   -  This *must* be immediately followed on the next line by the keyword
      ``style:`` and then the style of the interaction along with any
      global arguments they require. Any valid Lammps interaction style
      can be used expect for the style ``hybrid``. Currently ``hydrid``
      styles are not supported by this module.
   -  The next line *must* contain the keyword ``indices:`` followed by
      a list of integers which correspond to the interaction
      coefficients that will be perturbed beginning with zero. For
      example, ``indices: 1 2`` will instruct the program to scale the
      second and third coefficient but leave the first coefficient
      unchanged. This is useful in situations like bond interaction
      where one would typically like to scale the strength of the bond
      but leave the equilibrium distance unchanged.
   -  Finally, one or more lines of coefficient data in the form
      ``type_id one or more args`` corresponding to the specified style.
      Type ids must begin at with 1.
   -  A section is ended with a single newline.
   -  Sections can be in any order.

An example of a simple force-field file is shown below.

.. code:: bash

   # Final force-field coefficients for inserted particles
   [pair]
   style: lj/cut 1.0 1.0
   indices:    0 1
   #     type       eps        sigma        rcut
           1        1.0        1.0          5
           2        0.5        1.2          5

These two files along with a pointer to an active lammps instance are
required by the constructor of the ``InsertionManager`` class which
which is the main class of interest for insertions. In addition to these
three mandatory arguments, there number of optional arguments you can
specify to the constructor, such as list of $\ ``\lambda``\ $ values to
use for scaling the interaction coefficients. There are many other such
optional arguments. See the code documentation for a full description of
all the parameters.

Once you have an instance of the ``InsertionManager`` class you can
perform the actual insertion by calling it's ``insert`` member function.
This requires two parameters. The length of the relaxation period in
timesteps and the number of samples required from each $\ ``\lambda``\ $
value.

The output from this is by default just the potential energy data at
each $\ ``\lambda``\ $ point but can be changed to include any Lammps
thermo-style variables. This can be achieved by passing a string or a
list of strings to the ``output_style`` keyword of the ``insert``
function. The data is written out to a file named *mbar.dat* by default
but can be changed by using the ``outfile`` keyword of the ``insert``
function. It is written in a format that can be eaisly used with the
`pymbar <https://github.com/choderalab/pymbar>`__ a python
implementation of the multistate Bennett acceptance ratio.

Deleting particles
^^^^^^^^^^^^^^^^^^

Deleting particles is much the same as inserting particles. In fact, the
deletion procedure mathematically is just the inverse of the insertion
approach. The same method detailed above for insertion can be used. The
only change is to specify the $\ ``\lambda``\ $ values in descending
order. Thus, the ``delete`` member function is just a thinly veiled
wrapper of the ``insert`` function with some additional error checking.
It takes the exact same parameters as the ``insert`` function.

Examples
--------
The examples link to a collection of ipython-notebook which go through
some "toy" examples. These attempt to explain the functionality of this
module in a practicle way.

Source Code
-----------

`Module Source Code <https://gitlab.com/aestheses/comparative-metadynamics>`_

However, please note that the source code is currently under embargo until associated works are published, 
if you would like to be obtain a copy of the code, please contact Dr. Donal MacKernan at donal.mackernan@ucd.ie 

.. _pdoc: <https://pdoc3.github.io/pdoc/>
