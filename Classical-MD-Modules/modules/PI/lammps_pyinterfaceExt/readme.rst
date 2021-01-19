:orphan:

..  sidebar:: Software Technical Information
   
   This module is extends the lammps python interface to allow accessing 
   various force-field potential parameters from python.

    Language 
        Python (3+)
    
    Licence
        The software for this specific module is licensed under `GNU General Lesser Public License v3.0
        <https://opensource.org/licenses/LGPL-3.0>`_

    Documentation Tool
        `Sphinx <http://www.sphinx-doc.org/en/stable/markup/index.html>`_, follows LAMMPS format (ReST)    
    
    Application Documentation
        `Documentation <https://gitlab.com/aestheses/lammps_patches/-/tree/master/docs>`_

    Relevant Training Material
        Not currently available.

    Software Module Developed by
        Shrinath Kumar, Zein Jaafar and Donal MacKernan

.. _lammps_pyinterfaceext:

#####################
LAMMPS-pyinterfaceExt
#####################

..  local table of contents
..  contents:: :local:



The module contains patch files for the `Stable release 29 October 2020`_ version of LAMMPS, to 
enable accessing simulation force-filed parameters from python.

Purpose of Module
_________________

When performing alchemical free energy calculations, it is necessary to change the attributes of 
various particles in a simulations - Atom properties such as charge and mass or Force field 
properties such as :math:`\epsilon` or :math:`\sigma` of Lennard-Jones potentials. 

The LAMMPS library along with its Python interface provides the ability to directly access and 
change many such attributes in a running simulation. However, for pair potentials while it is 
possible to change their parameters it is currently not possible to read the existing parameters
from a running simulations. This is required for the alchemical free energy calculations using the
Particle Insertion approach as described in the :ref:`Particle_Insertion_core` module, where the
scaling of the forcefield attribute depends on the existing attributes.

This module address this limitation by extending the LAMMPS library and the Python interface to add 
a function allowing read access to pair-potential parameters. It also adds an extract method to the 
the ``pair_*.cpp`` files associated with some commonly used pair-potential in LAMMPS as described in 
`fix adapt`_.

Background Information
----------------------
LAMMPS (Large-scale Atomic/Molecular Massively Parallel Simulator) is a very versatile MD engine. 
The current stable release of which can be obtained from the link `LAMMPS stable 29Oct2020`_.
In particular, due to it's powerful 
library and python interface it allows great control and easy scripting of any time of simulation

The Particle Insertion approach for alchemical free energy calculations is 
currently only implemented using the LAMMPS MD engine. The set of patches in this module exist to 
accommodate the requirements of those modules.

Building and Testing
--------------------
Download and extract the LAMMPS source. Before building LAMMPS download the patch file and apply 
the patch using the :code:`patch` command from the root of the LAMMPS source directory.

.. code-block:: bash

  patch < lmp_pyExt.patch

Follow the normal LAMMPS building instructions at `https://lammps.sandia.gov/doc/Build.html 
<https://lammps.sandia.gov/doc/Build.html>`_ to build and install LAMMPS along with the applied 
patch. Make sure to enable ``pkg_python`` when building. 


Source Code
___________

Available as a patch file from `here <https://gitlab.com/aestheses/lammps_patches/-/tree/master/>`_

.. _Stable release 29 October 2020: https://github.com/lammps/lammps/releases/tag/stable_29Oct2020
.. _LAMMPS stable 29Oct2020: https://github.com/lammps/lammps/releases/tag/stable_29Oct2020
.. _fix adapt: https://lammps.sandia.gov/doc/fix_adapt_fep.html
