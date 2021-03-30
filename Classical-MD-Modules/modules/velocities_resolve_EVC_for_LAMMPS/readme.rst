..  sidebar:: Software Technical Information

  Name
    velocities_resolve_EVC_for_LAMMPS

  Language
    C/C++, LAMMPS

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    doxygen

  Application Documentation
    'https://gitlab.com/pcarrivain/velocities_resolve_evc/-/blob/master/refman.pdf'

  Relevant Training Material
    'https://gitlab.com/pcarrivain/velocities_resolve_evc/-/blob/master/velocities_resolve_EVC.pdf'

  Software Module Developed by
    Pascal Carrivain


.. _velocities_resolve_EVC_for_LAMMPS:

##############################################
E-CAM velocities_resolve_EVC_for_LAMMPS module
##############################################

..  contents:: :local:

The **velocities_resolve_EVC_for_LAMMPS** is a module
that resolve the excluded volume constraint
with a velocity formulation (no potential applied between two bonds).
It is an implementation for
`LAMMPS <https://lammps.sandia.gov>`_
of an already existing module
`velocities_resolve_EVC GitLab repository <https://gitlab.com/pcarrivain/velocities_resolve_evc>`_.
The **velocities_resolve_EVC_for_LAMMPS** uses
the module **minDist2segments_KKT_for_SRP**
(you can find on the
`minDist2segments_KKT_for_SRP GitLab repository <https://gitlab.com/pcarrivain/mindist2segments_kkt>`_)
to find the minimal distance between two bonds.

Purpose of Module
_________________

To study the long term memory of the initial conformation
of a highly entangled polymer we need to preserve the topology.
It means that two bonds cannot cross.
It is of great importance for the study of
post-mitotic chromosome unfolding.
Preservation of topology is also used in the framework of
`Dissipative-Particle-Dynamics <https://en.wikipedia.org/wiki/Dissipative_particle_dynamics>`_
in particular for the study of rheological properties.
To resolve the excluded volume constraints one could
use a soft or hard potential between the two points
(each point belong to one of the two overlapping bonds)
associated to the minimal distance.
Here, we propose to change the relative velocity between
overlapped bonds to resolve the excluded volume
constraint in one time-step of molecular dynamics.
We propose to implement this functionality
as a new fix for `LAMMPS <https://lammps.sandia.gov>`_.

* It is used in a scientific collaboration.

* Publications: not currently available.

.. note::

  The present module uses the E-CAM module
  **minDist2segments_KKT_for_SRP** you can find
  on the
  `minDist2segments_KKT GitLab repository <https://gitlab.com/pcarrivain/mindist2segments_kkt>`_.
  It also uses the E-CAM module **velocities_resolve_EVC**
  you can find on the
  `velocities_resolve_EVC GitLab repository <https://gitlab.com/pcarrivain/velocities_resolve_evc>`_.
  This module is a part of a E-CAM post-doc
  `pilot project <https://www.e-cam2020.eu/contact-joint-to-resolve-volume-constraints/>`_.

Background Information
______________________

You can find a pdf file with a detailed derivation
of the velocity-based method
we use to resolve the excluded volume constraint
in one time-step of molecular dynamics on the
`velocities_resolve_EVC GitLab repository <https://gitlab.com/pcarrivain/velocities_resolve_evc>`_.

Building and Testing
____________________

The instruction to build and run test are
available on the GitLab repository.
The purpose of the module is to resolve
excluded volume constraints for polymer system.
Therefore, we provide a simple
`LAMMPS <https://lammps.sandia.gov>`_ input file
of a system of C chains of N bonds each
with volume interactions.
In particular, we use the
`LAMMPS <https://lammps.sandia.gov>`_
implementation of
`FENE bond <https://en.wikipedia.org/wiki/FENE>`_.
The algorithm we propose here checks every
time-step the maximal overlap and exit if
it exceeds a threshold you gave.
It also compute a specific quantity to determine
if two bonds cross during one time-step.

Source Code
___________

The source code and more details can be find on the
`velocities_resolve_EVC GitLab repository <https://gitlab.com/pcarrivain/velocities_resolve_evc>`_.
