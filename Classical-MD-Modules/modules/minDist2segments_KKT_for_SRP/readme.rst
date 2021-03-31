:orphan:
   
..  sidebar:: Software Technical Information

  Name
    minDist2segments_KKT_for_SRP

  Language
    C/C++, LAMMPS

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    sphynx

  Application Documentation
    `doxygen documentation <https://gitlab.com/pcarrivain/mindist2segments_kkt/-/tree/master/latex/refman.pdf>`_

  Relevant Training Material
    `pdf documentation <https://gitlab.com/pcarrivain/mindist2segments_kkt/-/tree/master/minDist2segments_KKT.pdf>`_

  Software Module Developed by
    Pascal Carrivain


.. _minDist2segments_KKT_for_SRP:

#########################################
E-CAM minDist2segments_KKT_for_SRP module
#########################################

..  contents:: :local:

The minDist2segments_KKT_for_SRP module returns
the minimal distance between two
`line segments <https://en.wikipedia.org/wiki/Line_segment>`_.
It uses the Karush-Kuhn-Tucker conditions
`(KKT) <https://en.wikipedia.org/wiki/Karush%E2%80%93Kuhn%E2%80%93Tucker_conditions>`_
for the minimization of distance under constraints.
The module implements the previous function
for the
`SRP fix in LAMMPS <https://lammps.sandia.gov/doc/pair_srp.html>`_.
Indeed, the SRP function to compute the minimal distance
does not always give the correct solution.

Purpose of Module
_________________

To study the long term memory of the initial conformation
of a highly entangled polymer we need to preserve the topology.
That means that two polymer bonds cannot cross.
It is of great importance
for the study of post-mitotic chromosome unfolding.
Minimal distance between two bonds can be used in
`Dissipative-Particle-Dynamics <https://en.wikipedia.org/wiki/Dissipative_particle_dynamics>`_
to prevent bond
crossings (see the reference [Kumar2001]_ and [Sirk2012]_) too.
To resolve the excluded volume constraints one could use a
repulsive potential between the
two points associated to the minimal distance
(see the reference [Kumar2001]_).
We propose a new option in the computation of the minimal distance
in the
`SRP fix <https://lammps.sandia.gov/doc/pair_srp.html>`_
for LAMMPS.
Indeed, SRP fix computes the minimal distance between
two infinite lines and reset the solution to
occur along the interior of the bond.
This method is not always accurate.
The KKT conditions allows to solve the problem of
minimal distance such finite segment length constraint holds.

.. note::

  It is part of
  `E-CAM post-doc pilot project <https://www.e-cam2020.eu/contact-joint-to-resolve-volume-constraints/>`_.

.. [Sirk2012] An enhanced entangled polymer model for dissipative particle dynamics,
              J. Chem. Phys. 136, 134903 (2012); `<https://doi.org/10.1063/1.3698476>`_
.. [Kumar2001] Brownian dynamics simulations of flexible polymers with spring–spring repulsions,
              J. Chem. Phys. 114, 6937, (2001); `<https://doi.org/10.1063/1.1358860>`_

Background Information
______________________

You can find pdf file with a detailed derivation of the minimal distance
between two segments using the Karush-Kuhn-Tucker conditions on the
`minDist2segments_KKT GitLab repository <https://gitlab.com/pcarrivain/mindist2segments_kkt>`_.
The modifications are to an existing code base
`SRP fix <https://lammps.sandia.gov/doc/pair_srp.html>`_
for LAMMPS.

Building and Testing
____________________

I provide simple modifications to the
`SRP fix <https://lammps.sandia.gov/doc/pair_srp.html>`_
files in the LAMMPS source code.
In order to use minimal distance between two segments
with KKT conditions you need
to pass **min_KKT** to the **distance** argument of the
`SRP fix <https://lammps.sandia.gov/doc/pair_srp.html>`_.
The instructions to install, test and run the module
can be found on the
`minDist2segments_KKT GitLab repository <https://gitlab.com/pcarrivain/mindist2segments_kkt>`_.
The purpose of the module is to calculate the minimal
distance between two segments.
For each distance we compare the result to an
"exact enumeration" of all the possible
distances and return a warning if the two results
differ by more than the enumeration precision.

Source Code
___________

You can find the modifications for the
`SRP fix <https://lammps.sandia.gov/doc/pair_srp.html>`_
files on the
`minDist2segments_KKT GitLab repository for_SRP folder <https://gitlab.com/pcarrivain/mindist2segments_kkt/-/tree/master/for_SRP>`_.
