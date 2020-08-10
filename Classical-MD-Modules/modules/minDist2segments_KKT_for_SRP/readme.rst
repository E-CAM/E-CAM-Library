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

The minDist2segments_KKT_for_SRP module returns the minimal distance between two `line segments <https://en.wikipedia.org/wiki/Line_segment>`_.
It uses the Karush-Kuhn-Tucker conditions `(KKT) <https://en.wikipedia.org/wiki/Karush%E2%80%93Kuhn%E2%80%93Tucker_conditions>`_ for the
minimization of distance under constraints.
The module implements the previous function for the `SRP fix in LAMMPS <https://lammps.sandia.gov/doc/pair_srp.html>`_.

Purpose of Module
_________________

To study the long term memory of the initial conformation of a highly entangled polymer we need to preserve the topology.
It means that two polymer bonds cannot cross. It is of great importance for the study of post-mitotic chromosome unfolding.
It also can be use in Dissipative-Particle-Dynamics to prevent bond crossings (see the reference [Sirk2012]_).
To resolve the excluded volume constraints one could use a repulsive potential between the two points associated to the minimal distance.
We propose a new option in the computation of the minimal distance in the `SRP fix <https://lammps.sandia.gov/doc/pair_srp.html>`_ for LAMMPS.

* Polymer simulation.

* To resolve the excluded volume constraints.

* Publications: not currently available.

.. note::

  This module is used by the ongoing work "velocities_resolve_EVC" module implementation for `LAMMPS <https://lammps.sandia.gov>`_.

.. note::

  This module is a part of a `pilot project (E-CAM post-doc) <https://www.e-cam2020.eu/contact-joint-to-resolve-volume-constraints/>`_.
  We would use it to avoid topology violation in an entangled polymer system.

..  If needed you can include latex mathematics like :math:`\frac{ \sum_{t=0}^{N}f(t,k) }{N}`
    which won't show up on GitLab/GitHub but will in final online documentation.

.. [Sirk2012] An enhanced entangled polymer model for dissipative particle dynamics, J. Chem. Phys. 136, 134903 (2012); `<https://doi.org/10.1063/1.3698476>`_.

Background Information
______________________

You can find pdf file with a detailed derivation of the minimal distance between two segments using the Karush-Kuhn-Tucker
conditions on the `minDist2segments_KKT GitLab repository <https://gitlab.com/pcarrivain/mindist2segments_kkt>`_.
The modifications are to an existing code base `SRP fix <https://lammps.sandia.gov/doc/pair_srp.html>`_ for LAMMPS.

Building and Testing
____________________

I provide simple modifications to the `SRP fix <https://lammps.sandia.gov/doc/pair_srp.html>`_ files in the LAMMPS source code (lammps-7Aug19 version).
In order to use minimal distance between two segments with KKT conditions you need to pass **min_KKT** to the **distance** argument from `SRP fix <https://lammps.sandia.gov/doc/pair_srp.html>`_.
The instructions to install, test and run the module can be find on the `minDist2segments_KKT GitLab repository <https://gitlab.com/pcarrivain/mindist2segments_kkt>`_.
The purpose of the module is to calculate the minimal distance between two segments.
For each distance we compare the result to an "exact enumeration" of all the possible
distances and return a warning if the two results differ by more than the enumeration precision.

Source Code
___________

You can find the modifications for the `SRP fix <https://lammps.sandia.gov/doc/pair_srp.html>`_ files on the `minDist2segments_KKT GitLab repository for_SRP folder <https://gitlab.com/pcarrivain/mindist2segments_kkt/for_SRP>`_.
