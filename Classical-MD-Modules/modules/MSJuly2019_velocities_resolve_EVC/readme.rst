..  sidebar:: Software Technical Information

  Name
    velocities_resolve_EVC

  Language
    C

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    Doxygen

  Application Documentation
    `Reference manual <https://gitlab.e-cam2020.eu/carrivain/velocities_resolve_evc/blob/master/refman.pdf>`_

  Relevant Training Material
    `PDF documentation <https://gitlab.e-cam2020.eu/carrivain/velocities_resolve_evc/blob/master/velocities_resolve_EVC.pdf>`_

  Software Module Developed by
    Pascal Carrivain


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _velocities_resolve_EVC:

###################################
E-CAM velocities_resolve_EVC module
###################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

``velocities_resolve_EVC`` is a module that resolves the excluded volume constraint with a velocity formulation (no potential
applied between two overlapped bonds).
``velocities_resolve_EVC`` uses the module :ref:`minDist2segments_KKT` to find the minimal distance between two bonds.

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

To study the long term memory of the initial conformation of a highly entangled polymer we need to preserve the topology.
It means that two bonds cannot cross. It is of great importance for the study of post-mitotic chromosome unfolding.
To resolve the excluded volume constraints you could use a soft or hard potential between the two points associated to the
minimal distance. Here, we propose to change the relative velocity between overlapped bonds to resolve the excluded volume
constraint in one time-step of molecular dynamics.

* Polymer simulation.

* To resolve the excluded volume constraints.

* It is used in a scientific collaboration.

* Publications: not currently available.

.. note::

  We would use the present module to avoid topology violation in an entangled polymer system.
  The present module uses the E-CAM module :ref:`minDist2segments_KKT`.

.. note::

  This module is a part of a pilot project (E-CAM post-doc). We would use it to avoid topology violation in an entangled polymer system.

Background Information
______________________

You can find a PDF file with a detailed derivation of the velocity-based method we use to resolve the excluded volume constraint
in one time-step of molecular dynamics on the `velocities_resolve_EVC GitLab repository <https://gitlab.e-cam2020.eu/carrivain/velocities_resolve_EVC>`_.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

I provide a simple Makefile you can find at the same location as the source code.
You need C++11 in order to use pseudo-random number generator.
Before the compilation you can clean the previous build with "make mrproper" command.
The purpose of the module is to resolve excluded volume constraints.
Therefore, we provide a simple example of a system of ``N`` bonds with volume interactions.
We test every ``n`` iterations the average overlap pairwise.

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

.. Here link the source code *that was created for the module*. If you are using Github or GitLab and the `Gitflow Workflow
   <https://www.atlassian.com/git/tutorials/comparing-workflows#gitflow-workflow>`_ you can point to your feature branch.
   Linking to your pull/merge requests is even better. Otherwise you can link to the explicit commits.

The source code and more information can be find at `velocities_resolve_EVC GitLab repository <https://gitlab.e-cam2020.eu/carrivain/velocities_resolve_EVC>`_.
