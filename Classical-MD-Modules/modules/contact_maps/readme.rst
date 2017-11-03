.. _contact-map:

###########
Contact Map
###########

.. sidebar:: Software Technical Information

  Language
    Python (2.7, 3.4, 3.5, 3.6)

  Licence
    LGPL 2.1 or later

  Documentation Tool
    Sphinx/RST

  Application Documentation
    http://contact-map.readthedocs.io/

  Relevant Training Material
    http://contact-map.readthedocs.io/en/latest/examples.html

.. contents:: :local:

Frequently, we characterize states (especially in biomolecular system) in
terms of the contacts between specific residues or atoms. When trying to
identify the specific contacts of interest, it can be useful to look at all
the contacts. This module provides tools for mapping and identifying
contacts in trajectories.

Purpose of Module
_________________

Contacts can be an important tool for defining (meta)stable states in
processes involving biomolecules. For example, an analysis of contacts can
be particularly useful when defining bound states during a binding processes
between proteins, DNA, and small molecules (such as potential drugs).

The contacts analyzed by contact_map can be either intermolecular or
intramolecular, and can be analyzed on a residue-residue basis or an
atom-atom basis.

This package makes it very easy to answer questions like:

* What contacts are present in a trajectory?
* Which contacts are most common in a trajectory?
* What is the difference between the frequency of contacts in one trajectory
  and another? (Or with a specific frame, such as a PDB entry.)
* For a particular residue-residue contact pair of interest, which atoms are
  most frequently in contact?

It also facilitates visualization of the contact matrix, with colors
representing the fraction of trajectory time that the contact was present.

Background Information
______________________

This is an independent module, but it builds on tools developed in `MDTraj
<http://mdtraj.org>`_.

Testing
_______

This module can be installed with conda, using ``conda install -c
conda-forge contact_map``. To intall the specific version associated with
this module, use ``conda install -c conda-forge contact_map==0.2.0``

Tests for this module can be run with pytest. Install pytest with ``pip
install pytest`` and then run the command ``py.test`` from within the
directory with the source code, or ``py.test --pyargs contact_map`` from
anywhere after installation.

Source Code
___________

The source code for this module, and modules that build on it, is hosted at
https://github.com/dwhswenson/contact_map. This module specifically includes
everything up to and including `release 0.2 <https://github.com/dwhswenson/contact_map/releases/tag/v0.2.0>`_.
