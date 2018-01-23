..  In ReStructured Text (ReST) indentation and spacing are very important
    (it is how ReST knows what to do with your document). For ReST to
    understand what you intend and to render it correctly please to keep the
    structure of this template. Make sure that any time you use ReST syntax
    (such as for ".. sidebar::" below), it needs to be preceded and followed
    by white space (if you see warnings when this file is built they this is
    a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to
    wrap around it. This list is a work in progress, please help us improve
    it. We use *definition lists* of ReST_ to make this readable.

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

  Software Module Developed by
    David W.H. Swenson

.. _contact-map-parallelization:

####################
E-CAM example module
####################

..  Let's add a local table of contents to help people navigate the page

.. contents:: :local:

This module adds the ability to parallelize the calculation of contact
frequencies (see the contact-map_ module). It includes improvements to the
core of the ``contact_map`` package to facilitate parallelization, as well
as integration with a framework for practical parallelization.

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

Contacts are defined as when two atoms, or atoms within two groups of atoms
(residues), are within some cutoff distance of each other. The contact map
is the set of all contacts in a given snapshot. The contact frequency is the
fraction of a trajectory in which each pair of contacts is present. The
contact frequency therefore requires calculation of the contact map for each
individual frame in the trajectory.

The original ``contact_map`` code included OpenMP (shared-memory)
parallelization of the calculation of a single contact map (a loop over
atoms). Each contact map in a contact frequency (the loop over the frames of
a trajectory) was done sequentially. However, each frame is completely
independent, and can be processed on a separate node. This module implements
that parallelization.

This module interfaces with the ``dask.distributed`` package for task-based
parallelization. The trajectory is separated into segments, with the
``dask`` network calculating the contact frequency of each segment in
parallel (reading from a common file source).  Then the partial contact
frequencies are combined into one ``ContactFrequency`` object. This also
includes methods, such as serialization into JSON strings, that would be
useful for parallelization by other tools.

.. This module interfaces with several parallelization frameworks. This was
.. done for several reasons. First, it provides options to the user, so that
.. the user may be able to select an already-installed framework. Second, by
.. separating the tasks to be parallelized from the framework being used, it
.. enables future addition of other parallelization frameworks. Finally, it
.. provides the opportunity to benchmark different frameworks on the same
.. problem.

.. The frameworks used are:

.. * **Dask.distributed**:
.. * **PyCOMPs**:
.. * **MDStudio**:

Background Information
______________________

This is part of the `contact map <http://contact-map.readthedocs.io/>`_
package, which in turn builds on tools in `MDTraj <http://mdtraj.org>`_.

The parallelization is based on |dask.distributed|_. See its docs for
details on setting up a dask scheduler/worker network.

.. |dask.distributed| replace:: ``dask.distributed``
.. _dask.distributed: https://distributed.readthedocs.io/

Building and Testing
____________________

The ``contact_map`` package can be installed with conda, using ``conda
install -c conda-forge contact_map``. This module is included in version
``0.3.0``, which can be specifically installed with ``conda install -c
conda-forge contact_map==0.3.0``.

``dask.distibuted`` must be installed separately, which can be done with
``conda install -c conda-forge dask distributed``.

Tests for this module can be run with pytest. Install pytest with ``pip
install pytest`` and then run the command ``py.test`` from within the
directory with the source code, or ``py.test --pyargs contact_map`` from
anywhere after installation. Tests specific to integration with
``dask.distributed`` will be marked as "skipped" if that framework is
not installed.

Source Code
___________

This module is composed of the following pull requests in the
``contact_map`` repository:

* https://github.com/dwhswenson/contact_map/pull/3
* https://github.com/dwhswenson/contact_map/pull/29
* https://github.com/dwhswenson/contact_map/pull/30

