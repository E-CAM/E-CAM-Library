
..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

.. sidebar:: Software Technical Information

  This module extends the contact_maps project.

  Name
    contact_maps

  Language
    Python 2.7, 3.5, 3.6

  Licence
    LGPL 2.1+

  Documentation Tool
    Sphinx/RST

  Application Documentation
    http://contact-map.readthedocs.io/

  Relevant Training Material
    TODO

  Software Module Developed by
    David W.H. Swenson


.. _contact_concurrences:

####################
Contact Concurrences
####################

.. Let's add a local table of contents to help people navigate the page

.. contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that
    explains the "helicopter view" of why you are creating this module. For
    example, you might say that "This module is a stepping stone to
    incorporating XXXX effects into YYYY process, which in turn should allow
    ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

This module deals with the analysis of contacts between protein residues
based on "contact concurrences," i.e., what contacts occur simultaneously
during a trajectory.  This is useful when using contacts as a definition of
a metastable state in a trajectory.

Purpose of Module
_________________

Contact frequencies, as developed in the module :ref:`contact-map`, are a
useful tool for studying proteins. However, they suffer from one problem
when 

For example, a particular contact pair might have a frequency of 0.1 during
a 100ns trajectory. But this could be achieved in several ways. If the
contact is randomly distributed through time, this contact probably isn't
characteristic of a metastable state. On the other hand, if is it constantly
present during the last 10 ns (and not otherwise present), it might
represent a metastable state. More important, there might be multiple
contacts that are *all* present during those last 10 ns. This module helps
identify and analyze those concurrent contacts by providing a tool to
visualize such concurrences.

.. TODO: example of that visualization

This is an important tool for identifying stable states based on long-lived
groups of contacts, and is being used as part of the `E-CAM pilot project on
binding kinetics <https://www.e-cam2020.eu/pilot-project-biki/>`_. It has
also been used a part of a bachelor's thesis project to develop an automated
approach to identifying metastable intermediates during binding/unbinding
processes.

Classes implemented in this module include:

* ``Concurrence``: Superclass for contact concurrence objects, enabling
  future custom concurrence types.
* ``AtomContactConcurrence``: Contact concurrences for atom-atom contacts.
* ``ResidueContactConcurrence``: Contact concurrences for residue-residue
  contacts (based on minimum distance between constituent atoms).
* ``ConcurrencePlotter`` and ``plot_concurrences``: Class and convenience
  function (respectively) for making plots of contact concurrence.
* ``ContactsDict``: Dict-like object giving access to atom or residue
  contacts based on string keys. Also added ``ContactObject.contacts``
  property, which returns a ``ContactsDict`` object for the
  ``ContactObject``.

.. * Who will use the module? in what area(s) and in what context?

.. * What kind of problems can be solved by the code?

.. * Are there any real-world applications for it?

.. * Has the module been interfaced with other packages?

.. * Was it used in a thesis, a scientific collaboration, or was it cited in
..   a publication?

.. * If there are published results obtained using this code, describe them
     briefly in terms readable for non-expert users.  If you have few
     pictures/graphs illustrating the power or utility of the module, please
     include them with corresponding explanatory captions.


Background Information
______________________

This module is part of the `contact_map
<http://contact-map.readthedocs.io>`_ project, which builds on tools from
`MDTraj <http://mdtraj.org>`_.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module will be included in the 0.4 release of ``contact_map``. After
that release, it can be easily installed with ``conda``, using ``conda
install -c conda-forge contact_map``, or ``conda install -c conda-forge
contact_map==0.4.0`` for the first version that includes this module. To see
the current release, go to https://pypi.org/project/contact-map/#history.

Until the release, this module can only be installed through a developer
install of ``contact_map``. This involves downloading the ``contact_map``
repository, installing the requirements, and then installing the
``contact_map`` package from source. Instructions can be found on the
`installation page
<http://contact-map.readthedocs.io/en/latest/installing.html#developer-installation>`_
of the ``contact_map`` documentation.


Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

The source code for this module is contained in the following pull request
in the ``contact_map`` repository:

* https://github.com/dwhswenson/contact_map/pull/28

