
..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

.. sidebar:: Software Technical Information

  This module includes contributions to two code packages, binding_md and
  contact_maps. 

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

TODO: Add background. Main contributions in this module:

* methods for the user designate different parts of the ligand (analogous to
  residues in a biomolecule; frequently a the contacts with specific parts
  of small molecule/potential will be important.)
* tools for identifying which contacts exist at the same times (are
  correlated) within a trajectory


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

.. note::

  If the module is an ingredient for a more general workflow (e.g. the
  module was the necessary foundation for later code; the module is part of
  a group of modules that will be used to calculate certain property or have
  certain application, etc.) mention this, and point to the place where you
  specify the applications of the more general workflow (that could be in
  another module, in another section of this repository, an application’s
  website, etc.).

.. note::

  If you are a post-docs who works in E-CAM, an obvious application for the
  module (or for the group of modules that this one is part of) is your
  pilot project. In this case, you could point for the pilot project page on
  the main website (and you must ensure that this module is linked there).


Background Information
______________________



Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module will be included in the 0.4 release of ``contact_map``. After
that release, it can be easily installed with ``conda``, using ``conda
install -c conda-forge contact_map``, or ``conda install -c conda-forge
contact_map==0.4.0`` for the first version that includes this module.

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

