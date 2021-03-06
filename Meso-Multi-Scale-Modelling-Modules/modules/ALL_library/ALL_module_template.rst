..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

:orphan:

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    A Load Balancing Library (ALL)

  Language
    C++, Fortran interfaces available

  Licence
    `BSD 3-Clause <https://choosealicense.com/licenses/bsd-3-clause/>`_

  Documentation Tool
    No tool used in source code, repo documentation written in `Markdown <https://en.wikipedia.org/wiki/Markdown>`_

  Application Documentation
    See `ALL repository <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing>`_

  Relevant Training Material
    None available

  Software Module Developed by
    Rene Halver


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`ALL_example`"). You *MUST* change the reference below from "ALL_method_example"
    to something unique otherwise you will cause cross-referencing errors. The reference must come right before the
    heading for the reference to work (so don't insert a comment between).

.. _ALL_method_example:

###############################
E-CAM example ALL method module
###############################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why this
    module was are created.

The A Load Balancing Library (ALL) library aims to provide an easy way to include dynamic domain-based load balancing
into particle based simulation codes. The library is developed in the Simulation Laboratory Molecular Systems of the
Juelich Supercomputing Centre at Forschungszentrum Juelich.

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module provides an additional method to the ALL library, up-to-date descriptions of the methods in the library can
be found in the `ALL README file <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/blob/master/README.md>`_.

**Take the method description from the README and reproduce it here**

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

See :ref:`ALL_background` for details.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

ALL uses the `CMake <https://cmake.org/runningcmake/>`_ build system, specific build and installation requirements can
be found in the `ALL README file <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/blob/master/README.md>`_.

**Need to provide information on how to test the particular method here.**

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

**Here link the source code *that is relevant for the module*. If you are using Github or GitLab and the `Gitflow
Workflow <https://www.atlassian.com/git/tutorials/comparing-workflows#gitflow-workflow>`_ you can point to your feature
branch. Linking to your pull/merge requests is even better. Otherwise you can link to the explicit commits or locations
in the source code.**

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html
