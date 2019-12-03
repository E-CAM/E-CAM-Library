:orphan:

..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    minDist2segments_KKT

  Language
    C++

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    Sphinx

  Application Documentation
    `Doxygen documentation <https://gitlab.e-cam2020.eu/carrivain/mindist2segments_kkt/blob/master/refman.pdf>`_

  Relevant Training Material
    `PDF documentation <https://gitlab.e-cam2020.eu/carrivain/mindist2segments_kkt/blob/master/minDist2segments_KKT.pdf>`_

  Software Module Developed by
    Pascal Carrivain


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _minDist2segments_KKT:

#################################
E-CAM minDist2segments_KKT module
#################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

The minDist2segments_KKT module returns the minimal distance between two `line segments <https://en.wikipedia.org/wiki/Line_segment>`_.
It uses the Karush-Kuhn-Tucker conditions `(KKT) <https://en.wikipedia.org/wiki/Karush%E2%80%93Kuhn%E2%80%93Tucker_conditions>`_
for the minimization under constraints.

..  The E-CAM library is purely a set of documentation that describes software development efforts related to the project.
    A *module* for E-CAM is the documentation of the single development of effort associated to the project. In that sense, a
    module does not directly contain source code but instead contains links to source code, typically stored elsewhere. Each
    module references the source code changes to which it direcctly applies (usually via a URL), and provides detailed
    information on the relevant *application* for the changes as well as how to build and test the associated software.

..  The original source of this page (:download:`readme.rst`) contains lots of additional comments to help you create your
    documentation *module* so please use this as a starting point. We use Sphinx_ (which in turn uses ReST_) to create this
    documentation. You are free to add any level of complexity you wish (within the bounds of what Sphinx_ and ReST_ can
    do). More general instructions for making your contribution can be found in ":ref:`contributing`".

..  Remember that for a module to be accepted into the E-CAM repository, your source code changes in the target application
    must pass a number of acceptance criteria:

.. * Style *(use meaningful variable names, no global variables,...)*

.. * Source code documentation *(each function should be documented with each argument explained)*

.. * Tests *(everything you add should have either unit or regression tests)*

.. * Performance *(If what you introduce has a significant computational load you should make some performance optimisation
   effort using an appropriate tool. You should be able to verify that your changes have not introduced unexpected
   performance penalties, are threadsafe if needed,...)*

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

To study the long term memory of the initial conformation of a highly entangled polymer we need to preserve the topology.
It means that two polymer bonds cannot cross. It is of great importance for the study of post-mitotic chromosome unfolding.
To resolve the excluded volume constraints one could use a soft or hard potential between the two points associated to the
minimal distance.

..  Give a brief overview of why the module is/was being created, explaining a little of the scientific background and how
    it fits into the larger picture of what you want to achieve. The overview should be comprehensible to a scientist
    non-expert in the domain area of the software module.

..  This section should also include the following (where appropriate):

* Polymer simulation.

* To resolve the excluded volume constraints.

* It is used in a scientific collaborations.

* Publications: not currently available.

.. note::

  We would use the present module to avoid topology violation in an entangled polymer system.
  This module is used by the ongoing work "velocities_resolve_EVC" module.

.. note::

  This module is a part of a pilot project (E-CAM post-doc). We would use it to avoid topology violation in entangle polymer system.
  url to the pilot project main page not currently available.

..  If needed you can include latex mathematics like :math:`\frac{ \sum_{t=0}^{N}f(t,k) }{N}`
    which won't show up on GitLab/GitHub but will in final online documentation.

..  If you want to add a citation, such as [CIT2009]_, please check the source code to see how this is done. Note that
    citations may get rearranged, e.g., to the bottom of the "page".

..  .. [CIT2009] This is a citation (as often used in journals).

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

..  If the modifications are to an existing code base (which is typical) then this would be the place to name that
    application. List any relevant urls and explain how to get access to that code. There needs to be enough information
    here so that the person reading knows where to get the source code for the application, what version this information is
    relevant for, whether this requires any additional patches/plugins, etc.

..  Overall, this module is supposed to be self-contained, but linking to specific URLs with more detailed information is
    encouraged. In other words, the reader should not need to do a websearch to understand the context of this module, all
    the links they need should be already in this module.

You can find pdf file with a detailed derivation of the minimal distance between two segments using the Karush-Kuhn-Tucker
conditions on the `minDist2segments_KKT GitLab repository <https://gitlab.e-cam2020.eu/carrivain/mindist2segments_kkt>`_.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

I provide a simple Makefile you can find at the same location that the source code.
<<<<<<< HEAD
You need C++11 in order to use pseudo-random number generator.
It has OpenMP acceleration. Edit the make file to enable it.
=======
You need C++11 in order to use the pseudo-random number generator.
>>>>>>> 3dae205631a9670376742991d107654cf5145fb9
Before the compilation you can clean the previous build with "make mrproper" command.
The purpose of the module is to calculate the minimal distance between two segments.
For each distance we compare the result to an "exact enumeration" of all the possible
distances and return a warning if the two results differ by more than the enumeration
precision.

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

.. Here link the source code *that was created for the module*. If you are using Github or GitLab and the `Gitflow Workflow
   <https://www.atlassian.com/git/tutorials/comparing-workflows#gitflow-workflow>`_ you can point to your feature branch.
   Linking to your pull/merge requests is even better. Otherwise you can link to the explicit commits.

<<<<<<< HEAD
The source code and more informations can be find at `minDist2segments_KKT GitLab repository <https://gitlab.e-cam2020.eu/carrivain/mindist2segments_kkt>`_.
=======
The source code and more information can be find at `GitLab E-CAM 2020 <https://gitlab.e-cam2020.eu/carrivain/mindist2segments_kkt>`_.

>>>>>>> 3dae205631a9670376742991d107654cf5145fb9
