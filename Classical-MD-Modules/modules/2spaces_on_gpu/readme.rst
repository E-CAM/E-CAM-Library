..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    2spaces_on_gpu

  Language
    C, OpenCL

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    doxygen

  Application Documentation
    'https://gitlab.e-cam2020.eu/carrivain/2spaces_on_gpu/blob/master/refman.pdf'

  Relevant Training Material
    not available yet

  Software Module Developed by
    Pascal Carrivain


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _2spaces_on_gpu:

###########################
E-CAM 2spaces_on_gpu module
###########################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

The 2spaces_on_gpu module implement the 2-spaces algorithm on GPU. This algorithm is designed to move one-half of the polymer
in one Monte-Carlo iteration. It also preserves the excluded volume constraints. We write a OpenCL implementation to be used
on CPUs or GPUs.

.. The E-CAM library is purely a set of documentation that describes software development efforts related to the project. A
.. *module* for E-CAM is the documentation of the single development of effort associated to the project.In that sense, a
.. module does not directly contain source code but instead contains links to source code, typically stored elsewhere. Each
.. module references the source code changes to which it direcctly applies (usually via a URL), and provides detailed
.. information on the relevant *application* for the changes as well as how to build and test the associated software.

.. The original source of this page (:download:`readme.rst`) contains lots of additional comments to help you create your
.. documentation *module* so please use this as a starting point. We use Sphinx_ (which in turn uses ReST_) to create this
.. documentation. You are free to add any level of complexity you wish (within the bounds of what Sphinx_ and ReST_ can
.. do). More general instructions for making your contribution can be found in ":ref:`contributing`".

.. Remember that for a module to be accepted into the E-CAM repository, your source code changes in the target application
.. must pass a number of acceptance criteria:

.. * Style *(use meaningful variable names, no global variables,...)*

.. * Source code documentation *(each function should be documented with each argument explained)*

.. * Tests *(everything you add should have either unit or regression tests)*

.. * Performance *(If what you introduce has a significant computational load you should make some performance optimisation
   effort using an appropriate tool. You should be able to verify that your changes have not introduced unexpected
   performance penalties, are threadsafe if needed,...)*

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

Polymer of size L is supposed to equilibrate after a time like :math:`L^3`. Therefore, tt could be difficult to study the
equilibrium properties of large polymers. The 2-spaces algorithm already improves the efficient of each Monte-Carlo by
moving half of the polymer. We can use the GPU units to take care one of the sub-move amongst the Monte-Carlo step.

.. Give a brief overview of why the module is/was being created, explaining a little of the scientific background and how
.. it fits into the larger picture of what you want to achieve. The overview should be comprehensible to a scientist
.. non-expert in the domain area of the software module.

.. This section should also include the following (where appropriate):

* Polymer simulation.

* To equilibrate large polymer.

* It is used in a scientific collaboration with C. Vaillant and D. Jost (ENS Lyon).

* Publications: not currently available.

.. note::

  We would use the present module to equilibrate large polymer with computation power of GPUs.

.. note::

  This module does not belong to a pilot project (E-CAM post-doc).

..
   If needed you can include latex mathematics like
   :math:`\frac{ \sum_{t=0}^{N}f(t,k) }{N}`
   which won't show up on GitLab/GitHub but will in final online documentation.

   If you want to add a citation, such as [CIT2009]_, please check the source code to see how this is done. Note that
   citations may get rearranged, e.g., to the bottom of the "page".

   .. [CIT2009] This is a citation (as often used in journals).

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

.. If the modifications are to an existing code base (which is typical) then this would be the place to name that
.. application. List any relevant urls and explain how to get access to that code. There needs to be enough information
.. here so that the person reading knows where to get the source code for the application, what version this information is
.. relevant for, whether this requires any additional patches/plugins, etc.

.. Overall, this module is supposed to be self-contained, but linking to specific URLs with more detailed information is
.. encouraged. In other words, the reader should not need to do a websearch to understand the context of this module, all
.. the links they need should be already in this module.

Please consider reading the two research articles `Massively Parallel Architectures and Polymer Simulation <https://www.semanticscholar.org/paper/Massively-Parallel-Architectures-and-Polymer-Ostrovsky-Smith/f79694076e40eca0fae9b35a381e43b7abfa029c>`_
and `Cellular automata for polymer simulation with application to polymer melts and polymer collapse including implications for protein folding <https://www.sciencedirect.com/science/article/pii/S0167819100000818>`_ for details about the method.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

I provide a simple make file as well as OpenCL kernel and main source code to run the model.
You need C++11 in order to use pseudo-random number generator.
Before the compilation you can clean the previous build with "make mrproper" command.

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

.. Here link the source code *that was created for the module*. If you are using Github or GitLab and the `Gitflow Workflow
   <https://www.atlassian.com/git/tutorials/comparing-workflows#gitflow-workflow>`_ you can point to your feature branch.
   Linking to your pull/merge requests is even better. Otherwise you can link to the explicit commits.

The source code and more informations can be find on the `2spaces_on_gpu GitLab repository <https://gitlab.e-cam2020.eu/carrivain/2spaces_on_gpu>`_.
