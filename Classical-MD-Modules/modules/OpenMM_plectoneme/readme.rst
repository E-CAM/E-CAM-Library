..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    OpenMM_Plectoneme

  Language
    Python 3.7, OpenMM API

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    sphynx

  Application Documentation
    `pydoc3.7 <https://gitlab.e-cam2020.eu:10443/carrivain/plectonemes-with-openmm/blob/master/openmm_plectoneme_functions.html>`_

  Relevant Training Material
    `pdf documentation <https://gitlab.e-cam2020.eu/carrivain/plectonemes-with-openmm/blob/master/openmm_plectoneme.pdf>`_

  Software Module Developed by
    Pascal Carrivain


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _openmm_plectoneme:

##############################
E-CAM openmm_plectoneme module
##############################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

The openmm_plectoneme is a module that introduce twist to a ring plectoneme and sample the accessible conformations under
torsionnal constraints. This module takes advantage of the OpenMM software and GPU acceleration.
It builds a Kremer-Grest polymer model with virtual sites to attach a frame to each of the bead.

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

Bacterial DNA is known to form specific conformations called plectonemes because of internal twisting constraints.
In order to study such a system we need to introduce a linking number deficit in to a circular polymer.
We then tackle the question : does the protocol matter ?
Indeed, does a highly overtwisted ring polymer reach the thermal equilibrium ? Does the memory of initial conformation
matter ?

.. Give a brief overview of why the module is/was being created, explaining a little of the scientific background and how
.. it fits into the larger picture of what you want to achieve. The overview should be comprehensible to a scientist
.. non-expert in the domain area of the software module.

.. This section should also include the following (where appropriate):

* Polymer physicist.

* To understand the conformation of bacterial DNA under torsionnal constraints.

* It is used in a scientific collaboration with Ivan Junier from TIMC-IMAG, Grenoble, France.

* Publications: not currently available.

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

We use the OpenMM toolkit for molecular dynamics. We implemented functionnalities to build a frame and add twisting energy
to a Kremer-Grest polymer system. It is important to study bacterial DNA conformations under twisting constraints.
You can find pdf file with a detailed description `GitLab E-CAM 2020 <https://gitlab.e-cam2020.eu/carrivain/plectonemes-with-openmm>`_.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The module openmm_plectoneme comes with a script example as well as script test (using unittest python module).
In order to test the twist implementation we provide a script that make comparison between the twisting correlations
we measure from our model with the theoretical one.

Source Code
___________

The source code and more informations can be find at `GitLab E-CAM 2020 <https://gitlab.e-cam2020.eu/carrivain/plectonemes-with-openmm>`_.
