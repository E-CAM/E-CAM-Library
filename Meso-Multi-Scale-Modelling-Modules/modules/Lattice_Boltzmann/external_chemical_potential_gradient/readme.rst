..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    Ludwig: A lattice Boltzmann code for complex fluids

  Language
    C

  Licence
    `<https://github.com/ludwig-cf/ludwig/blob/master/LICENSE>`_

  Documentation Tool
    LaTex-generated pdf

  Application Documentation
    `<https://github.com/ludwig-cf/ludwig/tree/master/docs/tutorial>`_

..  Relevant Training Material
    Add a link to any relevant training material. If there currently is none then say 'Not currently available.'

..  Software Module Developed by
    Add the name of the person who developed the software for this module here


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. .. _example:

########################################################
Externally imposed chemical potential gradient in Ludwig
########################################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

The E-CAM library is purely a set of documentation that describes software development efforts related to the project. A
*module* for E-CAM is the documentation of the single development of effort associated to the project.In that sense, a
module does not directly contain source code but instead contains links to source code, typically stored elsewhere. Each
module references the source code changes to which it directly applies (usually via a URL), and provides detailed
information on the relevant *application* for the changes as well as how to build and test the associated software.

.. The original source of this page (:download:`readme.rst`) contains lots of additional comments to help you create your
.. documentation *module* so please use this as a starting point. We use Sphinx_ (which in turn uses ReST_) to create this
.. documentation. You are free to add any level of complexity you wish (within the bounds of what Sphinx_ and ReST_ can
.. do). More general instructions for making your contribution can be found in ":ref:`contributing`".

Remember that for a module to be accepted into the E-CAM repository, your source code changes in the target application
must pass a number of acceptance criteria:

* Style *(use meaningful variable names, no global variables,...)*

* Source code documentation *(each function should be documented with each argument explained)*

* Tests *(everything you add should have either unit or regression tests)*

* Performance *(If what you introduce has a significant computational load you should make some performance optimisation
  effort using an appropriate tool. You should be able to verify that your changes have not introduced unexpected
  performance penalties, are threadsafe if needed,...)*

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The gradient in chemical potential in a binary fluid mixture gives rise to a flow, whenever the value of the order parameter
differs from 0. In Ludwig, we first implement the gradient as a vectorial physical property, which can, similarly to all
other physical properties, be used anywhere in the code. Further on, we use it in the context of binary fluid
mixture in the subroutines that account for the time evolution of the order parameter (Cahn-Hilliard equation) and the force,
that arises due to the non-zero chemical potential gradient and order parameter. This module is essential for studying binary
fluid flows in porous materials as well as flows, arising due to the wetting effect of the walls in nanochannels.

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module implements the externally imposed chemical potential gradient (for binary fluid mixture) in the Ludwig code.
The latter, together with its documentation and tutorial is availiable on the following link: `<https://github.com/ludwig-cf/ludwig>`_.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

Provide the build information for the module here and explain how tests are run. This needs to be adequately detailed,
explaining if necessary any deviations from the normal build procedure of the application (and links to information
about the normal build process needs to be provided).


Source code
___________

The module has been provided as a pull request on the github repository of Ludwig, availiable at: `<https://github.com/ludwig-cf/ludwig/pull/80>`_.


