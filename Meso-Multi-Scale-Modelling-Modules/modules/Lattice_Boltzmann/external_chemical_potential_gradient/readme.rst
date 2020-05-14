..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

.. :orphan:

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
..    All source code created for this module should be documented so please indicate what tool has been used for
..    documentation. Doxygen covers  most languages but for Fortran you might want to use
..    `Ford <http://fortranwiki.org/fortran/show/FORD>`_, for Python ReST_, etc.

  Application Documentation
    Provide a link to any documentation for the application.

  Relevant Training Material
    Add a link to any relevant training material. If there currently is none then say 'Not currently available.'

  Software Module Developed by
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

Give a brief overview of why the module is/was being created, explaining a little of the scientific background and how
it fits into the larger picture of what you want to achieve. The overview should be comprehensible to a scientist
non-expert in the domain area of the software module.

This section should also include the following (where appropriate):

* Who will use the module? in what area(s) and in what context?

* What kind of problems can be solved by the code?

* Are there any real-world applications for it?

* Has the module been interfaced with other packages?

* Was it used in a thesis, a scientific collaboration, or was it cited in a publication?

* If there are published results obtained using this code, describe them briefly in terms readable for non-expert users.
  If you have few pictures/graphs illustrating the power or utility of the module, please include them with
  corresponding explanatory captions.

.. note::

  If the module is an ingredient for a more general workflow (e.g. the module was the necessary foundation for later
  code; the module is part of a group of modules that will be used to calculate certain property or have certain
  application, etc.) mention this, and point to the place where you specify the applications of the more general
  workflow (that could be in another module, in another section of this repository, an application’s website, etc.).

.. note::

  If you are a post-doc who works in E-CAM, an obvious application for the module (or for the group of modules that
  this one is part of) is your pilot project. In this case, you could point to the pilot project page on the main
  website (and you must ensure that this module is linked there).

If needed you can include latex mathematics like
:math:`\frac{ \sum_{t=0}^{N}f(t,k) }{N}`
which won't show up on GitLab/GitHub but will in final online documentation.

..  If you want to add a citation, such as [CIT2009]_, please check the source code to see how this is done. Note that
..  citations may get rearranged, e.g., to the bottom of the "page".

..  .. [CIT2009] This is a citation (as often used in journals).

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

If the modifications are to an existing code base (which is typical) then this would be the place to name that
application. List any relevant urls and explain how to get access to that code. There needs to be enough information
here so that the person reading knows where to get the source code for the application, what version this information is
relevant for, whether this requires any additional patches/plugins, etc.

Overall, this module is supposed to be self-contained, but linking to specific URLs with more detailed information is
encouraged. In other words, the reader should not need to do a websearch to understand the context of this module, all
the links they need should be already in this module.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

Provide the build information for the module here and explain how tests are run. This needs to be adequately detailed,
explaining if necessary any deviations from the normal build procedure of the application (and links to information
about the normal build process needs to be provided).


