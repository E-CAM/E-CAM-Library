..  sidebar:: Software Technical Information

  Name
    psolver_integration

  Language
    Fortran, with YAML I/O.

  Licence
    `GPL <https://opensource.org/licenses/gpl-license>`_

  Documentation Tool
    Doxygen

  Application Documentation
    `The Solver Package page on the BigDFT Wiki <http://bigdft.org/Wiki/index.php?title=The_Solver_Package>`_

  Relevant Training Material
    `The Solver Package page on the BigDFT Wiki <http://bigdft.org/Wiki/index.php?title=The_Solver_Package>`_

  Software Module Developed by
    BigDFT Developers, SIESTA Developers, Octopus Developers.


.. _psolver_integration:

############################################
Integration of PSolver in SIESTA and Octopus
############################################

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

A Poisson solver is an efficient tool to determine electromagnetic fields produced by an electric charge distributed in
space. The integration of PSolver into SIESTA and Octopus has opened the way for these software programs to access more
complex physical systems.


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

If you want to add a citation, such as [CIT2009]_, please check the source code to see how this is done. Note that
citations may get rearranged, e.g., to the bottom of the "page".

.. [CIT2009] This is a citation (as often used in journals).

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

Source Code
___________

* `PSolver in SIESTA <https://gitlab.com/siesta-project/siesta/-/merge_requests/10>`_


.. Here are the URL references used (which is alternative method to the one described above)

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html

