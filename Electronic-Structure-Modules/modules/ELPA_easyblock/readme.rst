..  sidebar:: Software Technical Information

  Name
    EasyBuild

  Language
    Python

  Licence
    `GPL-2.0 <https://opensource.org/licenses/GPL-2.0>`_

  Documentation Tool
    ReST_

  Application Documentation
    https://easybuild.readthedocs.io

  Relevant Training Material
    See documentation

  Software Module Developed by
    Micael Oliveira


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _elpa_easyblock:

###############################
Add ELPA easyblock to EasyBuild
###############################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

EasyBuild is used by a number of large HPC sites and integrating targeted support for ELPA ensures that those sites
use optimally built versions of ELPA.

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

Automate the selection of appropriate configuration flags for ELPA within EasyBuild depending on the type of CPU and available features.
Include additional options as appropriate. Build single and double precision versions of ELPA and also ensure it is linked against the expected version of the linear algebra libraries.


Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

EasyBuild is a software build and installation framework that allows you to manage (scientific) software on High
Performance Computing (HPC) systems in an efficient way. Full details on can be found in the
`EasyBuild documentation <https://easybuild.readthedocs.io/en/latest/>`_.

EasyBuild already had limited support for ELPA, this module allows for automated hardware specific configuration and optimisations.


Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

To build the software requires EasyBuild (see
`installation instructions for EasyBuild here <https://easybuild.readthedocs.io/en/latest/Installation.html>`_) and an
example build command would be:

::

    eb ELPA-2018.11.001-intel-2019a.eb


Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

There are two relevant Pull Requests in the main EasyBuild repositories:

* https://github.com/easybuilders/easybuild-easyblocks/pull/1621

* https://github.com/easybuilders/easybuild-easyconfigs/pull/8360

.. Here are the URL references used (which is alternative method to the one described above)

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html

