..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    ``jobqueue_features``

  Language
    Python, YAML

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    In-source documentation

  Application Documentation
    Not currently available. Example usage provided.

  Relevant Training Material
    Not currently available.

  Software Module Developed by
    Adam Włodarczyk (Wrocław Centre of Networking and Supercomputing),
    Alan O'Cais (Juelich Supercomputing Centre)


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _htc_eb:

####################
HTC Multi-node Tasks
####################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

This module is the fourth in a sequence that will form the overall capabilities of the HTC library (see :ref:`htc_mpi`
for the previous module). This module deals with installing the software on HPC systems in a coherent manner through the
tool `EasyBuild <https://easybuild.readthedocs.io/en/latest/>`_.

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The HTC library requires configuration for the target system. Typically, this configuration is applicable system-wide.
If the software is provided in the main software stack of the system, this configuration can also be provided centrally.
The goal of the integration with EasyBuild is to highlight how this configuration can be made with an explicit example
of the configuration for the
`JURECA <http://www.fz-juelich.de/ias/jsc/EN/Expertise/Supercomputers/JURECA/JURECA_node.html>`_ system.

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

EasyBuild is a software build and installation framework that allows you to manage (scientific) software on High
Performance Computing (HPC) systems in an efficient way. Full details on can be found in the
`EasyBuild documentation <https://easybuild.readthedocs.io/en/latest/>`_.

EasyBuild already has support for Python packages, what we describe here is the specific configuration required to
install a particular version of the library on a specific software stack on JURECA.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

To build the software requires EasyBuild (see
`installation instructions for EasyBuild here <https://easybuild.readthedocs.io/en/latest/Installation.html>`_) and the
build command:

::

    eb

However, please note that this will only work "out of the box" for those with software installation rights on the JURECA
system. The provided sources (as described below) are intended as templates for those who are familiar with EasyBuild to
adapt to their system (the only expected adaption would be to change the ``toolchain`` to suit their own system).

Source Code
___________

The latest version of the library itself is available on the `jobqueue_features GitHub repository
<https://github.com/E-CAM/jobqueue_features>`_.

There is an open `Pull Request for the JURECA software stack <https://github.com/easybuilders/JSC/pull/6>`_ that
provides all necessary dependencies for the library.

The
:download:`configuration file required for the library on JURECA <./jobqueue_features-0.0.4-intel-para-2018b-Python-3.6.6.eb>`
is included below (a version for Python 2 can also be created by simply changing the Python dependency version):

.. literalinclude:: ./jobqueue_features-0.0.4-intel-para-2018b-Python-3.6.6.eb
  :language: python
