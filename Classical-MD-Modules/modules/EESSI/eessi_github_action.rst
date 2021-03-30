..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    EESSI GitHub Action

  Language
    Yaml, bash

  Licence
    `MIT <https://opensource.org/licenses/MIT>`_

  Documentation Tool
    Markdown

  Application Documentation
    https://github.com/marketplace/actions/eessi

  Relevant Training Material
    None

  Software Module Developed by
    Alan O'Cais (for contribution described here)

.. _eessi_github_action:

####################################################
EESSI-based GitHub Action for Continuous Integration
####################################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

The European Environment for
Scientific Software
Installations (`EESSI <https://eessi.github.io/docs/>`_) is a collaboration
between a number of academic and industrial partners in the HPC community to set up a
shared stack of scientific software installations
to avoid the installation and execution of
sub-optimal applications on HPC resources. The software stack is
intended to work on laptops, personal workstations, HPC clusters and in the cloud,
which means the project will need to support different CPUs, networks, GPUs, and so on.


EESSI can be leveraged in continuous integration (CI) workflows to easily provide the
dependencies of an application. With this module we are a
`GitHub Action <https://docs.github.com/en/actions>`_ for EESSI so that it can be used
with CI on GitHub.

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

To set up the European Environment for Scientific Software Installations (EESSI) for use
in GitHub Workflows.

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The European Environment for
Scientific Software
Installations `EESSI <https://eessi.github.io/docs/>`_ is a collaboration
between a number of academic and industrial partners in the HPC community. Through the
EESSI project, they want to set up a shared stack of scientific software installations
to avoid not only duplicate work across HPC sites but also the execution of
sub-optimal applications on HPC resources.

The software stack is
intended to work on laptops, personal workstations, HPC clusters and in the cloud,
which means the project will need to support different CPUs, networks, GPUs, and so on.
As such the stack can also be leveraged to provide dependencies for applications within
CI workflows.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

You can use this GitHub Action in a workflow in your own repository, see the
`EESSI action in the GitHub Marketplace <https://github.com/marketplace/actions/eessi>`_
for further details.

A minimal job example for GitHub-hosted runners of type ``ubuntu-latest`` is:

.. code-block:: bash

  jobs:
    ubuntu-minimal:
      runs-on: ubuntu-latest
      steps:
      - uses: eessi/github-action-eessi@v1
      - name: Test EESSI
        run: |
          module avail
        shell: bash

This means that one can potentially load any application provided by EESSI in your
workflow. A further full example that uses GROMACS from a particular version of the
EESSI stack (``2020.12``) is:

.. code-block:: bash

  name: ubuntu_gromacs
  on: [push, pull_request]
  jobs:
    build:
      runs-on: ubuntu-latest
      steps:
      - uses: actions/checkout@v2
      - uses: eessi/github-action-eessi@main
        with:
          eessi_stack_version: '2020.12'
      - name: Test EESSI
        run: |
          module load GROMACS
          gmx --version
        shell: bash

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

We link here the
`GitHub repository of the EESSI GitHub Action <https://github.com/EESSI/github-action-eessi>`_.

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html
