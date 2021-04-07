..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    LearnHPC

  Language
    Terraform for infrastructure

  Licence
    `GPL v2 <https://opensource.org/licenses/GPL-2.0>`_

  Documentation Tool
    Markdown (and `MkDocs <https://www.mkdocs.org/>`_)

  Application Documentation
    https://learnhpc.eu/

  Relevant Training Material
    https://learnhpc.eu/hpc-intro

  Software Module Developed by
    Alan O'Cais (for contribution described here)

.. _learnhpc_gpu:

######################################
EESSI and vGPU support in Magic Castle
######################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

In the module :ref:`eessi_singularity`, we introduced the European Environment for
Scientific Software
Installations (`EESSI <https://eessi.github.io/docs/>`_) which provides a
shared stack of scientific software installations. That software stack is
intended to work on laptops, personal workstations, HPC clusters and in the cloud. That
initiative is built upon the previous efforts of
`Compute Canada <https://www.computecanada.ca/>`_ to develop a pan-Canadian software
infrastructure.

Another interesting project to come from Compute Canada, which leverages the software
infrastructure, is `Magic Castle <https://github.com/ComputeCanada/magic_castle>`_.
Magic Castle which aims to recreate the Compute Canada user experience in public clouds,
it uses the open-source software Terraform and HashiCorp Language (HCL) to define the
virtual machines, volumes, and networks that are required to replicate a virtual HPC
infrastructure. After deployment, the user is provided with a complete HPC cluster
software environment including a Slurm scheduler, a Globus Endpoint, JupyterHub, LDAP,
DNS, and over 3000 research software applications compiled by experts with EasyBuild.

Magic Castle is compatible with AWS, Microsoft Azure, Google Cloud, OpenStack, and OVH.

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module describes the inclusion and support of the EESSI software stack in Magic
Castle. In addition we also include the generalisation of the virtual GPU (vGPU) support
within Magic Castle for those found in the
`Fenix Research Infrastructure <https://fenix-ri.eu/>`_.

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

EU-wide requirements for HPC training are exploding as the adoption of HPC in the wider
scientific community gathers pace. However, the number of topics that can be thoroughly
addressed without providing access to actual HPC resources is very limited, even at the
introductory level. In cases where such access is available, security concerns and the
overhead of the process of provisioning accounts make the scalability of this approach
questionable.

EU-wide access to HPC resources on the scale required to meet the training needs of all
countries is an objective that we attempt to address with
`LearnHPC <https://www.learnhpc.eu/>`_. The proposed
solution leverages Magic Castle to provision virtual HPC systems in a public cloud. This
infrastructure will allow us to dynamically create temporary event-specific HPC
clusters for training purposes, including a scientific software stack from EESSI.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

Since EESSI is now already integrated in Magic Castle, one can simply follow the
standard
`Magic Castle setup instructions <https://github.com/ComputeCanada/magic_castle#setup>`_
and use the
`switch for the EESSI software stack <https://github.com/ComputeCanada/magic_castle/blob/master/docs/README.md#417-software_stack-optional>`_
in the infrastructure configuration file.

If you use vGPU enabled instances for execution nodes in your virtual cluster, the vGPUs
will be automatically configured and included as available resources in the SLURM
environment.

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

For EESSI support in Magic Castle, see

* https://github.com/ComputeCanada/magic_castle/pull/124
* https://github.com/ComputeCanada/puppet-magic_castle/pull/77
* https://github.com/EESSI/software-layer/pull/43
* https://github.com/EESSI/software-layer/pull/47

For the support of the vGPUs from the Fenix Infrastructure, see

* https://github.com/ComputeCanada/puppet-magic_castle/pull/93
* https://github.com/ComputeCanada/puppet-magic_castle/pull/95
* https://github.com/ComputeCanada/puppet-magic_castle/pull/94

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html
