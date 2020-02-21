..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    Wannier90, AiiDA

  Language
    Fortran90 for Wannier90, Python for AiiDA

  Licence
    `GPL <https://opensource.org/licenses/gpl-license>`_ for Wannier90,
    `MIT <https://opensource.org/licenses/mit-license>`_ for AiiDA

  Documentation Tool
    `Ford <http://fortranwiki.org/fortran/show/FORD>`_ online link to different Wannier90 source files `<http://www.wannier.org/ford/>`_, ReST_ for AiiDA

  Application Documentation
    `Wannier90 <http://www.wannier.org/support/>`_, `AiiDA <https://aiida.readthedocs.io/projects/aiida-core/en/latest/>`_

  Relevant Training Material
    See application documentation above

  Software Module Developed by
    Giovanni Pizzi and Antimo Marrazzo in collaboration with Valerio Vitale


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _max_collab:

########################################
Automated high-throughput Wannierisation
########################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

Maximally-localised Wannier functions (MLWFs) are routinely used to compute from first- principles advanced materials
properties that require very dense Brillouin zone integration and to build accurate tight-binding models for
scale-bridging simulations. At the same time, high-thoughput (HT) computational materials design is an emergent field
that promises to accelerate the reliable and cost-effective design and optimisation of new materials with target
properties. The use of MLWFs in HT workflows has been hampered by the fact that generating MLWFs automatically and
robustly without any user intervention and for arbitrary materials is, in general, very challenging. We address this
problem directly by proposing a procedure for automatically generating MLWFs for HT frameworks. Our approach is based
on the selected columns of the density matrix method (SCDM, see :ref:`SCDM_in_Wannier90`) and is implemented in an AiiDA
workflow.

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

Create a fully-automated protocol based on the SCDM algorithm for the construction of MLWFs, in which the two free
parameters are determined automatically (in our HT approach the dimensionality of the disentangled space is fixed by the
total number of states used to generate the pseudopotentials in the DFT calculations).

In the `paper derived from this work <https://psi-k.net/download/highlights/Highlight_147.pdf>`_ [vitale2019]_, we apply our approach to a dataset of 200 bulk crystalline materials that span a wide structural and chemical
space. We assess the quality of our MLWFs in terms of the accuracy of the band-structure interpolation that they provide
as compared to the band-structure obtained via full first-principles calculations.

.. [vitale2019]  `arXiv:1909.00433 <https://arxiv.org/abs/1909.00433>`_ [physics.comp-ph] 

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module is a collaboration between the E-CAM and `MaX <http://www.max-centre.eu/>`_ HPC centres of excellence.

In :ref:`SCDM_in_Wannier90`, E-CAM has implemented the SCDM algorithm in the ``pw2wannier90`` interface code between the
Quantum ESPRESSO software and the Wannier90 code. We have used this
implementation as the basis for a complete computational workflow for obtaining MLWFs
and electronic properties based on Wannier interpolation of the BZ, starting only from the
specification of the initial crystal structure. We have implemented our workflow within the
AiiDA materials informatics platform, and we used it to perform a HT study on a dataset
of 200 materials.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

An AiiDA export file is provided with the full provenance of all simulations run in the project.

Source Code
___________

See the `Materials Cloud entry <https://archive.materialscloud.org/2019.0044/v2>`_.
A downloadable virtual machine is provided that
allows to reproduce the results of the associated paper and also to run new calculations for different materials, including all
first-principles and atomistic simulations and the computational workflows.

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
