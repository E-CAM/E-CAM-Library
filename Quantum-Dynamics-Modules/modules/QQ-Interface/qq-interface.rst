..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    Quantics

  Language
    Fortran90

  Licence
    None

  Documentation Tool
    Documentation provided as in-line comments within the source code and in the Quantics online documentation


  Application Documentation
    Useful documentation can be found here_
    
    .. _here: http://stchem.bham.ac.uk/~quantics/doc/ 

  Relevant Training Material
    Useful training can be found here_

    .. _here: http://stchem.bham.ac.uk/~quantics/doc/

..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _qq-interface:

####################
QQ-Interface (Quantics-QChem-Interface)
####################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

.. This is an example of what a *module* for E-CAM looks like. The original source of this page (:download:`readme.rst`)
   contains lots of additional comments to help you create your module (and understand ReST_ syntax) so please use this as
   a starting point. You are free add any level of complexity you wish (within the bounds of what ReST_ can do). More
   general instructions for making your contribution can be found in ":ref:`contributing`".

.. Remember that for a module to be accepted into the E-CAM repository, your source code changes in the target application
   must pass a number of acceptance criteria:

.. * Style *(use meaningful variable names, no global variables,...)*

.. * Source code documentation *(each function should be documented with each argument explained)*

.. * Tests *(everything you add should have either unit or regression tests)*

.. * Performance *(If what you introduce has a significant computational load you should make some performance optimisation
  effort using an appropriate tool. You should be able to verify that your changes have not introduced unexpected
  performance penalties, are threadsafe if needed,...)*

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The Quantics-Qchem-Interface module connects the full quantum nonadiabatic wavefunction propagation code Quantics to the time dependent density functional (TDDFT) module of the electronic structure program QChem. QChem provides analytic gradients, hessians and derivative couplings at TDDFT level. With this module it is possible to use the QChem TDDFT module for excited state direct dynamics calculations. Quantics will start QChem calculations whenever needed, prepares the input file from a template and will read the output of QChem. The QChem results are stored in the Quantics database and can be used in dynamics simulations. Due to the modular design of Quantics the TDDFT module of QChem can be used for all dynamics simulations, e.g. dd-vMCG or surface hopping simulations.


.. Background Information
.. ______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

.. If the modifications are to an existing code base (which is typical) then this would be the place to name that
.. application. List any relevant urls and explain how to get access to that code. There needs to be enough information
.. here so that the person reading knows where to get the source code for the application, what version this information is
.. relevant for, whether this requires any additional patches/plugins, etc.

.. Overall, this module is supposed to be self-contained, but linking to specific URLs with more detailed information is
.. encouraged. In other words, the reader should not need to do a websearch to understand the context of this module, all
.. the links they need should be already in this module.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

To use the module, get the latest version of Quantics from the repository and build it as usual. Moreover you have to have a running version of QChem installed on your system. An example calculation, simulating the photodissociation of water using 4 coupled states is added to the Quantics repository. In principle you can run the module with::

 quantics water-dd

In the specific example, Quantics will search for a script called 'run_qchem' (specified in the input file) to start a QChem calculation. The file 'run_qchem' script is of course dependent on your system configuration. For more information please refer to the Quantics documentation.

Source Code
___________


The source code for the chebyshev propagator can be found within the Quantics software which can be downloaded via CCPForge_.  You firstly need to make an account (at CCPForge). The quantics project has a private repository so you also need to be a member of the project to checkout. then type into terminal::

 svn checkout --username your-user-name https://ccpforge.cse.rl.ac.uk/svn/quantics/gmctdh/quantics/branches/ecam17/  

.. _CCPFORGE: https://ccpforge.cse.rl.ac.uk/gf/project/quantics/


Within the Quantics program, explicit code for the QQ-Interface routine is located in the file ~/quantics/source/opfuncs/funcqchemmod.f90

