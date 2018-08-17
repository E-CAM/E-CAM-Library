..  Four lines for Sara: 
    The Quantics-Qchem-Interface module connects the Quantics code to the time-dependent density functional (TDDFT) module of the electronic structure program QChem. With this module it is possible to use the QChem TDDFT module for excited state direct dynamics calculations. Quantics will start QChem calculations whenever needed, prepares the input file from a template and will read the output of QChem. The QChem results are stored in the Quantics database and can be used in dynamics simulations.
    At the moment it is not possible to use the ADC(2) method for direct dynamics calculations, as in the official release of QChem no nonadiabatic couplings at ADC(2) level are available. They are available in a developer version of QChem and it is planned to extend the interface so that ADC(2) can be used for direct dynamics calculations.


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
    GNU General Lesser Public License

  Documentation Tool
    Documentation provided as in-line comments within the source code and in the Quantics online documentation


  Application Documentation
    Useful documentation can be found `here <http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/>`_
    


  Relevant Training Material
    Useful training can be found `here <http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/>`_



  Software Developed by
    Johannes Ehrmaier, Graham Worth


.. only test 

.. _qq-interface:

#######################################
QQ-Interface (Quantics-QChem-Interface)
#######################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:



Purpose of Module
_________________


The Quantics-Qchem-Interface module connects the full quantum nonadiabatic wavefunction propagation code Quantics to the time-dependent density functional (TDDFT) module of the electronic structure program QChem. QChem provides analytic gradients, hessians and derivative couplings at TDDFT level. With this module it is possible to use the QChem TDDFT module for excited state direct dynamics calculations. Quantics will prepare the input file from a template, start QChem calculations whenever needed and will read the output of QChem. The QChem results are stored in the Quantics database and can be used in dynamics simulations. Due to the modular design of Quantics the TDDFT module of QChem can be used for all dynamics simulations, e.g. dd-vMCG (direct-dynamics with variational multi-configurational gaussians) or surface hopping simulations.


Applications
____________


The module will be used to examine the nonadiabatic excited state dynamics of small to medium sized molecules. The TDDFT module of QChem allows to treat systems that are too large for efficient CASSCF calculations. Until today photoinduced dynamics simulations of such molecules were only possible using trajectory based algorithms. With Quantics a full quantum-mechanical description of the nuclear motion is possible.


Building and Testing
____________________



To use the module, get the latest version of Quantics from the repository and build it as usual. Moreover you have to have a running version of `QChem <http://www.q-chem.com>_` installed on your system. An example calculation, simulating the photodissociation of water using 4 coupled states is added to the Quantics repository, the documentation of the example can be found `here <http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/howtos/run_dd.html>_` After you have copied the 'water.inp' and the 'run_qchem' files to your directory, you have specified the template for the electronic structure calculations and you performed the preparatory calculations, you can start the simulation with::

   quantics -mnd water.inp


In the specific example, Quantics will search for a script called 'run_qchem' (specified in the input file) to start a QChem calculation. The file 'run_qchem' script is of course dependent on your system configuration and has to be adapted. For more information how to run the test simulation please refer to the Quantics documentation.

As vMCG dynamics is very sensitive to numerical issues it is possible, depending on your compiler and machine, that your results differ slightly from the provided reference values (in the order of a few percent), but the qualitative behavior of the results should be preserved.


Source Code
___________


The source code for the QQ-Interface can be found within the Quantics software which can be downloaded via `CCPForge https://ccpforge.cse.rl.ac.uk/gf/project/quantics/_`.  You firstly need to make an account (at CCPForge). The Quantics project has a private repository so you also need to be a member of the project to checkout. then type into terminal::

 svn checkout --username your-user-name https://ccpforge.cse.rl.ac.uk/svn/quantics/gmctdh/quantics/trunk/  



Within the Quantics program, explicit code for the QQ-Interface routine is located in the file ~/quantics/source/opfuncs/funcqchemmod.f90. Most changes can be found in the subroutines 'ddqchem' and 'wrqchem'.

