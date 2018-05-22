.. _example:

###############################
E-CAM Model Libra	ry module
###############################

.. sidebar:: Software Technical Information

  This list is a work in progress, please help us improve it. We use *definition lists* of ReST_ to make this readable

  Language
    Fortran 95

  Compiler
    gfortran, ifort

  Licence
    GNU Lesser General Public License (LGPL)

  Documentation Tool
    Doxygen

.. contents:: :local:

Purpose of Module
_________________

This module enables to use potentials extracted from the literature. It has the following features:

* One or several degrees of freedom
* One or several electronic states
* For each electronic state, the energy, gradient and hessian can be obtained in the diabatic or adiabatic representations

Applications of the Module
__________________________

This module can be used to 

Installing
__________

 Dependencies: this module needs the fortran modules in the Source_Lib/sub_system directory.

 Build the library (with dependencies):

      make lib

=> it creates a "libpot.a" file

 Build a driver to show how to call subroutines in a external program:

      make lib

=> it creates a "Driver.x" file

 Build the module documentation (with doxygen):

     make doxy

Testing
_______

 To test the installation, you can run the script "run_tests" in Tests directory:
 
     cd Tests ; ./run_tests

The script tests two aspects:

* The "ModLib" library with the implemented potentials

* The "dnSLib" library in which the value, first and second derivatives of intrinsic fortran function (sin, cos ...) are implemented as generic functions. Furthermore, the usual operations (+, - , *, /, **) 	and comparison operators (==, > ...) are also implemented.

 The results will be compared to previous results in Tests/RES_old


Source Code
___________

The source code can be downloaded from the `E-CAM gitlab <https://gitlab.e-cam2020.eu/lauvergn/????>`_.
