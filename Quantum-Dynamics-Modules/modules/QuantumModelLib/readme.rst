.. _QuantumModelLib:

############################
Quantum Model Library module
############################

.. sidebar:: Software Technical Information

  Language
    Fortran 2003

  Compiler
    * gfortran (version 6.0.3 linux and OSX)
    * ifort (version: 14.0.2, 16.0.3, 17.0.1 linux)

  Licence
    GNU Lesser General Public License (LGPL)

  Documentation Tool
    Doxygen

.. contents:: :local:

Purpose of Module
_________________

This module enables to use potential energy surfaces extracted from the literature. It has the following features:


* One or several degrees of freedom
* One or several electronic states
* For each electronic state, the energy, gradient and hessian can be obtained in the diabatic or adiabatic representations
* The gradient and the hessian can be computed analytically (even for the adiabatic representations) or numerically (with finite differences)

Applications of the Module
__________________________

In standard quantum dynamics approaches (when the wave function is expanded on a grid and/or on a basis set), it is essential to use potential energy surfaces in an analytical form which have to be linked to the quantum dynamics code.

This module contains several model potentials from the literature, which can be called using simple fortran subroutines.

For instance, the phenol potential [1] (2 coordinates, 3 electronic surfaces) is called with the following fortran line:

::

  CALL sub_model1_V(V,Q,ndim,nsurf,pot_name,option)

or

::

  CALL sub_model1_V(V,Q,2,3,'phenol',option)


with pot_name='phenol' and  where V is a 3x3 matrix representing the adiabatic potential (nsurf=3), Q a vector with 2 components (ndim=2) associated to the 2 coordinates.

The diabatic potential can be obtained by calling the "sub_model1_DiaV" subroutine with the same arguments.

[1] Z. Lan, W. Domcke, V. Vallet, A.L. Sobolewski, S. Mahapatra, J. Chem. Phys. 122 (2005) 224315

Installing
__________

 Dependencies: none

 Build the library (with dependencies):

::

      make lib

=> it creates a "libpot.a" library, where the subroutine "sub_model1_V" and others are present.

 Build a driver to show how to call subroutines from an external program:

::

      make driver

=> it creates a "Driver.x" executable file.

 Build the module documentation (with doxygen):

::

     make doxy

Testing
_______

 To test the installation, you can run the script "run_tests" in Tests directory:
 
::

     cd Tests ; ./run_tests

The script tests two aspects:

* The "ModLib" library with the implemented potentials

* The "dnSLib" library in which the value, first, second and third derivatives of intrinsic fortran functions (sin, cos ...) are implemented as generic functions. Furthermore, the usual operations (+, - , \*, /, \*\*) and comparison operators (==, > ...) are also implemented.

 The results will be compared to previous results in Tests/RES_old


Source Code
___________

The source code can be downloaded from the `E-CAM gitlab <https://gitlab.e-cam2020.eu/lauvergn/QuantumModelLib>`_.
