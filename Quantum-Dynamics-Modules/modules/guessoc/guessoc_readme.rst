..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

:orphan:

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

.. _guessoc:

#############################
Spin orbit coupling smoothing
#############################

..  sidebar:: Software Technical Information

  Name
    Guessoc  

  Language
    Fortran 90

  Licence
    GNU General Lesser Public License

  Documentation Tool
    Documentation provided in a README file together with the source code.

  Application Documentation
    Detailed documentation related to the running of the module can found here 'https://gitlab.e-cam2020.eu:10443/sanz/durham-ecam/blob/master/README'

  Relevant Training Material
    Training material is available through the test example

  Software Module Developed by
    Cristina Sanz Sanz
.. contents:: :local:

.. Add technical info as a sidebar and allow text below to wrap around it

Purpose of Module
_________________

This module is a standalone program that allows to smooth the off diagonal values (this program is created for spin-orbit couplings) of an 
electronic structure calculation. The purpose of the module is to remove the sudden changes in the off-diagonal elements (spin-orbit couplings) due to the 
swap between near states and sign changes that electronic structure calculation programs produce along one of the coordinates (internuclear distance in 
the test example). The way to remove the discontinuities is based in the idea that after the diagonalisation of the matrix, the eigenvalues are the spin-orbit states that are obtained from the electronic structure calculation, so the (spin-orbit) coupling elements can be optimised (using conjugate gradient in this program) so that after the diagonalisation using the optimised value the eigenvalue is as near as possible as the one obtained from the electronic structure calculation. We ensure the continuity using the couplings of a point near to the point that we want to optimised. Further details can be found in PCCP, 21, 14429 (2019).

The module is particularly thought for computational chemists that need to fit the spin-orbit couplings to use them in quantum and/or classical dynamics simulations.  

It is a very practical code that save time to scientist that need to remove discontinuities in the off-diagonal values of the (spin-orbit) couplings. It is particularly
useful for matrices bigger 3x3 or 4x4 where the number of couplings makes it difficult to smooth manually. 

The applicability of the code is general for all type of dynamical simulations that require the (spin-orbit) couplings as an analytical function or requires the derivatives of the 
(spin-orbit) couplings.

The code has been already used for the control of the photodissociation of IBr system, where the total number of the states for the simulations is 36, producing a total number of (spin-orbit) couplings equal to 1260. All the couplings needed to be fit to use them in a wavepacket propagation. (Sanz-Sanz C., Worth, G.A., PCCP, 21, 14429-14439 (2019)).

Building and Testing
____________________

The module includes a Makefile. To compile you need:  that will compile the code always that user has a fortran compiler and the lapack and blas libraries installed in the computer. The compiler included in the Makefile is gfotran and no specification is given for the location of lapack and blas subroutines. Be sure that you have access to the compiler and libraries. Otherwise, change the compiler to use and include the full path of the libraries.

Once the executable is created the user can run it just typing ./guessSO.exe. The program reads the input files from the directory inpmat/. For the testing run there are 36x36 elements, each element of the matrix is in a different file. Open one of the files to see the structure of the input files. The program creates intermediate files, ssXX-XX.dat, so that the user can see the evolution of the running. The final output files are adia.dat, adiai.dat, soXX-XX.dat. Where adiai.dat are the initial eigenvalues of the non-optimised matrix (spin-orbit states in this example), adia.dat are the eigenvalues with the optimised matrix and the soXX-XX.dat are the individual elements of the optimised matrix in which the off-diagonal values should be now smooth and suitable for fitting. 


Source Code
___________

The source code for this module can be download via gitlab_. You firstly need to make an account (at gitlab). The code of the guessoc module has its own repository so you can clone it typing:

git clone ssh://git@gitlab.e-cam2020.eu:10022/sanz/durham-ecam.git or
git clone https://gitlab.e-cam2020.eu:10443/sanz/durham-ecam.git

.. _gitlabssh: ssh://git@gitlab.e-cam2020.eu:10022/sanz/durham-ecam.git
.. _gitlab: https://gitlab.e-cam2020.eu:10443/sanz/durham-ecam.git

