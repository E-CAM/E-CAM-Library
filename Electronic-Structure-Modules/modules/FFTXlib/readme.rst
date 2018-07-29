
########
FFTXlib
########

.. sidebar:: Software Technical Information

  Language
    Fortran 1995

  Documentation Tool
    Sphinx, ReStructuredText 

  Application Documentation
   `Doc mirror <http://people.sissa.it/~degironc/FFTXlib/Doc/temp_doc.pdf>`_ 

  Relevant Training Material
    See usage examples in the ``examples`` directory of the source code.
  
  Licence
    GNU Lesser General Public License v3.0

.. contents:: :local:

Purpose of Module
_________________

FFTXlib module is a collection of driver routines for complex 3D fast Fourier transform (FFT) libraries
to be used within planewave-based electronic structure calculation software. 
 
Generally speaking, FFT algorithm requires a data array to act on, a clear description of the 
input-output sequence and transform domains.
In the context of planewave based electronic structure calculations, the data array may hold elements such as 
electronic wavefunction :math:`\psi` or charge density :math:`\rho` or their functions. 
The transform domains are direct (real) and reciprocal space, 
the discretization in real space is represented as a uniform grid of the unit cell and
the discretization of the reciprocal space is in the basis of planewaves whose wavevectors 
are multiples of reciprocal space vectors :math:`(\mathbf G)` .

To understand the main motivation behind FFTXlib routines we need to clarify the differences between the representation
of wavefunction and charge density in planewave based codes:

In these codes, the expansion of wavefunction in planewave basis is
truncated at a cut-off wave-vector :math:`\mathbf G_{max}`.
Since density is the norm-square of the wavefunction, the expansion that is consistent with
the one of wavefunctions requires a cut-off wavevector twice that of wavefunctions: :math:`2 \mathbf G_{max}`.
Meanwhile, the real space FFT domain and the arrays sizes of both density and 
wavefunction in their real space representation are the same.

Therefore, to boost optimization and to reduce numerical noise, the library implements two possible options while performing FFT: 
in one ( 'Wave') the wavevectors beyond :math:`\mathbf G_{max}` are ignored, 
in the other ( 'Rho' ) no such assumption is made. 

Another crucial feature of FFTXlib is that some approximations in the electronic structure calculations 
(such as usage of non-normconserving pseudopotentials) require that density is not just 
norm-square of wavefunctions, but has spatially localized extra components. In that case, 
these localized contributions may require higher G-vector components than the ones needed for density, 
:math:`2 \mathbf G_{max}`. 
Hence, in such systems, the density array in reciprocal space has more elements 
than the norm-conserving case (or in other words, a finer resolution, a denser grid is needed in real space)
while the resolution needed to represent wavefunctions are left unchanged. 

To accommodate for these different requirements of grid size, and to be able to make Fourier transforms back and forth between them, 
the FFTXlib routines explicitly require descriptor arguments which define the grids to be used. For example, 
if potential is obtained from density, the FFT operations on it should use the denser grid;
while FFT on wavefunctions should use the smoother grid (corresponding to :math:`2\mathbf G_{max}` as defined before).
When the Hamiltonian's action on wavefunctions are being calculated, the potential should be 
brought from dense to smooth grid. 
But when the density is being calculated, wavefunction normsquare should be carried from smooth to dense grid.
 
A final important feature of FFTXlib is the index mapping. In the simple case of no parallelization, 
as a choice, the reciprocal space arrays are ordered in increasing order of :math:`|G|^2` 
while the real space arrays are sorted in column major order.
Therefore for FFT to be performed, a map between these two orders must be known. 
This index map is created and preserved by the library. 


In summary, FFTXlib allows the user to perform complex 3D fast Fourier transform (FFT) in the context of 
plane wave based electronic structure software. It containes routines to initialize the array structures, 
to calculate the desired grid shapes and impose underlying size assumptions, and provide 
correspondence maps for indices between the two transform domains.

Once this data structure is constructed, forward or inverse in-place FFT can be performed. 
For this purpose FFTXlib can either use a local copy of an earlier version of FFTW (a commonly used open source FFT library),
or it can also serve as a wrapper to external FFT libraries via conditional compilition using pre-processor directives. 
It supports both MPI and OpenMP parallelization technologies.

FFTXlib is currently employed within Quantum Espresso package, a widely used suite of codes 
for electronic structure calculations and materials modeling in the nanoscale, based on 
planewave and pseudopotentials. FFTXlib is also interfaced with "CB Toy Code" module ( link to version in E-CAM library here)
that solves the Kohn Sham equations in the basis of planewaves. 


Background Information
______________________

FFTXlib is mainly a rewrite and optimization of earlier versions of FFT related routines inside Quantum ESPRESSO pre-v6;
and finally their replacement. 
This may shed light on some of the variable name choices, as well as the default of :math:`2\mathbf G_{max}` cut-off
for the expansion of the smooth part of the charge density, and the required format for lattice parameters in order to build the 
FFT domain descriptor.
Despite many similarities, current version of FFTXlib dramatically changes the FFT strategy in the parallel execution, 
from 1D+2D FFT performed in QE pre v6
to a 1D+1D+1D one; to allow for greater flexibility in parallelization. 

Building and Testing
______________________________

A stable version of the module can be downloaded using `this link <http://people.sissa.it/~degironc/FFTXlib/Downloads>`_
..  when fftxlib has its own repo, this link can be moved there.
Current installation and testing are done with gfortran compiler, version 4.4.7.
The configuration uses GNU Autoconf 2.69.

The commands for installation are::

 $ tar -zxvf FFTXlib-1.0.tgz
 $ ./configure
 $ make libfftx

As a result, the library archive "libfftx.a" is produced in src directory,
and symbolicly linked to a "lib" directory.

.. To test whether the library is working as expected, run:: 

..  $ make FFTXtest

.. Besides the PASS/FAIL status of the test, by changing the bash script in the tests directory, you can perform your custom tests.
.. Read the README.test documentation in the tests subdirectory for further details about the tests.

To see how the library works in a realistic case scenario of an electronic structure calculation, run::

 $make FFTXexamples

.. Besides the PASS/FAIL status of the example, by changing the bash script in the examples directory, you can create your custom examples.
A mini-app will be compiled in src directory and will be symbolicly copied into ``bin`` directory. 
The mini-app simulates an FFT scenario with a test unit cell, and plane wave expansion cutoff. 
It creates the FFT structures and tests forward and backward transform on sample array and reports timings. 
Read the README.examples documentation in the examples subdirectory for further details.

Source Code
____________

The FFTXlib bundle corresponding to the stable release can be downloaded from this `link <http://people.sissa.it/~degironc/FFTXlib/Downloads>`_
The source code itself can be found under the subdirectory ``src``.

Since the development is still going on , 
the final version of the bundle can be obtained from the git repository using ``git``::

  git clone https://gitlab.com/QEF/FFTXlib/FFTXlib.git

The version that corresponds to the one of examples and tests is "TBD" and can be obtained via::
 $ git checkout TBD


Further Information
____________________

This documentation and more can be found inside the ``docs`` subdirectory. 
A copy of it is embedded on Prof. de Gironcoli's website_ .
 
.. _website:  http://people.sissa.it/~degironc/

The FFTXlib is developped with the contributions of C. Cavazzoni, S. de Gironcoli,
P. Giannozzi, F. Affinito, P. Bonfa', Martin Hilgemans, Guido Roma, Pascal Thibaudeau,
Stephane Lefranc, Nicolas Lacorne, Filippo Spiga, Nicola Varini, Jason Wood.


