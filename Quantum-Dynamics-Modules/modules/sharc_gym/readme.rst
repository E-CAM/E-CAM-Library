..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

:orphan:

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    Quantics, SHARC

  Language
    Fortran90, Python 2.7.

  Licence
    None

  Documentation Tool
    In-code comments

  Application Documentation
    http://chemb125.chem.ucl.ac.uk/worthgrp/quantics/doc/quantics/input.html

  Relevant Training Material
    Not currently available.

  Software Module Developed by
    Moritz Heindl, Sandra Gomez-Rodriguez


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _sharc_gym:

#########
SHARC-gym
#########

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module aims at building a bridge between surface hopping (SH) [1]_ and more accurate
methods (summarized with the term QUANTUM in the following) like
multiconfigurational time dependent hartree (MCTDH) [2]_ and variational
multiconfigurational gaussian (vMCG) [3]_ by exploiting both types of methods
to overcome the shortcomings of the other in a hybrid approach called the SHARC-gym [4]_.

In the computational simulation of molecular movements and reactions various degrees of
simplification have been introduced.  From exact quantum dynamics available only to model
a few degrees of freedom up to huge coarse-grained simulations capable to model whole
proteins different levels of sophistication are available.  Exact quantum dynamics and
methods that will converge to the exact result are capable to shed insight into the most
intricate of mechanisms at  the  heart  of  processes  like  photosynthesis.   Unfortunately,
the  use  of  these  methods  is hampered by the unfavorable scaling of the simulation time
with the size of the investigated system, limiting those approaches to a few dozen degrees
of freedom.  During the last decades, surface hopping has risen to be one of the most
popular approaches for the simulation of events that involve more than a single
electronic state and more than 10 atoms.  This popularity is due to the ease of
implementation of an SH algorithm and the possibility to plug in properties
calculated using any of the most popular quantum chemistry packages.  However,
while the foundations of SH are easy to grasp, the ad hoc nature of SH means that
there is never any guarantee that the simulated dynamics for a given system resembles
results obtained via more elaborate methods that do not suffer from such crude approximations.
Many of the shortcomings of SH have been highlighted in the scientific literature and
remedies to overcome those have been proposed.  This means that a whole range of various
additional parameters and flavours of SH exist at present that are combined
or used exclusively at the will of the user, hoping that these corrections will result
in a more accurate modelling of the problem at hand.

The SHARC-gym allows the user to overcome this uncertainty by combining SH and QUANTUM
methods in a hybrid fashion.  The method follows an iterative procedure which is briefly
stated here (see also Ref [4]_):

1. Hamiltonian loop: The aim of this loop is to select the most important degrees of
   freedom using SH so that a stripped-down Hamiltonian can be used in QUANTUM dynamics.
   For this,a full-dimensional SH dynamics is conducted which serves as a reference
   throughout this loop.From this full-D SH reference, the degrees of freedom (molecular
   vibrations, movement or even electronic states) that drive the observed dynamics can be
   determined.  Using these essential degrees of freedom,  a new model with reduced
   dimensionality is constructed and a new SH simulation calculated.  If this new
   simulation still contains the most important features of the dynamics, even more
   degrees of freedom can be cut from the Hamiltonian and the SH dynamics is repeated.
   Once too many modes have been stripped away and the results diverge from the full-D SH
   reference, this process is stopped and the Hamiltonian that was used before this last
   dynamics is used in the subsequent Parameter loop.

2. Parameter loop:  In this loop, the reduced Hamiltonian is used in a QUANTUM simulation
   which serves as a QUANTUM reference throughout the loop.  Now that a QUANTUM reference
   in this reduced Hamiltonian is available, the plethora of parameters available in SH
   can be validated for this system. If the initially used set of SH parameters was found
   to perform well, then the SHARC-gym is finished, resulting in a QUANTUM-validated set
   of parameters for the full-D SH dynamics and a reduced Hamiltonian that captures the
   essential dynamics of the much bigger system.  If the best set of SH parameters
   diverges from the set that has been used to determine the reduced Hamiltonian, this
   new set of parameters has to be used again in the Hamiltonian loop and the process has
   to be repeated as a whole until the best agreement is found.

The hybrid approach of the SHARC-gym enables the use of more accurate QUANTUM methods on a
subset of degrees of freedom of larger systems that - as a whole - cannot be treated using
a QUANTUM method.  This selection of important degrees of freedom is based solely on
another dynamics result, eliminating the bias of selecting a set of reactive coordinates
beforehand. The SH dynamics benefit from a validation of the chosen parameters against the
QUANTUM reference.  Furthermore, the SHARC-gym provides a huge amount of possible test
systems to quantify the shortcomings of different parameters of SH or even SH as a whole
as the SHARC-gym may result in a QUANTUM reference which disagree with all the different
flavours of SH.The current implementation of the SHARC-gym uses the SH code
SHARC [5]_ [6]_ and the set of QUANTUM methods implemented in QUANTICS [7]_.

..  References:

.. [1] J.C. Tully, R. K. Preston:   Trajectory  surface  hopping  approach  to  nonadiabatic molecular collisions:  The reaction of H+ with D2,J. Chem. Phys,55, 562 (1971).

.. [2] M.H.  Beck,A.  Jackle, G.  A.  Worth, H.-D.  Meyer:   The  multiconfiguration time-dependent  hartree  (MCTDH)  method:  a  highly  efficient  algorithm  for  propagating wavepackets,Phys. Rep.,324, 1 (2000).

.. [3] G.W. Richings, I. Polyak, K.E. Spinlove, G.A. Worth,I. Burghardt, B. La-sorne:  Quantum dynamics simulations using Gaussian wavepackets:  the vMCG method,Int Rev Phys Chem,34, 269 (2015).

.. [4] S.Gomez, M. Heindl, A. SzabadiandL. Gonzalez:  From Surface Hopping to Quantum  Dynamics  and  Back.  Finding  Essential  Electronic  and  Nuclear  Degrees  of  Freedom and Optimal Surface Hopping Parameters,J. Phys. Chem. A,123, 8321 (2019).

.. [5]  S.Mai, P. Marquetand, L. Gonzalez: Nonadiabatic Dynamics: The SHARC Approach,WIREs Comput. Mol. Sci.,8, e1370 (2018)

.. [6] S.Mai, M. Richter, M. Heindl, M. F. S. J. Menger, A. Atkins, M. Ruckenbauer, F.  Plasser, L.  M.  Ibele, S.  Kropf, M.  Oppel, P.  Marquetand, L.  Gonzalez:SHARC2.1:  Surface Hopping Including Arbitrary Couplings — Program Package for Non-Adiabatic Dynamics, (2019)

.. [7] G.A. Worth, K. Giri, G. W. Richings, M. H. Beck, A. J ̈ackle, H.-D. Meyer:QUANTICS, a suite of programs for molecular QUANTum dynamICS simulations, Version1.1 (2015)

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The SHARC-gym is currently available from a GitHub repository. It needs a working SHARC
installation which is available for free from `<https://sharc-md.org/>`_. Future
development will make the
SHARC-gym available as a built-in in SHARC and will feature improved functionalities to
easily use the QUANTICS set of quantum dynamics methods in combination with SHARC-gym.

Building and Testing
____________________

The SHARC-gym consists of a set of Python scripts written in Python 2.7. To build a working
SHARC installation follow the corresponding installation guide
(`SHARC installation <https://sharc-md.org/?page_id=50#tth_chAp2>`_ ).

A test example for the SHARC-gym is available on the SHARC-gym GitHub page. Entering the
``testcase`` directory, follow the instructions written in ``instructions.txt``.

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

The source code can be found in the
`SHARC-gym repository on GitHub <https://github.com/moritzH7/SHARC-gym>`_.

