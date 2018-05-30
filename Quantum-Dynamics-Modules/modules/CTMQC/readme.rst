.. _CTMQC:

####################
CTMQC
####################

.. sidebar:: Software Technical Information

  Language
    Fortran 90

  License
    don't know yet

  Documentation Tool
    doxygen

.. contents:: :local:


Purpose of Module
_________________

**CTMQC** is a module for excited-state nonadiabatic dynamics, therefore it is used to simulate the coupled dynamics of electrons and nuclei (ideally in gas phase molecular systems) in response to, for instance, an initial electronic excitation.

The purpose of the module is to familiarize the user with a new simulation technique, i.e., the CTMQC method, for treating problems where electronic excited states are populated during the molecular dynamics. Photo-activated ultrafast processes are typical situations in which an approach like CTMQC can be used to predict molecular properties, like structures, quantum yields, or quantum coherence.


As clarified below, the CTMQC module is based on the coupled-trajectory mixed quantum classical algorithm [CTMQC1]_ [CTMQC2]_ that has been derived starting from the evolution equations in the framework the exact factorization of the electron-nuclear wavefunction [EF1]_ [EF3]_. The CTMQC algorithm belongs to the family of quantum-classical methods, as the time evolution of the nuclear degrees of freedom is treated within the classical approximation, whereas electronic dynamics is treated fully quantum mechanically. Basically, the nuclei evolve as point particles, following classical trajectories, while the electrons *generate* the potential inducing such time evolution.

In its current implementation, the module cannot deal with arbitrary nuclear dimensions, but it is restricted to treat 3-dimensional problems, which gives the possibility to compare quantum-classical results easily and directly with quantum wavepacket dynamics. CTMQC has been analyzed and benchmarked against exact propagation results on typical low-dimensional model systems [CTMQC3]_, and applied for the simulation of the photo-initiated ring-opening process of Oxirane [CTMQC4]_. For this study, CTMQC has been implemented in a developer version of the CPMD electronic structure package based on time-dependent density functional theory. Concerning electronic input properties, the CTMQC module requires a grid representation of the adiabatic potential energy surfaces and of the nonadiabatic coupling vectors, since the electronic dynamics is represented and solved in the adiabatic basis. This feature allows the algorithm to be easily adaptable, in the current form, to any quantum chemistry electronic structure package. The number of electronic states to be included is not limited, and can be specified as input.


Coupled-Trajectory Mixed Quantum-Classical Dynamics
___________________________________________________

The *exact factorization of the electron-nuclear wavefunction* [EF1]_ provides a prescription for decomposing the time-dependent Schrödinger equation for a system of interacting electrons and nuclei into the coupled dynamics of the subsystems, i.e., the electronic and the nuclear. The time-dependent molecular wavefunction, :math:`\Psi(\mathbf r, \mathbf R,t)`, is the solution of the TDSE :math:`\hat H\Psi = i\partial_t\Psi`, with Hamiltonian `\hat H(\mathbf r,\mathbf R) = \hat T_n(\mathbf R) + \hat H_{BO}(\mathbf r,\mathbf R)`, containing the nuclear kinetic energy, :math:`\hat T_n`, and the electronic Born-Oppenheimer Hamiltonian, :math:`\hat H_{BO}`, defined as the sum of the electronic kinetic energy and of the interaction potentials. Here, the symbols :math: `\mathbf r,\mathbf R` indicate all electronic and nuclear coordinates, respectively. The full wavefunction can be exactly written as the product

:math:`\Psi(\mathbf r,\mathbf R,t) = \chi(\mathbf R,t)\Phi_{\mathbf R}(\mathbf r,t)`,

where :math:`\chi(\mathbf R,t)` can be considered a genuine nuclear wavefunction, yielding the exact nuclear many-body density and current density, and :math:`\Phi_{\mathbf R}(\mathbf r,t)`, the electronic function, depends parametrically on the nuclear configuration. 

Inserting the exact-factorization form of the full wavefunction into the time-dependent Schrödinger equation yields the coupled evolution equations for the two components of the molecular wavefunction, namely

:math:`\Big[\hat H_{BO} +\hat U_{en}^{coup} - \epsilon\Big]\Phi_{\mathbf R}(\mathbf r,t) = i\hbar\partial_t\Phi_{\mathbf R}(\mathbf r,t)`

:math:`\Bigg[\sum_{\nu=1}^{N_n}\frac{[-i\hbar\nabla_\nu+\mathbf A_\nu]^2}{2M_\nu}+\epsilon\Bigg]\chi(\mathbf R,t) = i\hbar\partial_t\chi(\mathbf R,t)`

where the new quantities introduced will be discussed below. The derivation of these equations can be found in [EF2]_. Nuclear masses are indicated by the symbol :math:`M_\nu$`, with the index :math:`\nu` running over the :math:`N_n` nuclei. In the electronic equation, the operator :math:`\hat U_{en}^{coup}[\Phi_{\mathbf R},\chi]` couples the electronic evolution to the nuclear dynamics as it depends on the nuclear wavefunction,

:math:`\hat U_{en}^{coup} [\Phi_{\mathbf R},\chi]= \sum_{\nu=1}^{N_n} \frac{1}{M_\nu}\Bigg[\frac{[-i\hbar \nabla_\nu-\mathbf A_\nu]^2}{2}+\left(\frac{-i\hbar\nabla_\nu\chi}{\chi}+\mathbf A_\nu\right)\cdot\left(-i\hbar\nabla_\nu-\mathbf A_\nu\right)\Bigg].`

The scalar potential, or time-dependent potential energy surface :math:`\epsilon(\mathbf R,t)`, and the time-dependent vector potential :math:`\mathbf A_\nu(\mathbf R,t)`, are defined by 

:math:`\epsilon(\mathbf R,t) = \left\langle \Phi_{\mathbf R}(t)\right|\hat H_{BO}+\hat U_{en}^{coup}-i\hbar\partial_t\left|\Phi_{\mathbf R}(t)\right\rangle_{\mathbf r}`

:math:`\mathbf A_\nu(\mathbf R,t) = \left\langle \Phi_{\mathbf R}(t)\right|\left.-i\hbar\nabla_\nu\Phi_{\mathbf R}(t)\right\rangle_{\mathbf r}`,

respectively, where :math:`\langle\,\cdot\,\rangle_{\mathbf r}` stands for an integration over the electronic coordinates. In the nuclear time-dependent Schrödinger equation, the time-dependent potentials fully account for electronic nonadiabatic effects, i.e., excited-state effects, on nuclear motion.


Applications of the Module
__________________________




Installation
____________

The CTMQC is a fortran90 based code.
Compilation of the code requires the gfortran compiler, and Lapack and Blas libraries.

Once the main directory has been downloaded, go to the directory and

::

        cd ./src 

        make

Running the command make will compile the source code and generate the executable main.x.
Go back to the main directory with the command

::

        cd ../

and run the command

::

        ./create_dirs.sh

that creates the directory output where all output files will be generated. The directory output contains subdirectories: coeff (for one-dimensional calculations only) contain..., ... etc.

The directory tests contains one-dimensional model systems (potentials and nonadiabatic coupling vectors)

**Tully #1**: 
        


Testing
_______

To run the executable from the main directory, write the command

::

        ./src/main.x < path_to_input

where path_to_input is the path to the input file. Examples of input files are provided in the tests directory, e.g. ./tully_1/k0_10au/input.in .


Source Code
___________

The CTMQC source code and test files can be found at `CTMQC <https://gitlab.e-cam2020.eu:10443/Quantum-Dynamics/CT-MQC>`_.


References
__________

.. [CTMQC1] S. K. Min, F. Agostini, E. K. U. Gross, *Phys. Rev. Lett.* 
          **115** (2015) 073001 `DOI: 10.1103/PhysRevLett.115.073001 
          <https://doi.org/10.1103/PhysRevLett.115.073001>`_

.. [CTMQC2] F. Agostini, S. K. Min, A. Abedi, E. K. U. Gross, *J. Chem. Theory Comput* 
          **5** (2016) 2127 `DOI: 10.1021/acs.jctc.5b01180
          <https://doi.org/10.1021/acs.jctc.5b01180>`_

.. [CTMQC3] Graeme H. Gossel, Federica Agostini, Neepa T. Maitra, (2018) `arXiv: 1805.03534 [physics.chem-ph]
          <https://arxiv.org/abs/1805.03534>`_

.. [CTMQC4] S. K. Min, Federica Agostini, I. Tavernelli, E. K. U. Gross, *J. Phys. Chem. Lett.* 
          **8** (2017) 3048 `DOI: 10.1021/acs.jpclett.7b01249
          <https://doi.org/110.1021/acs.jpclett.7b01249>`_

.. [EF1] A. Abedi, N. T. Maitra, E. K. U. Gross, *Phys. Rev. Lett.* 
          **105** (2010) 123002 `DOI: 10.1103/PhysRevLett.105.123002 
          <https://doi.org/10.1103/PhysRevLett.105.123002>`_

.. [EF2] A. Abedi, N. T. Maitra, E. K. U. Gross, *J. Chem. Phys.* 
          **137** (2012) 22A530 `DOI: 10.1063/1.4745836 
          <https://doi.org/10.1063/1.4745836>`_

.. [EF3] F. Agostini, B. F. E. Curchod, R. Vuilleumier, I. Tavernelli, E. K. U. Gross, 
           *TDDFT and Quantum-Classical Dynamics: a Universal Tool Describing the Dynamics of Matter*
           in 'Handbook of Materials Modeling. Volume 1 Methods: Theory and Modeling'', edited by 
           Wanda Andreoni and Sidney Yip, Springer (in production).
