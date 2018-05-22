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

**CTMQC** is a code for excited-state nonadiabatic simulations.


Coupled-Trajectory Mixed Quantum-Classical Dynamics
___________________________________________________


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
