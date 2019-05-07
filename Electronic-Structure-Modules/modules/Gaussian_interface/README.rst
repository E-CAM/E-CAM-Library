####################
Gaussian_interface
####################

.. sidebar:: Software Technical Information

  Language
    Python

  Licence
    MIT

  Documentation Tool
    Doxygen

Purpose of the Module
_____________________

This module generates the Gaussian input files of clusters built by cutting of the configurations stored in .gro files.  The clusters are used as model systems to perform fitting of force fields. The user must specify an atom of the configuration and a cut off distance (in nm). The module saturates the residues and prepares the input file for the calculation of the energy and the forces of the cluster generated in the Gaussian format.       


Input/Output Structure
______________________

An "input.txt" file must be included in the working directory. It must contain the following information:

 1. the index of the central atom of the clusters
 2. the cut off distance (in nm) in the case of heterogeneous systems or the number of the solvent molecules in the case of solution systems 
 3. the extended path where the .gro files are stored
 4. the extended path of the file of the output files
 5. the level of theory for the calculation of energy and forces
 
The output module produces .com Gaussian input files.
 

Source Code
___________

The source code of the algorithm is available from the `E-CAM Gitlab repository`__. It is included in the file Gaussian_interface.py. For the execution the following command line is required: 

::

      $ python3.6 Gaussian_interfaces.py


Testing
_______

To test the module an example of the "input.txt" file and a set of 24 configurations is provided in the ./test directory.

.. __: https://gitlab.e-cam2020.eu/fracchia/Gaussian_interface/

 




