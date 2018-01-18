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
_________________

This module generates the Gaussian input files of clusters built by cutting of the configurations stored in .gro files. The user must specify an atom of the configuration and a cut off (in nm). The module saturates the residues and prepares the input file for the calculation of the energy and the forces of the cluster generated in the Gaussian format.       


Input/Output Structure
______________________

The .gro files must be stored in the ./selected_Configurations directory. The module produces the input files for Gaussian in the ./CLUSTERS directory.
 

Source Code
___________

The source code of the algorithm is avaliable from the `E-CAM Gitlab repository`__. It is included in the file Gaussian_interface.py. For the execution the following command line is required: 

::

      $ python3.5 Gaussian_interfaces.py


Testing
_______


To test the module a set of 24 configurations is provided in the ./selected_configurations directory



 




