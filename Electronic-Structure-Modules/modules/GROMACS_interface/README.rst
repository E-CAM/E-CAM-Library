####################
GROMACS_Interface
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

This module reads the configurations of a molecular system generated by GROMACS and prepares the input for the GRASP Sampling module. The molecular system must contain a metal ion, that in the module is identified with the variable "metal_atom". The module performs the following operations:

 1. It reads the configurations stored as .gro files in the ./GROMACS_Configurations directory
 2. It calculates, for each configuration, the Euclidean distances of all atoms from the metal ion.
 3. It idenfifies the permutational equivalent atoms  
 4. It performs a Gaussian transformation of the distances
 5. It calculates the variances of transformed distances
 6. It selects the "v_len" coordinates with the higher variances
 7. It prepares the input for the GRASP Sampling module as a matrix N_conf x v_len including the transformed distances for all the configurations stored in the ./GROMACS_Configurations directory.    


Input/Output Structure
______________________

The "input_gromacs.txt" file must contain the following information:

 1. the path where the .gro files are stored
 2. the path of the file of the selected coordinates
 3. the path where the output files are written
 4. the number of the atoms of the system minus one
 5. the size of the output vectors that describe the configurations
 6. the readable format file of the input files (.gro)
 7. the sigma value of the Gaussian transformation
 8. the ID of the reference atom (the metal ion) in the Gromacs configurations
 
The module produces the "selected_coords.txt" file that identifies the v_len coordinates of the output matrix. The output files are: "d_store.txt" the matrix of the distances and "k_store.txt" the matrix of the transformed distances.
 

Source Code
___________

The source code of the algorithm is available from the `E-CAM Gitlab repository`__. It is included in the file gromacs_interfaces.py, it is exectuted as follows: 

::

      $ python3.6 gromacs_interface.py


Testing
_______


To test the module an example of the "input_gromacs.txt" file and a set of 100 configurations is provided in the ./test directory.
 
.. __: https://gitlab.e-cam2020.eu/fracchia/GROMACS_interface/



