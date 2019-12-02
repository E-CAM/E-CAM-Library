##########
PANNA-TFR
##########

.. sidebar:: Software Technical Information

 Language
   Python 3.6.

 Documentation Tool
   Sphinx,ReStructuredText

 Application Documentation
   `Doc mirror <https://gitlab.com/PANNAdevs/panna/tree/master/doc>`_

 Relevant Training Material
   See usage examples in the ``doc/tutorial`` directory of the source code.

 Licence
    The MIT License (MIT)

.. contents:: :local:


Purpose of Module
___________________

PANNA-TFR module demonstrates how to efficiently pack the Behler-Parinello and 
modified Behler-Parinello descriptor vectors (See References 1,2,3) written in binary format, into TensorFlow data format
for efficient reading during training. 

These descriptors can then be used within TensorFlow efficiently, reducing the overhead during batch creation. 
PANNA-TFR is built on TensorFlow. 

Features
__________

PANNA-TFR supports descriptors that change size across records, i.e. data points with different number of atoms
are stored efficiently without padding.

Building and Testing
______________________________

A stable version of the module can be downloaded using the download button on this `page <https://gitlab.com/PANNAdevs/panna>`_

As a python module PANNA-TFR does not require installation but it relies on numpy library version => 1.15.0 and tensorflow version => 1.13.0

In order to set up and test the module, run the following::

 $ tar -zxvf panna-master.tar.gz
 $ cd panna-master
 $ python3 ./panna/test-tfr-packer.py

Usage
______

PANNA-TFR main script requires a configuration file that specifies the parameter of the calculation 
such as location of descriptor files or how many descriptors to be packed in a single record file.
A typical command for using this module is as follows::

 $ export PYTHONPATH=/path/to/panna/directory/panna 
 $ python3 tfr_packer.py --config tfr_configuration.ini

A detailed tutorial about the contents of the configuration file can be found 
`here <https://gitlab.com/PANNAdevs/panna/blob/master/doc/tutorial/README_tutorial_2_data_preparation.md>`_.

In this comprehensive tutorial, how use this module with other modules such as PANNA-GVECT and PANNA-TOOLS
is also demonstrated. Together, these modules cover all the steps necessary while going from raw data to descriptors that can be 
used in machine learning workflow.

Source Code
___________

PANNA-TFR source is currently hosted on `gitlab <https://gitlab.com/PANNAdevs/panna>`_.

Further Information
______________________

The PANNA-TFR module is developed with the contributions of R. Lot, Y. Shaidu, F. Pellegrini, E. Kucukbenli

References
____________
PANNA manuscript:
[1] R. Lot, Y. Shaidu, F. Pellegrini, E. Kucukbenli. 
`arxiv:1907.03055 <https://arxiv.org/abs/1907.03055>`_. Submitted (2019). 

[2] J. Behler and M. Parrinello, Generalized Neural-Network 
Representation  of  High-Dimensional  Potential-Energy
Surfaces, Phys. Rev. Lett. 98, 146401 (2007)

[3] Justin S. Smith, Olexandr Isayev, Adrian E. Roitberg. 
ANI-1: An extensible neural network potential with DFT accuracy 
at force field computational cost. Chemical Science,(2017), DOI: 10.1039/C6SC05720A
