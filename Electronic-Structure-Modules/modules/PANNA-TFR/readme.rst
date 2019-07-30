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

A stable version of the module can be downloaded using:: 
 XXX
Current installation and testing are done with::
 XXX

Here are the commands for installation::
 XXX

Test::
 XXX

Source Code
___________

To be released soon. 

Further Information
______________________

The PANNA-TFR module is developed with the contributions of R. Lot, Y. Shaidu, F. Pellegrini, E. Kucukbenli
