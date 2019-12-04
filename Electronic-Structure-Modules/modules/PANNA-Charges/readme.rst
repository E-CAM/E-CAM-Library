##############
PANNA-Charges
##############

.. sidebar:: Software Technical Information

 Language
   Python 3.6

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

PANNA-Charges module demonstrates how to train a neural network to predict local atomic charges. 
This network can later be used to calculate the electrostatic energy density of a crystal. 
See Reference [2]_ for the theoretical model behind this approach.

PANNA-Charges, following other modules within the PANNA project [1]_, uses TensorFlow framework. 

Features
__________

PANNA-Charge supports periodic and aperiodic structures, multiple species, 
and a different all-to-all connected network architecture for each species.
It further supports controlling the training dynamics: eg. freeze/unfreeze layers, weight transfer, decaying learning rates etc. 

Building and Testing
______________________________

A stable version of the module can will be released in the near future, 
and will be available for download using the download button on this `page <https://gitlab.com/PANNAdevs/panna>`_

As a python module PANNA-Charges does not require installation but it relies on numpy library version >= 1.15.0, tensorflow version >= 1.13.0, and 
tensorboard version >= 1.13.0. Note that with version 2.0.0, tensorflow libraries went under substantial changes in structure, the 1.1X.X 
family supports the equally valid previous structure and is still being maintained. PANNA-TRAIN requires tensorflow 1.1X.X family of versions. 

In order to set up and test the module, run the following::

 $ tar -zxvf panna-master.tar.gz
 $ cd panna-master
 $ python3 ./panna/test-charges-train.py

Usage
______

PANNA-Charges main script, charges_train.py, requires a configuration file that specifies the parameter of the calculation 
such as number of layers and nodes of each neural network layer, learning parameter etc. 
A typical command for using this module is as follows::

 $ export PYTHONPATH=/path/to/panna/directory/panna 
 $ python3 charges_train.py --config charges_train_config.ini

A detailed tutorial about the contents of the configuration file will be released  
`here <https://gitlab.com/PANNAdevs/panna/blob/master/doc/tutorial/README_tutorial_3_charges_training.md>`_.

In this comprehensive tutorial, a neural network training scenario for systems with long range interactions will be demonstrated. 

Source Code
___________

PANNA-Charges source is not currently public, when it is released it will be hosted on `GitLab <https://gitlab.com/PANNAdevs/panna>`_.

Further Information
______________________

The PANNA-Charges module is developed with the contributions of Y. Shaidu, R. Lot, F. Pellegrini, E. Kucukbenli.

References
____________

PANNA manuscript:

.. [1] R. Lot, Y. Shaidu, F. Pellegrini, E. Kucukbenli. 
  `arxiv:1907.03055 <https://arxiv.org/abs/1907.03055>`_. Submitted (2019). 

and

.. [2] N. Artrith, T. Morawietz, J. Behler. PRB 83, 153101 (2011). 
  High-dimensional neural-network potentials for multicomponent systems: Applications to zinc oxide.
  Erratum: PRB 86, 079914 (2012). 
