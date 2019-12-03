#############
PANNA-TRAIN
#############

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

PANNA-TRAIN is a neural network training module for atomistic data, eg. prediction of total energy and forces 
given a crystal structure. 
It implements a separate atomic network for each species, following the seminal work of Behler and Parinello (see References [1]_, [2]_, [3]_)
which can later be used as interatomic potential in molecular dynamics simulations.

PANNA-TRAIN uses TensorFlow framework as the underlying neural network training and data i/o engine.

Features
__________

PANNA-TRAIN supports all to all connected networks for each species. 
Networks with different number of nodes and layers are allowed. 
It further supports controlling the training dynamics: eg. freeze/unfreeze layers, weight transfer, decaying learning rates etc. 

Building and Testing
______________________________

A stable version of the module can be downloaded using the download button on this `page <https://gitlab.com/PANNAdevs/panna>`_

As a python module PANNA-TRAIN does not require installation but it relies on numpy library version >= 1.15.0, tensorflow version >= 1.13.0, and 
tensorboard version >= 1.13.0. Note that with version 2.0.0, tensorflow libraries went under substantial changes in structure, the 1.1X.X 
family supports the equally valid previous structure and is still being maintained. PANNA-TRAIN requires tensorflow 1.1X.X family of versions. 

In order to set up and test the module, run the following::

 $ tar -zxvf panna-master.tar.gz
 $ cd panna-master
 $ python3 ./panna/test-train.py

Usage
______

PANNA-TRAIN main script requires a configuration file that specifies the parameter of the calculation 
such as number of layers and nodes of each neural network layer, learning parameter etc. 
A typical command for using this module is as follows::

 $ export PYTHONPATH=/path/to/panna/directory/panna 
 $ python3 train.py --config train_configuration.ini

A detailed tutorial about the contents of the configuration file can be found 
`here <https://gitlab.com/PANNAdevs/panna/blob/master/doc/tutorial/README_tutorial_1_training.md>`_.

In this comprehensive tutorial, a neural network training scenario is demonstrated from beginning to end. 
Network validation is a key step in network training, hence in the tutorial how to use this module together 
with PANNA-EVAL module used in validation is also explained. 
Together, these two modules cover all the steps necessary to train an atomistic neural network, starting from a data which specifies
the machine learning task in (input, target output) pair form. 

Source Code
___________

PANNA-TRAIN source is currently hosted on `GitLab <https://gitlab.com/PANNAdevs/panna>`_.

Further Information
______________________

The PANNA-TRAIN module is developed with the contributions of R. Lot, Y. Shaidu, F. Pellegrini, E. Kucukbenli

References
____________

PANNA manuscript:

.. [1] R. Lot, Y. Shaidu, F. Pellegrini, E. Kucukbenli. 
  `arxiv:1907.03055 <https://arxiv.org/abs/1907.03055>`_. Submitted (2019). 

and

.. [2] J. Behler and M. Parrinello, Generalized Neural-Network 
  Representation  of  High-Dimensional  Potential-Energy
  Surfaces, Phys. Rev. Lett. 98, 146401 (2007)

.. [3] Justin S. Smith, Olexandr Isayev, Adrian E. Roitberg. 
  ANI-1: An extensible neural network potential with DFT accuracy 
  at force field computational cost. Chemical Science,(2017), DOI: 10.1039/C6SC05720A
