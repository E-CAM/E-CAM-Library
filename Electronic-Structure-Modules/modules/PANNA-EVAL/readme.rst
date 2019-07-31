###########
PANNA-EVAL
###########

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

PANNA-EVAL module evaluates an all to all connected neural network  
to predict atomistic quantities, e.g. total energy and forces of a given crystal structure. 

PANNA-EVAL can be used with other modules of the PANNA project for neural network validation, 
but it can also serve to carry the information of the trianed network to other platforms such as
molecular dynamics code LAMMPS. 

Although PANNA-EVAL does not need the advanced capabilities of the TensorFlow framework, 
it uses the 'checkpoint' information to automatically test the performance of a network from training data. 

Features
__________

PANNA-EVAL module has two user-end scripts: evaluate.py and extract_weights.py.

Main script of the PANNA-EVAL module, evaluate.py can evaluate all to all connected networks with various sizes for each species. 
It can also calculate the derivative of the target function, ie. forces for an energy network.

This module was primarily created to validate TensorFlow networks stored during training in checkpoint format, hence it has the functionality to look for 
checkpoint numbers in a training directory, and/or run several checkpoint evaluations at once.

Extract_weights.py script allows to save the network parameters from TensorFlow native checkpoint format to other useful ones, such as 
human readable or LAMMPS potential formats. This last one allows neural networks that are trained and validated using PANNA modules to 
be exported to LAMMPS as interatomic potentials. 


Building and Testing
______________________________

A stable version of the module can be downloaded using the download button on this `page <https://gitlab.com/PANNAdevs/panna>`_

As a python module PANNA-EVAL does not require installation but it relies on numpy library version >= 1.15.0, tensorflow version >= 1.13.0. 
Note that with version 2.0.0, tensorflow libraries went under substantial changes in structure, the 1.1X.X 
family supports the equally valid previous structure and is still being maintained. PANNA-EVAL requires tensorflow 1.1X.X family of versions. 

In order to set up and test the module, run the following::

 $ tar -zxvf panna-master.tar.gz
 $ cd panna-master
 $ python3 ./panna/test-evaluate.py

Currently this test only assesses the evaluate.py script. Another test for extract_weights.py will be released in the near future. 

Usage
______

PANNA-EVAL main script requires a configuration file that specifies the parameter of the calculation 
such as where to find the network to evaluate, which checkpoints to evaluate etc.. 
A typical command for using this module is as follows::

 $ export PYTHONPATH=/path/to/panna/directory/panna 
 $ python3 evaluate.py --config val_config.ini

A detailed tutorial about the contents of the configuration file can be found 
`here <https://gitlab.com/PANNAdevs/panna/blob/master/doc/tutorial/README_tutorial_1_training.md>`_.

In this comprehensive tutorial, a neural network training scenario is demonstrated from beginning to end. 
Network training and validation are two key steps of generating a predictive network, 
hence in the tutorial how to use this module together with PANNA-TRAIN module used in training is also explained. 
Together, these two modules cover all the steps necessary to train an atomistic neural network, starting from a data which specifies
the machine learning task in (input, target output) pair form. 

Source Code
___________

PANNA-EVAL source is currently hosted on `gitlab <https://gitlab.com/PANNAdevs/panna>`_.

Further Information
______________________

The PANNA-EVAL module is developed with the contributions of R. Lot, Y. Shaidu, F. Pellegrini, E. Kucukbenli

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
