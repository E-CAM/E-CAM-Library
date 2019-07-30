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
It implements a separate atomic network for each species, following the seminal work of Behler and Parinello. 
(See References 1,2,3)
which can later be used as interatomic potential in molecular dynamics simulations.

PANNA-TRAIN uses TensorFlow framework as the underlying neural network training and data i/o engine.

Features
__________

PANNA-TRAIN supports all to all connected networks for each species. 
Networks with different number of nodes and layers are allowed. 
It further supports controlling the training dynamics: eg. reeze/unfreeze layers, weight transfer, decaying learning rates etc. 

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

The PANNA-TRAIN module is developed with the contributions of R. Lot, Y. Shaidu, F. Pellegrini, E. Kucukbenli
