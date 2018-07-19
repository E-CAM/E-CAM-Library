.. sidebar:: Software Technical Information

  Name
   Weigthed Linear Ridge Regression    

  Language
    C

  Licence
    MIT

  Documentation Tool
    Doxygen

  Application Documentation
    link to documetation

  Relevant Training Material
    Not currently available.

  Software Module Developed by
    Francesco Fracchia

################################
Weigthed Linear Ridge Regression
################################

..  contents:: :local:

Purpose of the Module
_________________

This module solves the weighted linear ridge regression problem providing the linear parameters of a model selected by the user that minimize the deviations of the predictions from the references of the data set. Therefore, it is a supervised learning tool that optimizes the linear parameters of a analytical expression in order to fit a data set. Each element of the data set can be weighted according to the relative importance or reliability attributed by the user. The regularization provides a protection from the overfitting, this inconvinient can occur if the flexibility of the model is too high in relation to the available data. Moreover, the module calculates the leave-one-out cross-validation error for the employed data set. It offers an internal indicator of the accuracy of the fitting.
The module is a component of the LRR-DE software tool, developed to parametrize force fields of metal ions. In the LRR-DE software tool, the WLRR module is combined with the metaheuristic optimization algorithm differential evolution in order to tune the hyperparameters of the model (the regularization parameter and the non-linear parameters of the model).

The LRR-DE module has been developed to parametrize force fields of metal ions, however the method can be applied to optimize the parameters of a general functional form with respect to reference data.

Background Information
______________________

The theoretical background of the LRR-DE procedure is illustrated in the paper [FF2018]_. This module uses the GNU Scientific Library.


Building and Testing
______________________

To compile the code execute the Makefile (including the demo.c file provided in the ./test directory). In ./test directory a data set is provided. 


Source Code
___________

The source code of the algorithm is avaliable from the `E-CAM Gitlab repository`__. 

.. [FF2018] Fracchia F., Del Frate G., Mancini G., Rocchia W., Barone V., Force Field Parametrization of Metal Ions from Statistical Learning Techniques. J. Chem. Theory Comput., 2018, 14(1), pp 255-273

.. __: https://gitlab.e-cam2020.eu/fracchia/Weighted_Linear_Ridge_Regression



 




