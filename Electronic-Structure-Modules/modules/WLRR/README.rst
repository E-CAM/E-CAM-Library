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

This module solves the weighted linear ridge regression problem providing the linear parameters of a linear model selected by the user that minimizes the deviations of the predictions from the references of the data set. Moreover, it calculates the leave-one-out cross-validation error for the employed data set. The module is a component of the LRR-DE software tool, developed to parametrize force fields of metal ions.

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



 




