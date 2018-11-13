.. sidebar:: Software Technical Information

  Name
   Weigthed Linear Ridge Regression    

  Language
    C

  Licence
    MIT

  Documentation Tool
    Doxygen

  Relevant Training Material
    Not currently available.

  Software Module Developed by
    Francesco Fracchia

################################
Weigthed Linear Ridge Regression
################################

..  contents:: :local:

Purpose of the Module
_____________________

This module solves the weighted linear ridge regression problem calculating the linear parameters of a model selected by the user that minimize the deviations of the predictions from the references of the data set. Therefore, it is a supervised learning tool that optimizes the linear parameters of an analytical expression in order to fit a data set. Each element of the data set can be weighted according to the relative importance or reliability attributed by the user. The regularization provides a protection from the over-fitting, this inconvenient can occur if the flexibility of the model is too high in relation to the available data. Moreover, the module calculates the leave-one-out cross-validation error for the employed data set. 
The WLRR module is a component of the LRR-DE software tool, developed to parametrize force fields of metal ions. In the LRR-DE software tool, the WLRR module is combined with the metaheuristic optimization algorithm differential evolution in order to tune the hyper-parameters of the model (the regularization parameter and the non-linear parameters of the model).

The LRR-DE module has been developed to parametrize force fields of metal ions, however the method can be applied to optimize the parameters of a general functional form with respect to reference data.

Background Information
______________________

The theoretical background of the LRR-DE procedure is illustrated in the paper [FF2018]_. The LRR-DE procedure is a supervised learning methodology that combines the weighted linear ridge regression algorithm, to obtain the linear patameters of the model, with the differential evolution optimizer, to obtain the non-linear parameters of the model, using the leave-one-out cross-validation error as objective function. This module uses the GNU Scientific Library.


Building and Testing
______________________

To compile the code execute the Makefile (including the demo.c file provided in the `./test`__ directory). In ./test directory a multi-objective data set is provided. The demo.c file includes an example for the definition of a model. The example is the parametrization of a force field with three components (Coulomb, Lennard-Jones 12-6) of the zinc ion in water with respect the solvatation energy and the forces on the ion for a set of clusters. The linear parameters calculated by the module should be 2.40203305, 0.00001364, and -0.10986800.


Source Code
___________

The source code of the algorithm is available is available from the `Weighted Linear Ridge Regression repository <https://gitlab.e-cam2020.eu/fracchia/Weighted_Linear_Ridge_Regression>`__. The ./source directory includes two files: i) wlrr.c contains the functions that perform the scaling of the data, the operation of fitting and the calculation of the leave-one-out cross-validation error; ii) wlrr.h define the data types employed by wlrr.c. 

.. [FF2018] Fracchia F., Del Frate G., Mancini G., Rocchia W., Barone V., Force Field Parametrization of Metal Ions from Statistical Learning Techniques. J. Chem. Theory Comput., 2018, 14(1), pp 255-273

.. __: https://gitlab.e-cam2020.eu/fracchia/Weighted_Linear_Ridge_Regression/tree/master/test




 




