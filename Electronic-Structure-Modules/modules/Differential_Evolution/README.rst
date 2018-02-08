####################
SHADE
####################

.. sidebar:: Software Technical Information

  Language
    C

  Licence
    MIT

  Documentation Tool
    Doxygen

Purpose of the Module
_________________

This module performs a single-objective global optimization in a continuous domain using the metaheuristic algorithm Success-History based Adaptive Differential Evolution (SHADE). SHADE is a recent adaptive version of the differential evolution algorithm, a stochastic population-based derivative-free optimizer. The module is a component of the software tool NAME_SOFTWARE, developed to parametrize force fields of metal ions. In particular, the role of the SHADE algorithm is the optimization of the hyperparameters of the model. The algorithm is set to perform a minimization of a cost function.


Background Information
______________________

The SHADE algorithm has been proposed by R. Tanabe and A. Fukunaga in the paper "Success-history based parameter adaptation for differential evolution.", Evolutionary Computation (CEC), 2013 IEEE Congress on (pp. 71-78).


Input/Output Structure
______________________

The input of the module is the objective function, upper and lower limits of the domain for each dimension of the search space, and the parameters of the algorithm. The parameters of the algorithm are the size of the vector population, the maximum number of evaluations of the objective function, and the parameters of that govern the termination of the optimization.
The output provides the coordinates of the identified minimum and the corresponding value of the objective function.


Source Code
___________

The source code of the algorithm is avaliable from the `E-CAM Gitlab repository`__. To compile the code execute the Makefile (including the demo.c file provided in the ./test directory). The GNU Scientific Library is necessary.


Testing
_______

The demo.c file in the ./test directory includes three test functions: sphere function, ellipse function, and the Michalewicz function. The command to run the test is 

 $ ./de NAMEFUNCTION

where NAMEFUNCTION can assume the values "sphere", "ellips" or "michel". The dimensionality of the functions is set equal to 20, however it can be modified in the demo.c file. Further functions can be added in the demo.c file defining the functional form and the domain of the search space.




 




