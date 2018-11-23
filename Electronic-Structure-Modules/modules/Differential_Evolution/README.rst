##############################
Differential Evolution (SHADE)
##############################

.. sidebar:: Software Technical Information

  Language
    C

  Licence
    MIT

  Documentation Tool
    Doxygen
    
  Software Module Developed by
    Francesco Fracchia

Purpose of the Module
_____________________

This module performs a single-objective global optimization in a continuous domain using the metaheuristic algorithm Success-History based Adaptive Differential Evolution (SHADE). SHADE is a recent adaptive version of the differential evolution algorithm, a stochastic population-based derivative-free optimizer. The module is a component of the software tool LRR-DE, developed to parametrize force fields of metal ions. In particular, the role of the SHADE algorithm in LRR-DE is the optimization of the hyperparameters of the model. However the module has general applicability to the black-box minimization of a cost function.

The input of the module is the objective function, upper and lower limits of the domain for each dimension of the search space, and the parameters of the algorithm. The parameters of the algorithm are the size of the vector population, the maximum number of evaluations of the objective function, and the parameters of that govern the termination of the optimization.
The output provides the coordinates of the identified minimum and the corresponding value of the objective function. 


Background Information
______________________

The SHADE algorithm has been proposed by R. Tanabe and A. Fukunaga in the paper "Success-history based parameter adaptation for differential evolution.", Evolutionary Computation (CEC), 2013 IEEE Congress on (pp. 71-78). It is an adaptive version of the differential evolution algorithm [Storn1997]_. It is an evolutionary algorithm based on three main steps: mutation, crossover, selection. 

Source Code
___________

The source code of the algorithm is avaliable from the `Differential Evolution repository`__. To compile the code execute the Makefile (including the demo.c file provided in the ./test directory). The `GNU Scientific Library`__ is necessary (2.4 version tested). The de.c file contains the core of the code, in the de.h file the data types are defined. 


Testing
_______

The demo.c file in the ./test directory includes three test functions: sphere function, ellipse function, and the Michalewicz function. The command to run the test is 

 $ ./de NAMEFUNCTION

where NAMEFUNCTION can assume the values "sphere", "ellips" or "michel". The dimensionality of the functions is set equal to 20, however it can be modified in the demo.c file. The correct application of the module should identify the global minimum of the functions. They are provided in the file optima.dat in the ./test directory. 

The termination of the optimization is defined by three criterions: 1) the maximum number of objective function evaluations 2) the variation of the mean value of the objective function of the population calculated in two cycle separated from a constant number of iterations 3) the difference between the mean value of the objective function of the population and the best value of the population. The criterion are tuned by three parameters defined in the demo.c file: settings.max_evaluations (default value equal to 200000), settings.step_delta is the number of the iterations that separate two checks of the mean value of the objective function of the population (default value equal to 40), settings.tolerance is the tolerance value for the criterions 2) and 3) (default value equal to 0.000001).

A further parameter is set in demo.c file, settings.size (default value equal to 100), that defines the size of the population of the vectors.

Further functions can be added in the demo.c file defining the functional form and the domain of the search space.

.. [Storn1997] Storn, Rainer, and Kenneth Price. "Differential evolution–a simple and efficient heuristic for global optimization over continuous spaces." Journal of global optimization 11.4 (1997): 341-359.

.. __: https://gitlab.e-cam2020.eu:10443/fracchia/Differential_Evolution
.. __: https://www.gnu.org/software/gsl/




 




