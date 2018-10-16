.. _Openmpbeads:

###########
Openmpbeads
###########

.. sidebar:: Software Technical Information

  Language
    Fortran 90/95

  Licence
    MIT license (MIT)

  Documentation Tool
    Doxygen

  Software Module Developed by
    Przemyslaw Juda, Momir Mališ

.. contents:: :local:


Purpose of Module
_________________

Sampling of quantum properties is performed via the so-called classical isomorphism of path integral. 
In this scheme , a quantum degree of freedom is mapped into a classical polymer with a certain number of beads. 
This number of beads increases with the relevance of quantum effects and can become very large. 
Because in the sampling procedure, generally, for each polymer bead a potential energy evaluation is required 
within a single sampling step, polymer sampling subroutines become a bottleneck. 
**Openmpbeads** is a patch to the :ref:`PaPIM` code which increases the code's performance by parallelizing the polymer chain
sampling subroutines. 
The module introduces the OpenMP parallelization loops for the polymer sampling subroutines. 
In this way, a considerable increase of performance can be achieved. 


.. Applications of the Module
.. .__________________________
.. 
.. Openmpbeads ...


Compiling
_________

The :ref:`PaPIM` program source code (for PaPIM download see :ref:`here <PaPIM>`) and  
`Git <https://git-scm.com/>`_ should be available. 
The downloaded Openmpbeads patch should be placed in the PaPIM main directory, 
and applied to the PaPIM source code by executing the following command:

::

        git apply openmpbeads.patch


After the patch has been successfully applied, the OpenMP parallelized PaPIM code can be re-compiled as described 
in the PaPIM :ref:`documentation <PaPIM>`.


Testing
_______

The successful Openmpbeads patch application and compilation should be verified by executing the 
codes standard tests. 
The code's tests are located in the directory ``./tests``. 
The same set of tests as for the verification of the PaPIM code is executed, but now 
with the addition of utilizing OpenMP parallelization. Thus a number of processor cores available for the test
should be at least two. 
For details of the PaPIM code standard tests see :ref:`here <PaPIM>`. 
Before running the tests the code has to be properly compiled by running the ``make`` command in the 
``./source`` sub-directory (see compilation of PaPIM code :ref:`here <PaPIM>`). 
The ``numdiff`` package is used for automatic comparison purposes and should be made available before running the tests, 
otherwise the ``diff`` command will be used automatically instead but the user is warned that the test might fail 
due to small numerical differences. 
The tests are performed automatically by executing the following command in the ``./tests`` sub-directory:

::

        cd tests

        ./test.sh -m [number of MPI cores] -o [number of OpenMP cores]

The ``[number of MPI cores]`` should not exceed 20, and the ``[number of OpenMP cores]`` should not exceed 5. 
The product of ``[number of MPI cores]`` and ``[number of OpenMP cores]`` should not exceed to the total number
of available cores on the system and should also not exceed number 100. 
Due to small numerical discrepancies between generated outputs and reference values which can cause the tests to fail, 
the user is advised to manually examine the numerical differences between generated output and the corresponding 
reference values in case the tests fail. 


Source Code
___________

The Openmpbeads module patch is located at: https://gitlab.e-cam2020.eu/Quantum-Dynamics/PIM/tree/openmpbeads.


Source Code Documentation
_________________________

The Openmpbeads patch also adds additional description to the PaPIM code's documentation. 
Details how to access and generate PaPIM documentation are given :ref:`here <PaPIM>`.


