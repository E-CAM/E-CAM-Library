.. _Openmpbeads:

####################
Openmpbeads
####################

.. sidebar:: Software Technical Information

  Language
    Fortran 90/95

  Licence
    MIT license (MIT)

  Documentation Tool
    Doxygen

.. contents:: :local:

.. This is an example of what a *module* for E-CAM looks like. Please add to this template any additional items that are
.. straightforward to fill out in the general case. You are free add any level of complexity you wish (within the bounds of
.. what ReST_ can do).

.. To add your module, fork this GitLab repository to your account on GitLab. Clone your repository, make a feature branch
.. and add a directory that will contain your module information. Copy this :download:`readme.rst` file there. Push your
.. changes back to GitLab and immediately open a merge request from your feature branch against our repository. We can
.. discuss your module in the merge request and help you get it accepted.

.. Add technical info as a sidebar and allow text below to wrap around it

Purpose of Module
_________________

In order to accurately capture quantum effects, specifically those originating from tunneling of light         
particles or very low temperatures, polymer chains have to be very long to properly sampled the exact
quantum Wigner distribution which becomes very broad in phase space. Because in the sampling procedure,             
generally, for each polymer bead a potential energy has to be calculated within a single sampling step, 
polymer sampling subroutines become a bottleneck for capturing quantum effects. 
Openmpbeads is a patch to the PaPIM code which increases the code's performance by parallelizing the polymer chain 
sampling subroutines. The module introduces the OpenMP parallelization loops for the position (:math:`r`) and auxiliary 
position (:math:`\delta r`) polymer sampling subroutines. In the current version of the PaPIM code within the 
polymer sampling subroutines energy calculations are carried out for each bead of the polymer chain in a sequential way. 
Openmpbeads parallelizes these potential energy calculations over a number of additionally available cores using 
OpenMP parallelization procedure. Because the sampling algorithm is proportional to the number of beads within 
the polymer chain (:math:`N_{\text{beads}}`), or in case of staging of the polymer chain the number of 
calculations is proportional to the segment length of the chain (:math:`N_{\text{seg.size}}`), by further 
distributing this number over available cores (:math:`N_{\text{OpenMP}}`) a significant 
increase of performance can be achieved (:math:`N_{\text{beads}} / N_{\text{OpenMP}}` or 
:math:`N_{\text{seg.size}} / N_{\text{OpenMP}}`), with a limit of a potential energy calculation of a single polymer 
bead being executed on a independent core (:math:`N_{\text{beads}} / N_{\text{OpenMP}} = 1` or 
:math:`N_{\text{seg.size}} / N_{\text{OpenMP}} = 1`). 


.. Applications of the Module
.. .__________________________
.. 
.. Openmpbeads ...


Compiling
_________

The PaPIM_ program source code should be available (for PaPIM download see here_). 
`git` should be also availabe. 
The downloaded Openmpbeads patch should be placed in the PaPIM main directory, 
and is applied to the PaPIM source code by executing the following commnad:

::

        git apply openmpbeads.patch


After the patch has been successfully applied, the OpenMP parallelized PaPIM code can be re-compiled as described 
in the PaPIM documentation_ .

.. _PaPIM: here_
.. _documentation: here_


Testing
_______

The successfulness of Openmpbeads patch application and compilation should be verified by executing the 
codes standard tests. 
The code's tests are located in the directory ``./tests``. 
The same set of tests as for the verification of the PaPIM code is executed, but now 
with the addition of utilizing OpenMP parallelization. Thus a number of processor cores available for the test
should be at least two. 
For details of the PaPIM code standard tests see here_ . 
Before running the tests the code has to be properly compiled by running the ``make`` command in the 
``./source`` sub-directory (see compilation of PaPIM code here_ ). 
The numdiff package is used for automatic comparison purposes and should be made available before running the tests, 
otherwise the diff command will be used automatically instead but the user is warned that the test might fail 
due to numerical differences. 
The tests are performed automatically by executing the command ``./test.sh`` in the ``./tests`` sub-directory 
for all three systems:

::

        cd tests

        ./test.sh -m [number of MPI cores] -o [number of OpenMP cores]

The ``number of MPI cores`` should not exceed 20, and the ``number of OpenMP cores`` should not exceed 5, 
while the product of ``number of MPI cores`` and ``number of OpenMP cores`` should not exceed 100 or the total number 
of cores available on the system. 
Due to small numerical discrepancies between generated outputs and reference values which can cause the tests to fail, 
the user is advise to manually examine the numerical differences between generated output and the corresponding 
reference values in case the tests fail. 


Source Code
___________

The Openmpbeads module patch is located at: https://gitlab.e-cam2020.eu/Quantum-Dynamics/PIM/tree/openmpbeads


Source Code Documentation
_________________________

The Openmpbeads patch adds description to the PaPIM code's documentation. 
Details how to access and generate PaPIM documentation are given here_ .


.. _here: ../PaPIM/readme.html

