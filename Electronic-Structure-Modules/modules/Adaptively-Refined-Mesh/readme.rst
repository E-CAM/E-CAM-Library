.. _Adaptively-Refined-Mesh:

##############################
Adaptively-Refined-Mesh module
##############################

.. sidebar:: Software Technical Information


  Language
    FORTRAN                                               

  Licence
    GPLv2

  Documentation Tool
    FORD for source code documentation, see `Wannier code documentation  <http://www.wannier.org/ford/>`_.

  Application Documentation
    `http://www.wannier.org/user_guide.html <http://www.wannier.org/user_guide.html>`_

  Relevant Training Material
    `http://www.wannier.org/user_guide.html <http://www.wannier.org/user_guide.html>`_

.. contents:: :local:


.. Add technical info as a sidebar and allow text below to wrap around it

Purpose of Module
_________________

| Adaptively-refined mesh is implemented correctly for even sizes
| (e.g., 4x4). This module contains one important bugfix (calculation of orbital magnetization),
| another less serious bugfix (calculation of spin-colored Fermi contours) and a number of miscellaneous 
| smaller things noted down as TO DO since the last release. (More details in related pull-request 60, see link in source code section.)

Background Information
______________________

This module is produced during the ECAM/Wannier90-developer workshop held in San Sebastian. This coincided with the move of the Wannier90 repository to GitHub to enable easier integration of community contributions. One of the first such contributions was the ability to compute symmetry-adapted Wannier Functions. (For more background information, see ` Wannier code history  <http://www.wannier.org/history.html>`_)
 

Installing
__________

Installation of wannier90 code is relatively simple. Detailed installing inforamtion is given by `this link <https://raw.githubusercontent.com/wannier-developers/wannier90/develop/README.install>`_.


Testing
_______

Test-Suite (`Pull-Request 5 <https://github.com/wannier-developers/wannier90/pull/5>`_) and Travis-CI integration (`Pull-Request 6 <https://github.com/wannier-developers/wannier90/pull/6>`_) are added to Wannier90 repository during this workshop.

Thus, each Pull-Request within this ECAM module passed the Travis-CI continous integration test before being merged into the Wannier90 code.  Within the Travis-CI test, a set of tests in Test-Suite are checked. Manual testing can be done through the following command::
   
    $ make run-tests

If 'run-custom-test-parallel', it runs the specified test in parallel (4 process with MPI)::

     $ make run-custom-test-parallel testdir=example01

For more details, please see `HERE <https://github.com/wannier-developers/wannier90/tree/develop/test-suite>`_.

Source Code
___________

The source code of this module can be found in the following Pull-Requests in Wannier90 repository under Github: 

| https://github.com/wannier-developers/wannier90/pull/60 
