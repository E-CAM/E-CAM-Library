.. _Test-Suite-Travis-CI-Integration:

#######################################
Test-Suite-Travis-CI-Integration module
#######################################

.. sidebar:: Software Technical Information


  Language
    FORTRAN                                               

  Licence
    GPLv2

  Documentation Tool
    FORD for source code documentation, see `Source code documentation <http://www.wannier.org/ford/>`_.

  Application Documentation
    `http://www.wannier.org/user_guide.html <http://www.wannier.org/user_guide.html>`_

  Relevant Training Material
    `http://www.wannier.org/user_guide.html <http://www.wannier.org/user_guide.html>`_

.. contents:: :local:


.. Add technical info as a sidebar and allow text below to wrap around it

Purpose of Module
_________________

| This module added a test-suite, and integrated with GitHub and Travis-CI for
| continuous integration. A number of tests have been added
| (contributed mainly by S. Ponce, Oxford). Also compilation on the
| buildbot test farm at the Oxford Materials Modelling Laboratory has been activated

Background Information
______________________

This module is produced during the ECAM/Wannier90-developer workshop held in San Sebastian. This coincided with the move of the Wannier90 repository to GitHub to enable easier integration of community contributions. One of the first such contributions was the ability to compute symmetry-adapted Wannier Functions. (For more background information, see `Wannier code history <http://www.wannier.org/history.html>`_)


Installing
__________

Installation of wannier90 code is relatively simple. Detailed installing information is given by `this link <https://raw.githubusercontent.com/wannier-developers/wannier90/develop/README.install>`_.


Testing
_______

Test-Suite (`Pull-Request 5 <https://github.com/wannier-developers/wannier90/pull/5>`_) and Travis-CI integration (`Pull-Request 6 <https://github.com/wannier-developers/wannier90/pull/6>`_) are added to Wannier90 repository during this workshop.

Thus, each Pull-Request within this ECAM module passed the Travis-CI continuous integration test before being merged into the Wannier90 code.  Within the Travis-CI test, a set of tests in Test-Suite are checked. Manual testing can be done through the following command::

$ make run-tests

If 'run-custom-test-parallel', it runs the specified test in parallel (4 process with MPI)::

$ make run-custom-test-parallel testdir=example01

For more details, please see `HERE <https://github.com/wannier-developers/wannier90/tree/develop/test-suite>`_.

Source Code
___________

The source code of this module can be found in the following Pull-Requests in Wannier90 repository under Github: 

| https://github.com/wannier-developers/wannier90/pull/5
| https://github.com/wannier-developers/wannier90/pull/6 
| https://github.com/wannier-developers/wannier90/pull/14 
| https://github.com/wannier-developers/wannier90/pull/17 
| https://github.com/wannier-developers/wannier90/pull/22 
| https://github.com/wannier-developers/wannier90/pull/23 
| https://github.com/wannier-developers/wannier90/pull/26 
| https://github.com/wannier-developers/wannier90/pull/49 
| https://github.com/wannier-developers/wannier90/pull/51
| https://github.com/wannier-developers/wannier90/pull/59
| https://github.com/wannier-developers/wannier90/pull/63
| https://github.com/wannier-developers/wannier90/pull/79
| https://github.com/wannier-developers/wannier90/pull/84
