.. _esl-easyconfigs:

###############
ESL Easyconfigs
###############

..  sidebar:: Software Technical Information

  Language
    The easyconfigs are written in Python.

  Licence
    The building framework is distributed under the `GPL <https://opensource.org/licenses/gpl-license>`_. 
    For the licenses used in the different modules included in the Bundle, please check the corresponding documentation.

  Documentation Tool
    ReStructuredText

  Application Documentation
    `README <https://gitlab.com/ElectronicStructureLibrary/esl-easyconfigs/blob/master/Readme.rst>`_

  Relevant Training Material
    Not currently available.

  Software Module Developed by
    The ESL Easyconfigs was created by Micael Oliveira, Yann Pouillon and  Alin Marin Elena.

..  contents:: :local:

The ESL Easyconfigs aims at providing for all the `CECAM Electronic
Structure Library <http://esl.cecam.org>`_ modules and their dependencies
easybuild easyconfigs to allow easy installaiton on supercomputers around
the world that use EasyBuild package manager.


Purpose of Module
_________________

The ESL Easyconfig is a collection of Easybuild easyconfigs 
that allow to easily build on a supercomputer all the libraries and utilities broadly
used in electronic structure calculations, put together to make their
use easier by researchers and scientific software developers. It
includes a set of recipes for building the libraries and their dependencies helping users, 
developers and packagers in obtaining a working installation of complex combinations of
software packages without having to track the dependencies themselves.
We are aiming at providing the recipes up to date for two of the most common toolchains
foss and intel. Once considered mature enough the recipes will be upstreamed to EasyBuild 
official catalogue.


Installation
____________

One needs to install firstly `Easybuild`__ by following the preferred instructions

To install the full set of ESL modules and their dependencies for foss toolchain version
2019a (latest release at time of writing) one needs to do

.. code-block:: bash

      eb easyconfigs/e/esl-bundle/esl-bundle-0.3.1-foss-2019a.eb -r .


One shall note that in organizing the files the easyconfig recipes and their needed patches 
we follow the same convention as EasyBuild itself. 

Source Code
___________

The source code is available from the `Gitlab`__ under the `esl-easyconfigs`__
project. The ESL Bundle directory can be found `here`__.


.. __: EasyBuild https://easybuild.readthedocs.io
.. __: https://gitlab.com
.. __: https://gitlab.com/ElectronicStructureLibrary/esl-easyconfigs
.. __: https://gitlab.com/ElectronicStructureLibrary/esl-easyconfigs/tree/master


