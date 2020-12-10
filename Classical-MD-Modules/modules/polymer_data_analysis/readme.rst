:orphan:

..  sidebar:: Software Technical Information

  Name
    polymer_data_analysis

  Language
    Python 3.7, Numba, Numpy

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    sphynx

  Application Documentation
    `pydoc3.7 <https://gitlab.com/pcarrivain/openmm_plectoneme/blob/master/data_analysis.html>`_

  Relevant Training Material
    `<https://gitlab.com/pcarrivain/openmm_plectoneme/blob/master>`_

  Software Module Developed by
    Pascal Carrivain


.. _polymer_data_analysis:

##################################
E-CAM polymer_data_analysis module
##################################

..  contents:: :local:

The polymer_data_analysis module provides functions to compute quantities like
gyration radius, internal distances, contact maps, contact probabilities ...
This module takes advantage of the `Numba <https://numba.pydata.org/>`_
parallelisation and just-in-time compiler.

Purpose of Module
_________________

The module takes advantage of the Python langage as-well-as Numpy to write
simple scripts that run complete data analysis of polymer systems.
It also uses Numba to perform fast computation and easily parallelize
nested loops. Indeed, computation of quantities like contact maps or
internal distances needs algorithm that scales like the square
of the system size.

You can also compute quantities like twist, writhe to study bacteria
conformation.

We would like to provide a pipeline to help biophysicist extract observables
from simulations.

Background Information
______________________

The module uses Python langage, Numpy and Numba.

Building and Testing
____________________

The instructions to install, test and run the module can be find on the
`data analysis GitLab repository <https://gitlab.com/pcarrivain/bacteria_analysis>`_.

Source Code
___________

The source code and more information can be find on the
`data analysis GitLab repository <https://gitlab.com/pcarrivain/bacteria_analysis>`_.
