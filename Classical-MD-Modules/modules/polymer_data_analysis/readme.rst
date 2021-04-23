..  sidebar:: Software Technical Information

  Name
    polymer_data_analysis

  Language
    Python 3.7, Numba, Numpy, Dask

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    Sphinx

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
gyration radius, internal distances, contact maps, contact probabilities, etc.
This module takes advantage of the `Numba <https://numba.pydata.org/>`_
parallelisation implementation and JIT (just-in-time) compilation.
It also uses `Dask <https://dask.org>`_ to deploy numerous data
analysis on job queuing systems.

Purpose of Module
_________________

The module takes advantage of the Python language as well as
`Numpy <https://numpy.org>`_ to write simple scripts that
run complete data analysis of polymer systems.
It also uses `Numba <https://numba.pydata.org/>`_ to perform
fast computation and easily handle nested loops.

However, computation of quantities like contact maps or
internal distances needs an algorithm that scales like the square
of the system size. If computation cannot use
`Numba <https://numba.pydata.org/>`_,
the modules proposes to deploy data analysis on job queuing systems.

You can also compute quantities like twist and writhe to study bacteria
conformation.

We would like to provide a pipeline to help biophysicist extract
quantities from simulations.

Background Information
______________________

The module uses Python language, `Numpy <https://numpy.org>`_,
`Numba <https://numba.pydata.org/>`_ and `Dask <https://dask.org>`_.

Building and Testing
____________________

The instructions to install, test and run the module can be find on the
`data analysis GitLab repository <https://gitlab.com/pcarrivain/bacteria_analysis>`_.

Source Code
___________

The source code and more information can be find on the
`data analysis GitLab repository <https://gitlab.com/pcarrivain/bacteria_analysis>`_.
