
..  sidebar:: Software Technical Information

  Name
    pyscal

  Language
    Python (2.7, 3.4, 3.5, 3.6)

  Licence
    `GNU General Public License v3.0 <https://www.gnu.org/licenses/gpl-3.0.en.html>`_

  Documentation Tool
    Sphinx/RST

  Application Documentation
    https://pyscal.readthedocs.io/en/latest/

  Relevant Training Material
    https://mybinder.org/v2/gh/srmnitc/pyscal/master?filepath=examples%2F

  Software Module Developed by
    Sarath Menon
    Grisell Díaz Leines  
    Jutta Rogal


######
pyscal
######

..  contents:: :local:

**pyscal** is a python module for the calculation of local atomic structural environments including Steinhardt's bond orientational order parameters [1]_ during post-processing
of atomistic simulation data. The core functionality of pyscal is written in C++ with python wrappers using
`pybind11 <https://pybind11.readthedocs.io/en/stable/intro.html>`_  which allows for fast calculations and
easy extensions in python.

Purpose of Module
_________________

Steinhardt's order parameters are widely used for the identification of crystal structures [3]_. They are also used to distinguish
if an atom is in a solid or liquid environment [4]_. pyscal is inspired by the
`BondOrderAnalysis <https://homepage.univie.ac.at/wolfgang.lechner/bondorderparameter.html>`_ code,
but has since incorporated many additional features and modifications. The pyscal module includes the following functionalities:

* calculation of Steinhardt's order parameters and their averaged version [2]_.
* links with the `Voro++ <http://math.lbl.gov/voro++/>`_ code, for the calculation of Steinhardt parameters weighted using the face areas of Voronoi polyhedra [3]_.
* classification of atoms as solid or liquid [4]_.
* clustering of particles based on a user defined property.
* methods for calculating radial distribution functions, Voronoi volumes of particles, number of vertices and face area of Voronoi polyhedra, and coordination numbers.


Background Information
______________________


See the `application documentation <https://pyscal.readthedocs.io/en/latest/>`_ for full details.

The utilisation of Dask within the project came about as a result of the `E-CAM High Throughput Computing ESDW <https://www.e-cam2020.eu/event/4424/?instance_id=71>`_ held in Turin in 2018 and 2019.

Building and Testing
____________________


**Installation**

pyscal can be installed directly using `Conda <https://docs.conda.io/en/latest/>`_ by the following statement-

.. code:: console

    conda install -c pyscal pyscal

pyscal can be built from the repository by-

.. code:: console

    git clone https://github.com/srmnitc/pyscal.git
    cd pyscal
    python setup.py install --user

**Testing**

pyscal contains automated tests which
use the `pytest <https://docs.pytest.org/en/latest/>`_ python library, which can be installed by ``pip install pytest``.
The tests can be run by executing the command ``pytest tests/`` from the main code directory.

**Examples**

Examples using pyscal can be found `here <https://pyscal.readthedocs.io/en/latest/examples.html>`_.
An `interactive notebook <https://mybinder.org/v2/gh/srmnitc/pyscal/master?filepath=examples%2F>`_
using binder is also available.

Source Code
___________

The `source code <https://github.com/srmnitc/pyscal>`_.  of the module can be found on GitHub.


.. [1]  `Steinhardt, P. J., Nelson, D. R., & Ronchetti, M. (1983). Physical Review B, 28 <https://journals.aps.org/prb/abstract/10.1103/PhysRevB.28.784>`_.
.. [2]  `Lechner, W., & Dellago, C. (2008). The Journal of Chemical Physics, 129 <https://aip.scitation.org/doi/full/10.1063/1.2977970>`_.
.. [3]  `Mickel, W., Kapfer, S. C., Schröder-Turk, G. E., & Mecke, K. (2013). The Journal of Chemical Physics, 138 <https://aip.scitation.org/doi/full/10.1063/1.4774084>`_.
.. [4]  `Auer, S., & Frenkel, D. (2005). Advances in Polymer Science, 173 <https://link.springer.com/chapter/10.1007/b99429>`_.
