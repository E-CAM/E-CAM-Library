..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

:orphan:

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    pybop

  Language
    Python (2.7, 3.4, 3.5, 3.6)

  Licence
    `GNU General Public License v3.0 <https://www.gnu.org/licenses/gpl-3.0.en.html>`_

  Documentation Tool
    Sphinx/RST

  Application Documentation
    https://srmnitc.github.io/pybop/html/index.html

  Relevant Training Material
    https://mybinder.org/v2/gh/srmnitc/pybop/master?filepath=examples%2F

  Software Module Developed by
    Sarath Menon


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).


#####
pybop
#####

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

``pybop`` is a python module for calculation of bond orientational order parameters [#]_. The core functionality of ``pybop`` is written in C++ with python wrappers using `pybind11 <https://pybind11.readthedocs.io/en/stable/intro.html>`_ . This allows for fast calculations with possibilities for seamless expansion in python. 

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

Bond orientational order parameters have been widely used in distinction of crystal structures in computational studies [#]_. Additionally, these parameters have also been used to distinguish between solid and liquid particles in studies of crystallisation during solidification [#]_.

``pybop`` provides a flexible post-processing python environment for these calculations, at the same time ensuring speed and efficiency as the core code is written in C++ with `pybind11 bindings <https://pybind11.readthedocs.io/en/stable/intro.html>`_. ``pybop`` also links with `Voro++ <http://math.lbl.gov/voro++/>`_ code to carry out calculations of voronoi volumes, indices and face areas.

Some of the major uses of ``pybop`` are listed below-    

- calculations including the bond order parameters :math:`q_{i}` where :math:`i = \{2,3 \to 12\}`.  
- averaged versions which has been to improve the resolution in identification of crystal structures [#]_.
- weighted :math:`q_{i}` where the contributions are weighted by the voronoi face area shared with adjacent atoms [#]_.
- distinction of liquid and solid atoms based on :math:`q_{6}` parameter.
- calculation of the parameters in non-orthogonal simulation boxes.
- other quantities like radial distribution function, coordination number and voronoi volume of individual particles.

``pybop`` can read in output data from from LAMMPS [#]_ `dump format <https://lammps.sandia.gov/doc/dump.html>`_ and POSCAR files from VASP. The module also provides an easy interface for extension of the available data formats or linking with other codes to read in input data.

.. I will add information about the paper and results using pybop.



Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment



Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

**Installation**  

First, clone the ``pybop`` repository by ``git clone https://github.com/srmnitc/pybop.git``.
After cloning the repository, ``pybop`` can be installed by running ``python setup.py install`` from main code directory. It can be uninstalled by ``pip uninstall pybop``. All the dependencies of ``pybop`` are installed automatically.

**Testing**  

``pybop`` also contains automated tests which use the `pytest <https://docs.pytest.org/en/latest/>`_ python library, which can be installed by ``pip install pytest``. The tests can be run by executing the command ``pytest tests/`` from the main code directory.


**Examples**  

Examples uses of ``pybop`` can be found `here <https://srmnitc.github.io/pybop/html/examples.html>`_. An `interactive notebook <https://mybinder.org/v2/gh/srmnitc/pybop/master?filepath=examples%2F>`_ using binder is also available.

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

The `source code <https://github.com/srmnitc/pybop>`_.  of the module can be found is available on github. 


.. [#]  Steinhardt, PJ, Nelson, DR, Ronchetti, M. Phys. Rev. B 28, 1983.
.. [#]  Lechner, W, Dellago, C, Bolhuis, P.G. J. Chem. Phys. 125, 2011., Diaz Leines, G, Drautz, R, Rogal, J. J. Chem. Phys. 146, 2017.
.. [#]  Diaz Leines, G, Drautz, R, Rogal, J. J. Chem. Phys. 146, 2017.
.. [#]  Lechner, W, Dellago, C. J. Chem. Phys. 129, 2008.
.. [#]  Mickel, W, Kapfer, S.C, Schroder-Turk, G.E, Mecke, K. J. Chem. Phys. 138, 2013.
.. [#]  Plimpton, S. J Comp. Phys. 117, 1995.
