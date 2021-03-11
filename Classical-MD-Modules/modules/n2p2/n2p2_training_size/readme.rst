..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

:orphan:

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

.. sidebar:: Software Technical Information

  Name
    NNTSSD - Tools for Neural Network Training Set Size Dependence

  Language
    Python3_

  Licence
    `GPL-3.0-or-later <https://www.gnu.org/licenses/gpl.txt>`__

  Documentation Tool
    Sphinx_

  Application Documentation
    Available `here
    <https://github.com/MadlenReiner/n2p2/blob/n2p2_training_size/src/doc/sphinx/source/Tools/NNTSSD.rst>`__

  Relevant Training Material
    Included in the Documentation above.

  Software Module Developed by
    Madlen Reiner


.. _NNTSSD:

############################################
n2p2: Tools for Training Set Size Dependence
############################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:


This module provides tools to analyse the training set size dependence of
residual error of neural network potenials (NNPs). It is specifically written to
be used with the NNP `n2p2 <https://compphysvienna.github.io/n2p2>`_.


Purpose of Module
_________________

NNTSSD is a module that allows

* automated dataset creation of varied sizes
* training of the neural network
* analysis of the learning curves obtained in the training process

in order to determine representative learning curves showing residual errors for varied sizes of training sets.
It also provides tools that allow

* the usage of external test sets, which might be useful for developing epoch optimization approaches
* the usage of separate validation datasets, which are used to obtain TSSD
  curves that are independent from test sets that are used for epoch
  optimization
* graphic representation of learning curves and training performance
* a user-friendly way of running NNTSSD methods by filling in an input file

Other methods within the module allow

* processing of input data (namely splitting datasets)
* analysis of training performance (dependence of residual error of the number of training epochs)


Background Information
______________________

Neural network potentials are used in molecular dynamics simulation to reproduce
potential energy surfaces of ab initio methods. This module addresses the
question of dependence of the NNP’s prediction error (characterized by the RMSE
in energy and forces) on the size of the training dataset.


Building, Testing and Examples
______________________________

Building instructions for NNTSSD, information regarding software tests and and
examples can be found `here
<https://github.com/MadlenReiner/n2p2/blob/n2p2_training_size/src/doc/sphinx/source/Tools/NNTSSD.rst>`__.
The additions to *n2p2* presented here are not yet merged with the main *n2p2*
repository. Before following the above instructions please check out the
``n2p2_training_size`` branch in `the author's fork of n2p2
<https://github.com/MadlenReiner/n2p2/tree/n2p2_training_size>`__ using these
commands:

.. code-block:: bash

   git clone git@github.com:MadlenReiner/n2p2.git
   cd n2p2
   git checkout n2p2_training_size

Then, run the build process of *n2p2*

.. code-block:: bash

   cd src
   make

to create the training tools required for NNTSSD. In some cases it may be
required to set paths to external libraries in ``src/makefile.gnu``.


Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

The source code of this module can be found in the
``tools/python/NNTSSD/source`` of the ``n2p2_training_size`` branch in the
author's fork:

* `Link to NNTSSD source code
  <https://github.com/MadlenReiner/n2p2/tree/n2p2_training_size/tools/python/NNTSSD/source>`__

Another way of reviewing the code additions to *n2p2* is to visit the
corresponding pull request:

* `Link to the pull request to include NNTSSD in n2p2
  <https://github.com/CompPhysVienna/n2p2/pull/21>`__

Change to the tab *Files changed* to get an overview of all changes.


.. Here are the URL references used (which is alternative method to the one described above)

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html
.. _Python3: https://www.python.org/

