..  sidebar:: Software Technical Information

  Name
    pytbc

  Language
    Python (3.7)

  Licence
    `GNU General Public License v3.0 <https://www.gnu.org/licenses/gpl-3.0.en.html>`_

  Documentation Tool
    Sphinx/RST

  Application Documentation
    https://clangi.gitlab.io/pytbc/

  Relevant Training Material
    https://clangi.gitlab.io/pytbc/notebooks/example_TBC.html
    http://campari.sourceforge.net/V3/tutorials.html

  Software Module Developed by
    Cassiano Langini

  Contributions by   
    Marco Bacci
    Andreas Vitalis
    Davide Garolini


######
pytbc
######

..  contents:: :local:

**pytbc** contains Python bindings to the tree-based clustering algorithm 
by Vitalis and Caflisch [Vitalis2012]_ implemented in `Campari <http://campari.sourceforge.net/>`_.
The algorithm is written in Fortran90 and the Python bindings allow for more flexibility and possibility
of integration with other packages avoiding file-based I/O.
The binding interface is generated with `f90wrap <https://github.com/jameskermode/f90wrap>`_ 
to have access to derived types and then compiled with `f2py <https://docs.scipy.org/doc/numpy/f2py/>`_.

Purpose of Module
_________________

The clustering algorithm published in [Vitalis2012]_ is a hierarchical multi-resolution clustering algorithm 
built on an efficient tree data structure. It is based on the Birch clustering algorithm [CIT].
**pytbc** wraps the basic functionality of the algorithm which can be used with the most common 
clustering distances.

Background Information
______________________

See the `project page <https://gitlab.com/clangi/pytbc>`_ for full details.

Building and Testing
____________________


**Installation**

TODO

**Testing**

TODO

**Examples**

TODO

Source Code
___________

The `source code <https://gitlab.com/clangi/pytbc>`_.  of the module can be found on GitHub.


.. [Vitalis2012] A. Vitalis and A. Caflisch. Efficient Construction of Mesostate Networks from Molecular Dynamics Trajectories. 
   J. Chem. Theory Comput. 8 (3), 1108-1120 (2012) `DOI <https://pubs.acs.org/doi/abs/10.1021/ct200801b>`_
