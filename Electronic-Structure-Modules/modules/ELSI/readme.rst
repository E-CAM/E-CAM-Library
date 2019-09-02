..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    ELSI - ELectronic Structure Infrastructure

  Language
   ELSI is written in Fortran, with bindings in C/C++.

  Licence
   `3-Clause BSD License <https://opensource.org/licenses/BSD-3-Clause>`_

  Documentation Tool
    Doxygen for source code documentation. LaTeX for the user manual.

  Application Documentation
    `User Manual`_

  Relevant Training Material
    'Not currently available.'

  Software Module Developed by
    Victor Wu


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _ELSI:

##########################################
ELSI - ELectronic Structure Infrastructure
##########################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

ELSI provides and enhances scalable, open-source software library solutions for electronic structure calculations in
materials science, condensed matter physics, chemistry, molecular biochemistry, and many other fields.  ELSI focuses
on methods that solve or circumvent eigenvalue problems in electronic structure theory. The ELSI infrastructure should
also be useful for other challenging eigenvalue problems.


Purpose of Module
_________________

ELSI deals with the Kohn–Sham eigenvalue problem, which is central to Kohn–Sham
density-functional theory, one of the most widely used methods in electronic
structure. This problem is often the bottleneck in large scale calculations by
high-performance computation and many different algorithms and strategies exist
to tackle it. ELSI acts as a unified software interface to access different
algorithms and their corresponding implementations. This greatly simplifies the implementation and
optimal use of the different strategies.

One of the key design pillars of ELSI is portability and support for various
computing environments, from laptop-type computers all the way to the most
efficient massively parallel supercomputers and new architectures (GPU and
manycore processors).

The libraries currently supported in ELSI are:

- `ELPA (massively parallel dense eigensolvers) <http://elpa.mpcdf.mpg.de/>`_
- `libOMM (orbital minimization method) <http://esl.cecam.org/LibOMM>`_
- `PEXSI (pole expansion and selected inversion) <http://pexsi.org/>`_
- `EigenExa (massively parallel dense eigensolvers) <http://www.r-ccs.riken.jp/labs/lpnctrt/en/projects/eigenexa>`_
- `SLEPc-SIPs (sparse eigensolver based on shift-and-invert spectral transformations) <http://slepc.upv.es/>`_
- `NTPoly (density matrix purification) <http://github.com/william-dawson/NTPoly>`_

ELSI is used in several electronic structure codes, such as DFTB+, DGDFT, FHI-aims, and SIESTA.


Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

If the modifications are to an existing code base (which is typical) then this would be the place to name that
application. List any relevant urls and explain how to get access to that code. There needs to be enough information
here so that the person reading knows where to get the source code for the application, what version this information is
relevant for, whether this requires any additional patches/plugins, etc.

Overall, this module is supposed to be self-contained, but linking to specific URLs with more detailed information is
encouraged. In other words, the reader should not need to do a websearch to understand the context of this module, all
the links they need should be already in this module.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

ELSI can be installed as part of the ESL-Bundle. Instructions to build the
ESL-Bundle are provided in the :ref:`esl-bundle` module.

ELSI can also be built with EasyBuild_ by using the :ref:`esl-easyconfigs` module.

Detailed instructions on how to build ELSI without using the above options are
provided in the `User Manual`_. ELSI uses the CMake build system and the
procedure to build it is fairly standard:

.. code-block:: bash

 mkdir build
 cd build
 cmake [options] /path/to/elsi/sources
 make
 make install

A complete list of options can be found in the `User Manual`_.

ELSI also provides several test programs. To run the test programs one first needs to enable them when running CMake with the following option:

.. code-block:: bash

  -DENABLE_TESTS=ON

The test programs can be launched with

.. code-block:: bash

  make test


Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

The ELSI source code is available from the `ELSI website <https://wordpress.elsi-interchange.org/>`_ or from
the `ELSI Gitlab server <https://git.elsi-interchange.org/elsi-devel>`_.

ELSI was added to the :ref:`esl-bundle` in the following Merge Request:

* https://gitlab.com/ElectronicStructureLibrary/esl-bundle/merge_requests/9


.. Here are the URL references used (which is alternative method to the one described above)

.. _EasyBuild: https://easybuild.readthedocs.io
.. _User Manual: https://wordpress.elsi-interchange.org/index.php/download/

