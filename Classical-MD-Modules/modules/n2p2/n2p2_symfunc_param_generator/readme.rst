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
    n2p2: Symmetry Function Parameter Generator

  Language
    Python (3.7)

  Licence
    `GPL <https://opensource.org/licenses/gpl-license>`_

  Documentation Tool
    Sphinx_, ReST_

  Application Documentation
    https://github.com/flobuch/n2p2/tree/symfunc_paramgen/tools/python/symfunc_paramgen/doc

  Relevant Training Material
    https://github.com/flobuch/n2p2/tree/symfunc_paramgen/tools/python/symfunc_paramgen/examples

  Software Module Developed by
    Florian Buchner


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _n2p2_symfunc_paramgen:

##############################################
Symmetry Function Parameter Generator for n2p2
##############################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

This module implements schemes from the literature ([GaSc2018]_, [ImAn2018]_) for automatically generating parameter
sets for Behler-Parrinello-type symmetry functions ([BePa2007]_, [Beh2011]_), and variations thereof, in neural network
potential applications. It is designed to work in conjunction with the n2p2 package, but can be used as a standalone,
too.

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

.. Give a brief overview of why the module is/was being created, explaining a little of the scientific background and how
   it fits into the larger picture of what you want to achieve. The overview should be comprehensible to a scientist
   non-expert in the domain area of the software module.

To represent potential energy surfaces via an artificial neural network, in a first step, the n2p2 software package
uses Behler-Parrinello-type symmetry functions ([BePa2007]_, [Beh2011]_), and variations thereof. These serve as
descriptors of an atom's local chemical environment, that make manifest, already at the input level, the output's
invariance w.r.t. translations, rotations, and particle number permutations. They are obtained via a transformation
from the cartesian coordinates of the atom and its neighbor atoms.

The choice of these symmetry functions is an important step in the application of a neural network potential. Both
the number of symmetry functions to be used, and the parameters of those symmetry functions (symmetry functions with
different parameters being sensitive to different regions in an atom's surroundings), need to be decided on. In
principle, using more symmetry functions yields a more complete description of an atom's chemical environment and
thus improves accuracy. On the other hand, the numerical computation of those symmetry functions in fact tends to be
the most computationally expensive step in the application of a neural network potential, so it is undesirable to use
too many symmetry functions.

Now, what this module does is implement algorithms from the literature ([GaSc2018]_, [ImAn2018]_) for automatically
generating sets of these symmetry function parameters. The aim of these algorithms is to create symmetry function
parameter sets that capture all the possible spatial correlations of atoms with their neighbors as completely as
possible, while still being economical (i.e., as few symmetry functions as possible), and to do so in a more
systematic fashion than when parameters are chosen by hand.

Note that the implemented procedures for generating symmetry function parameter sets are agnostic to the actual
dataset of atom configurations that the neural network potential is applied to, and the correlations of atoms therein.
They are merely a way of covering all regions in an atom's environment as completely, yet at the same time as
parsimoniously, as possible, without any knowledge of where neighbor atoms in a given system are actually most likely
to be located. The symmetry functions generated this way are what is referred to as the 'pool of candidate SFs' in
[ImAn2018]_. This is alluding to the fact that this 'pool of candidate SFs' could then be further sparsified,
keeping only those symmetry functions that have the greatest descriptive power for a given dataset. Functionality for
this is, however, not currently implemented in this module.

The main module file is ``sfparamgen.py``, in ``tools/python/symfunc_paramgen/src``. It implements the class
``SymFuncParamGenerator``, which provides methods for the parameter generation described above, as well as for
outputting the parameter sets in the format that is required for the parameter file ``input.nn`` used by n2p2.

.. [GaSc2018] https://doi.org/10.1063/1.5019667
.. [ImAn2018] https://doi.org/10.1063/1.5024611
.. [BePa2007] https://doi.org/10.1103/PhysRevLett.98.146401
.. [Beh2011] https://doi.org/10.1063/1.3553717

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

.. If the modifications are to an existing code base (which is typical) then this would be the place to name that
   application. List any relevant urls and explain how to get access to that code. There needs to be enough information
   here so that the person reading knows where to get the source code for the application, what version this information is
   relevant for, whether this requires any additional patches/plugins, etc.

.. Overall, this module is supposed to be self-contained, but linking to specific URLs with more detailed information is
   encouraged. In other words, the reader should not need to do a websearch to understand the context of this module, all
   the links they need should be already in this module.

This module is designed to be used in conjunction with n2p2, a software package for high-dimensional neural network
potentials in computational physics and chemistry. For more information on n2p2 itself, see:

* n2p2 documentation: https://compphysvienna.github.io/n2p2/index.html
* n2p2 source code: https://github.com/CompPhysVienna/n2p2

That being said, this module does not directly interface the core of n2p2 or call any of its functionality. The
communication of this module with the core of n2p2 is limited to outputting symmetry function parameter sets in a
format that n2p2 can read (the format in which symmetry functions are specified in the parameter file ``input.nn`` of
n2p2). Therefore, the module's functionality for generating symmetry function parameter sets can in principle be used
independently of n2p2.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

.. Provide the build information for the module here and explain how tests are run. This needs to be adequately detailed,
   explaining if necessary any deviations from the normal build procedure of the application (and links to information
   about the normal build process needs to be provided).

Seeing as this module itself is just a lightweight Python tool and does not directly interface the core of n2p2, it does
not require building. Realistically, however, you will want to use it in conjunction with n2p2's functionality for
neural network potentials, for which you need to build n2p2. This is described
`here <https://compphysvienna.github.io/n2p2/>`_.

Follow these steps to test the module:

1. Install the pytest_ package.

2. Navigate to the ``tools/python/symfunc_paramgen/tests`` directory.

3. Run ``pytest`` in your terminal.

4. For an additional code coverage report install the pytest-cov_ package.

5. Go to the ``tools/python/symfunc_paramgen/tests`` directory.

6. Execute ``pytest --cov=sfparamgen --cov-report=html .``.

Examples
________
See the ``example.ipynb`` IPython notebook in the ``tools/python/symfunc_paramgen/examples``
directory (here is a direct link:
https://github.com/flobuch/n2p2/tree/symfunc_paramgen/tools/python/symfunc_paramgen/examples).
Inside the examples folder, run the example by typing ``jupyter notebook example.ipynb`` in your terminal.

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

The source code for this module can be found
`here <https://github.com/flobuch/n2p2/tree/symfunc_paramgen/tools/python/symfunc_paramgen>`__.

Ultimately, this module is intended to be merged into the official n2p2 code. For the status of the corresponding pull
request, see `here <https://github.com/CompPhysVienna/n2p2/pull/15>`__.

.. Here are the URL references used (which is alternative method to the one described above)

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html
.. _pytest: https://docs.pytest.org/en/latest/
.. _pytest-cov: https://pytest-cov.readthedocs.io/en/latest/

