..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

:orphan:

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

.. sidebar:: Software Technical Information

  The information in this section describes *n2p2* as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Name
    n2p2 (NeuralNetworkPotentialPackage)

  Language
    C++

  Licence
    `GPL-3.0-or-later <https://www.gnu.org/licenses/gpl.txt>`__

  Documentation Tool
    `Doxygen <http://www.doxygen.nl/>`__, `Sphinx <http://www.sphinx-doc.org>`__

  Application Documentation
    http://compphysvienna.github.io/n2p2/

  Relevant Training Material
    http://compphysvienna.github.io/n2p2/

  Software Module Developed by
    Martin P. Bircher and Andreas Singraber


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _n2p2_polynomial_symfuncs:

####################################
n2p2 - Polynomial Symmetry Functions
####################################

..  Let's add a local table of contents to help people navigate the page

.. contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

This module introduces a new set of atomic environment descriptors for
high-dimensional neural network potentials (HDNNPs) in *n2p2*.  Polynomial
symmetry functions [1]_ are designed to mimic closely the behavior of
traditional Behler-Parrinello symmetry functions [2]_ but with a significantly
reduced computational cost.

.. The E-CAM library is purely a set of documentation that describes software development efforts related to the
   project. A *module* for E-CAM is the documentation of the single development of effort associated to the project.In
   that sense, a module does not directly contain source code but instead contains links to source code, typically
   stored elsewhere. Each module references the source code changes to which it directly applies (usually via a URL),
   and provides detailed information on the relevant *application* for the changes as well as how to build and test the
   associated software.

.. The original source of this page (:download:`readme.rst`) contains lots of additional comments to help you create
   your documentation *module* so please use this as a starting point. We use Sphinx_ (which in turn uses ReST_) to
   create this documentation. You are free to add any level of complexity you wish (within the bounds of what Sphinx_
   and ReST_ can do). More general instructions for making your contribution can be found in ":ref:`contributing`".

.. Remember that for a module to be accepted into the E-CAM repository, your source code changes in the target
   application must pass a number of acceptance criteria: * Style *(use meaningful variable names, no global
   variables,...)*
   
   * Source code documentation *(each function should be documented with each argument explained)*
   
   * Tests *(everything you add should have either unit or regression tests)*
   
   * Performance *(If what you introduce has a significant computational load you should make some performance
     optimisation effort using an appropriate tool. You should be able to verify that your changes have not
     introduced unexpected performance penalties, are threadsafe if needed,...)*

Purpose of Module
_________________

The symmetry functions proposed in the original work of Behler and Parrinello
[2]_ contain expressions of the form :math:`exp(-\eta r_{ij}^2) f_c(r_{ij})` in
the innermost loop over all neighbors of atoms. Often the cutoff function
:math:`fc(r)` is chosen to be a cosine or hyperbolic tangent. Considering the
computational cost of these transcendental functions an alternative formulation
of symmetry functions based on polynomials like

.. math::

   ((15 - 6x) x - 10) x^3 + 1

has been published recently [1]_. Here, cheap polynomials are combined to form
compact functions in the radial and angular domain which mimic the behavior of
Behler-Parrinello type symmetry functions at a significantly reduced execution
time. The benefits, benchmarks and many example applications are presented in
great detail in [1]_. 

This module's changes to the *n2p2* code comprise of new classes for different
types of polynomial symmetry functions (PSFs), some helper classes and a
redesign of the symmetry function caching mechanism:

*  Helper classes ``CoreFunction`` and ``CompactFunction`` allow unified access
   to the compact function building blocks of PSFs.

*  Six different types of PSFs were implemented in these classes:

   -  ``SymFncCompRad``
   -  ``SymFncCompAngn``
   -  ``SymFncCompAngw``
   -  ``SymFncCompRadWeighted``
   -  ``SymFncCompAngnWeighted``
   -  ``SymFncCompAngwWeighted``

   Here, ``Rad`` and ``Angn``/``Angw`` indicate radial and angular symmetry
   functions variants, respectively. The suffix ``Weighted`` refers to an
   element weighting proposed in [3]_. For each new class in this list also a
   symmetry function group [4]_ version was implemented, following the same
   naming scheme prefixed with ``SymGrp``. See also `this section
   <https://compphysvienna.github.io/n2p2/topics/descriptors.html#low-cost-polynomial-symmetry-functions-with-compact-support>`__
   of the *n2p2* documentation for a more detailed description of the PSFs and
   their parameters.

*  The computation of a set of descriptors allows the reuse of intermediate
   results across multiple symmetry functions with varying parameters. The
   previously existing cutoff function caching [4]_ of *n2p2* was significantly
   improved. By overriding the ``getCacheIdentifiers()`` member function each
   ``SymFnc..`` class can provide identifier strings for required cache fields.
   The ``Mode::setupSymmetryFunctionCache()`` function collects the requirements
   of all symmetry functions and assigns cache positions in the
   ``Atom::Neighbor::cache`` array.

.. Keep the helper text below around in your module by just adding "..  " in
   front of it, which turns it into a comment

.. Give a brief overview of why the module is/was being created, explaining a little of the scientific background and
   how it fits into the larger picture of what you want to achieve. The overview should be comprehensible to a scientist
   non-expert in the domain area of the software module.
   
   This section should also include the following (where appropriate):
   
   * Who will use the module? in what area(s) and in what context?
   
   * What kind of problems can be solved by the code?
   
   * Are there any real-world applications for it?
   
   * Has the module been interfaced with other packages?
   
   * Was it used in a thesis, a scientific collaboration, or was it cited in a publication?
   
   * If there are published results obtained using this code, describe them briefly in terms readable for non-expert
     users. If you have few pictures/graphs illustrating the power or utility of the module, please include them
     with corresponding explanatory captions.

.. .. note::
   
     If the module is an ingredient for a more general workflow (e.g. the module was the necessary foundation for later
     code; the module is part of a group of modules that will be used to calculate certain property or have certain
     application, etc.) mention this, and point to the place where you specify the applications of the more general
     workflow (that could be in another module, in another section of this repository, an application’s website, etc.).

.. .. note::
   
     If you are a post-doc who works in E-CAM, an obvious application for the module (or for the group of modules that
     this one is part of) is your pilot project. In this case, you could point to the pilot project page on the main
     website (and you must ensure that this module is linked there).

.. If needed you can include latex mathematics like
  :math:`\frac{ \sum_{t=0}^{N}f(t,k) }{N}`
  which won't show up on GitLab/GitHub but will in final online documentation.

.. If you want to add a citation, such as [CIT2009]_, please check the source code to see how this is done. Note that
   citations may get rearranged, e.g., to the bottom of the "page".

.. .. [CIT2009] This is a citation (as often used in journals).

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

.. If the modifications are to an existing code base (which is typical) then this would be the place to name that
   application. List any relevant urls and explain how to get access to that code. There needs to be enough information
   here so that the person reading knows where to get the source code for the application, what version this information
   is relevant for, whether this requires any additional patches/plugins, etc.

.. Overall, this module is supposed to be self-contained, but linking to specific URLs with more detailed information
   is encouraged. In other words, the reader should not need to do a websearch to understand the context of this module,
   all the links they need should be already in this module.

This module is based on *n2p2*, a C++ code for generation and application of
neural network potentials used in molecular dynamics simulations. The source
code and documentation are located here:

* *n2p2* documentation: http://compphysvienna.github.io/n2p2/
* *n2p2* source code: http://github.com/CompPhysVienna/n2p2


Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

.. Provide the build information for the module here and explain how tests are run. This needs to be adequately
   detailed, explaining if necessary any deviations from the normal build procedure of the application (and links to
   information about the normal build process needs to be provided).

The code changes from this module are already merged with the main
repository of *n2p2* (see `pull request <https://github.com/CompPhysVienna/n2p2/pull/55>`__).

Because the introduction of a new set of symmetry function enhances the core
library of *n2p2* several applications shipped with *n2p2* will be affected by
the changes. The easiest way to test the new functionality is to run the examples
provided in these ``examples/nnp-predict/`` folders which make use of PSFs:

*  ``Anisole_SCAN``
*  ``DMABN_SCAN``
*  ``Ethylbenzene_SCAN``

First, since the changes from this module are already merged with the main
repository of *n2p2* (see `pull request
<https://github.com/CompPhysVienna/n2p2/pull/55>`__) it is sufficient to
`download the latest version <https://github.com/CompPhysVienna/n2p2>`__. Then,
compile the ``nnp-predict`` tool by running

.. code-block:: bash

   make nnp-predict -j
   
in the ``src`` directory. Next, switch to one of the above example directories
and run the prediction tool:

.. code-block:: bash

   ../../../bin/nnp-predict 0

In the ``SETUP: SYMMETRY FUNCTIONS`` section of the output there should be
symmetry functions with type (column ``tp``) between 20 and 25 which identifies
`different variants
<https://compphysvienna.github.io/n2p2/topics/descriptors.html#low-cost-polynomial-symmetry-functions-with-compact-support>`__
of PSFs. In addition, the section ``SETUP: SYMMETRY FUNCTION CACHE`` contains an
overview of the cache usage.

Regression testing is implemented in *n2p2* and automatically performed upon
submission of a pull request via `Travis CI <https://travis-ci.org>`__. The log
file showing the successful pass of all tests for the specific pull request can
be found `here 
<https://travis-ci.org/github/CompPhysVienna/n2p2/builds/750366858>`__. The
tests include the above prediction examples and also perform a comparison of
analytic and numeric derivatives of symmetry functions.


Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

.. Here link the source code *that was created for the module*. If you are using Github or GitLab and the `Gitflow
   Workflow <https://www.atlassian.com/git/tutorials/comparing-workflows#gitflow-workflow>`_ you can point to your
   feature branch.  Linking to your pull/merge requests is even better. Otherwise you can link to the explicit commits.
   
   * `Link to a merge request containing my source code changes
     <https://github.com/easybuilders/easybuild-easyblocks/pull/1106>`_
   
   There may be a situation where you cannot do such linking. In this case, I'll go through an example that uses a patch
   file to highlight my source code changes, for that reason I would need to explain what code (including exact version
   information), the source code is for.
   
   You can create a similar patch file by (for example if you are using git for your version control) making your
   changes for the module in a feature branch and then doing something like the following:

.. Don't forget the white space around the "literal block" (a literal block keeps all spacing and is a good way to
   include terminal output, file contents, etc.)

.. ::

..   [adam@mbp2600 example (master)]$ git checkout -b tmpsquash
     Switched to a new branch "tmpsquash"

..   [adam@mbp2600 example (tmpsquash)]$ git merge --squash newlines
     Updating 4d2de39..b6768b2
     Fast forward
     Squash commit -- not updating HEAD
      test.txt |    2 ++
      1 files changed, 2 insertions(+), 0 deletions(-)

..   [adam@mbp2600 example (tmpsquash)]$ git commit -a -m "My squashed commits"
     [tmpsquash]: created 75b0a89: "My squashed commits"
      1 files changed, 2 insertions(+), 0 deletions(-)

..   [adam@mbp2600 example (tmpsquash)]$ git format-patch master
     0001-My-squashed-commits.patch


.. To include a patch file do something like the following (take a look at the source code of this document to see the
   syntax required to get this):

..  Below I am telling Sphinx that the included file is C code, if possible it will then do syntax highlighting. I can
    even emphasise partiuclar lines (here 2 and 9-11)

.. .. literalinclude:: ./simple.patch
      :language: c
      :emphasize-lines: 2,9-11
      :linenos:


..  I can't highlight the language syntax of a patch though so I have to exclude
    :language: c

.. .. literalinclude:: ./simple.patch
      :emphasize-lines: 2,9-11
      :linenos:

.. If the patch is very long you will probably want to add it as a subpage which can be done as follows

.. .. toctree::
      :glob:
      :maxdepth: 1
   
      patch

..  Remember to change the reference "patch" for something unique in your patch file subpage or you will have
    cross-referencing problems

.. you can reference it with :ref:`patch`

The easiest way to view the source code changes covered by this module is to use
the `GitHub pull request page
<https://github.com/CompPhysVienna/n2p2/pull/55>`__. There, use the *Files
changed* `tab <https://github.com/CompPhysVienna/n2p2/pull/55/files>`__ to
review all changes.

.. Here are the URL references used (which is alternative method to the one described above)

.. .. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. .. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html

.. [1] `Bircher, M. P.; Singraber, A.; Dellago, C. Improved Description of
   Atomic Environments Using Low-Cost Polynomial Functions with Compact Support.
   arXiv:2010.14414 [cond-mat, physics:physics] 2020.
   <https://arxiv.org/abs/2010.14414>`__

.. [2] `Behler, J. Atom-Centered Symmetry Functions for Constructing
   High-Dimensional Neural Network Potentials. J. Chem. Phys. 2011, 134 (7),
   074106. <https://doi.org/10.1063/1.3553717>`__

.. [3] `Gastegger, M.; Schwiedrzik, L.; Bittermann, M.; Berzsenyi, F.;
   Marquetand, P. WACSF—Weighted Atom-Centered Symmetry Functions as Descriptors in
   Machine Learning Potentials. J. Chem. Phys. 2018, 148 (24), 241709.
   <https://doi.org/10.1063/1.5019667>`__

.. [4] `Singraber, A.; Behler, J.; Dellago, C. Library-Based LAMMPS
   Implementation of High-Dimensional Neural Network Potentials. J. Chem. Theory
   Comput. 2019, 15 (3), 1827–1840.
   <https://doi.org/10.1021/acs.jctc.8b00770>`__
