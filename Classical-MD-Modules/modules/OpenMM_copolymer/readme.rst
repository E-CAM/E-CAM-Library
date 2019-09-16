..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    OpenMM_Copolymer

  Language
    Python 3.7, OpenMM API

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    sphynx

  Application Documentation
    `pydoc3.7 <https://gitlab.e-cam2020.eu/carrivain/copolymer-using-openmm/blob/master/openmm_copolymer_functions.html>`_

  Relevant Training Material
    `<https://gitlab.e-cam2020.eu/carrivain/copolymer-using-openmm/blob/master>`_

  Software Module Developed by
    Pascal Carrivain


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _OpenMM_Copolymer:

#############################
E-CAM OpenMM_Copolymer module
#############################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

The OpenMM_Copolymer is a module that sample conformation of a block-copolymer given a genome epigenetic state file.
This module takes advantage of the OpenMM software and GPU acceleration.
It builds a Kremer-Grest polymer model with uni-dimensionnal epigenetic informations and construct the epigenetic interactions
based on the model you design.
You simply need to feed the module with a epigenetic state file, the interaction model and the mechanical properties of the polymer.

.. The E-CAM library is purely a set of documentation that describes software development efforts related to the project. A
.. *module* for E-CAM is the documentation of the single development of effort associated to the project.In that sense, a
.. module does not directly contain source code but instead contains links to source code, typically stored elsewhere. Each
.. module references the source code changes to which it direcctly applies (usually via a URL), and provides detailed
.. information on the relevant *application* for the changes as well as how to build and test the associated software.

.. The original source of this page (:download:`readme.rst`) contains lots of additional comments to help you create your
.. documentation *module* so please use this as a starting point. We use Sphinx_ (which in turn uses ReST_) to create this
.. documentation. You are free to add any level of complexity you wish (within the bounds of what Sphinx_ and ReST_ can
.. do). More general instructions for making your contribution can be found in ":ref:`contributing`".

.. Remember that for a module to be accepted into the E-CAM repository, your source code changes in the target application
.. must pass a number of acceptance criteria:

.. * Style *(use meaningful variable names, no global variables,...)*

.. * Source code documentation *(each function should be documented with each argument explained)*

.. * Tests *(everything you add should have either unit or regression tests)*

.. * Performance *(If what you introduce has a significant computational load you should make some performance optimisation
   effort using an appropriate tool. You should be able to verify that your changes have not introduced unexpected
   performance penalties, are threadsafe if needed,...)*

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

Recently, the epigenetic and the tri-dimensionnal structure of a genome is studied by means of block-copolymer.
The epigenetic information is not present on the DNA but on the histone tails modifications.
This one dimensionnal information can be projected along the contour of a polymer.
Then, every pairwise interacts according to the epigenetic states leading to specific patterns of interactions.
However, the strength of the interactions between epigenetic states is not clearly known.
The module we propose uses the OpenMM software with the GPU acceleration to sample as many as possible epigenetic parameters.

.. Give a brief overview of why the module is/was being created, explaining a little of the scientific background and how
.. it fits into the larger picture of what you want to achieve. The overview should be comprehensible to a scientist
.. non-expert in the domain area of the software module.

.. This section should also include the following (where appropriate):

* Polymer physicists, biophysicists, epigenetic modelling.

* To understand the link between epigenetic and tri-dimensionnal structure of a genome. To estimate first-passage-time encounter of two locii.

* It is used in a scientific collaboration to study a specific promoter-enhancer system in the fruit-fly organism.

* Publications: not currently available.

..
   .. note::

     If the module is an ingredient for a more general workflow (e.g. the module was the necessary foundation for later
     code; the module is part of a group of modules that will be used to calculate certain property or have certain
     application, etc.) mention this, and point to the place where you specify the applications of the more general
     workflow (that could be in another module, in another section of this repository, an application’s website, etc.).

   .. note::

     If you are a post-doc who works in E-CAM, an obvious application for the module (or for the group of modules that
     this one is part of) is your pilot project. In this case, you could point to the pilot project page on the main
     website (and you must ensure that this module is linked there).

..
   If needed you can include latex mathematics like
   :math:`\frac{ \sum_{t=0}^{N}f(t,k) }{N}`
   which won't show up on GitLab/GitHub but will in final online documentation.

   If you want to add a citation, such as [CIT2009]_, please check the source code to see how this is done. Note that
   citations may get rearranged, e.g., to the bottom of the "page".

   .. [CIT2009] This is a citation (as often used in journals).

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

We use the OpenMM toolkit for molecular dynamics. We implemented functionnalities to build a Kremer-Grest polymer system
with uni-dimensionnal epigenetic information. We also implement functions to build the quantities biologists extract
from experiments. You can find pdf file with a detailed description at `GitLab E-CAM 2020 <https://gitlab.e-cam2020.eu/carrivain/copolymer-using-openmm>`_.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

I built tests using the python unittest module `tests source code <https://gitlab.e-cam2020.eu/carrivain/copolymer-using-openmm/blob/master/unittest_openmm_copolymer_functions.py>`_.

Source Code
___________

The source code can be found at `source code <https://gitlab.e-cam2020.eu/carrivain/copolymer-using-openmm/blob/master>`_.

..
   .. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

   Here link the source code *that was created for the module*. If you are using Github or GitLab and the `Gitflow Workflow
   <https://www.atlassian.com/git/tutorials/comparing-workflows#gitflow-workflow>`_ you can point to your feature branch.
   Linking to your pull/merge requests is even better. Otherwise you can link to the explicit commits.

   * `Link to a merge request containing my source code changes
     <https://github.com/easybuilders/easybuild-easyblocks/pull/1106>`_

   There may be a situation where you cannot do such linking. In this case, I'll go through an example that uses a patch
   file to highlight my source code changes, for that reason I would need to explain what code (including exact version
   information), the source code is for.

   You can create a similar patch file by (for example if you are using git for your version control) making your changes
   for the module in a feature branch and then doing something like the following:

   ..  Don't forget the white space around the "literal block" (a literal block keeps all spacing and is a good way to
       include terminal output, file contents, etc.)

   ::

     [adam@mbp2600 example (master)]$ git checkout -b tmpsquash
     Switched to a new branch "tmpsquash"

     [adam@mbp2600 example (tmpsquash)]$ git merge --squash newlines
     Updating 4d2de39..b6768b2
     Fast forward
     Squash commit -- not updating HEAD
      test.txt |    2 ++
      1 files changed, 2 insertions(+), 0 deletions(-)

     [adam@mbp2600 example (tmpsquash)]$ git commit -a -m "My squashed commits"
     [tmpsquash]: created 75b0a89: "My squashed commits"
      1 files changed, 2 insertions(+), 0 deletions(-)

     [adam@mbp2600 example (tmpsquash)]$ git format-patch master
     0001-My-squashed-commits.patch


   To include a patch file do something like the following (take a look at the source code of this document to see the
   syntax required to get this):

   ..  Below I am telling Sphinx that the included file is C code, if possible it will then do syntax highlighting. I can
       even emphasise partiuclar lines (here 2 and 9-11)

   .. .. literalinclude:: ./simple.patch
	 :language: c
	 :emphasize-lines: 2,9-11
	 :linenos:


   ..  I can't highlight the language syntax of a patch though so I have to exclude
       :language: c

   .. literalinclude:: ./simple.patch
      :emphasize-lines: 2,9-11
      :linenos:

   If the patch is very long you will probably want to add it as a subpage which can be done as follows

   .. toctree::
      :glob:
      :maxdepth: 1

      patch

   ..  Remember to change the reference "patch" for something unique in your patch file subpage or you will have
       cross-referencing problems

   you can reference it with :ref:`patch`

   .. Here are the URL references used (which is alternative method to the one described above)

   .. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
   .. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html

