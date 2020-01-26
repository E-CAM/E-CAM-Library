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

The OpenMM_Copolymer is a module that sample conformation of a *block-copolymer* given an *epigenome* state file.
This module takes advantage of the OpenMM software and GPU acceleration.
It builds a `Kremer-Grest <https://aip.scitation.org/doi/10.1063/1.458541>`_ polymer model with uni-dimensional epigenetic information and construct the epigenetic interactions based on the model you design.
You simply need to feed the module with an *epigenome* state file, the interaction model and the mechanical properties of the polymer.
You can imagine to model small part of an *epigenome* or the whole genome confined inside the cell nucleus.

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

The epigenetic and the tri-dimensional structure of fly genome is studied by means of *block-copolymer*.
The *block-copolymer* is a polymer made of more than one monomer species.
The epigenetic information do not involve alterations in the DNA but `histone <https://en.wikipedia.org/wiki/Histone>`_ tails modifications.
This uni-dimensional information can be projected along the contour of a *block-copolymer* model.
Then, every pairwise of monomers interacts according to the epigenetic states leading to specific pattern of interactions.
The interaction patterns can be visualized using contacts map : two-dimensional map with position along the polymer and a third dimension with color scale for the intensity of contacts.
Since 2000, biologists can produce the same kind of data thanks to the *high-throughput-sequencing* methods 3C, 4C, 5C and Hi-C : `Chromosome-Conformation-Capture <https://en.wikipedia.org/wiki/Chromosome_conformation_capture>`_.
Recently, biologists shown the interactions pattern is correlated with the epigenetic information.
However, the strength and model of interactions between epigenetic states are not always clearly known.

In addition to the *high-throughput-sequencing* methods, we can study the spatial distances inside part of the genome with the help of FISH `<https://en.wikipedia.org/wiki/Fluorescence_in_situ_hybridization>`_ and high-resolution methods.
All the spatial distances can be simply extracted from the model built with the help of OpenMM_Copolymer module.

The module we propose uses the OpenMM software with GPU acceleration to sample as many as possible epigenetic parameters.
It is possible to use effective interactions (gaussian overlap or Lennard-Jones potential) to model the epigenetic.
The module introduces the possibility to replace effective epigenetic interactions with `binders model <https://www.ncbi.nlm.nih.gov/pubmed/22988072>`_ too.
In this case, the binder is like a protein that can bind to a specific site of the genome.
A simple input file is enough to tell the script about the binder-binder and monomer-binder interactions.

The present module assists with simulation of a *block-copolymer* model and assists with the analysis of the data (`HPC Dask <https://dask.org/>`_ and `Python compiler Numba <http://numba.pydata.org/>`_).
In particular, it assists the creation of polymer described by FENE bond and WCA repulsive potential to resolve the excluded volume constraints.
On top of that, it builds the epigenetic interactions based on a simple input file.
It can be used by polymer physicists, biophysicists for epigenetic modeling, to understand the link between epigenetic and tri-dimensional structure of a genome, to estimate first-passage-time encounter of two locii.
It is used in a scientific collaboration to study a specific promoter-enhancer system in the fruit-fly organism (Yad Ghavi-Helm and Cedric Vaillant, ENS Lyon, France).
However, the publication is not currently available.

Background Information
______________________

We use the OpenMM toolkit for molecular dynamics.
We implemented functionalities to build a Kremer-Grest polymer system with uni-dimensional epigenetic information.
We also implement functions to build the quantities biologists extract from `high-throughput-sequencing <https://en.wikipedia.org/wiki/Chromosome_conformation_capture>`_ and FISH `<https://en.wikipedia.org/wiki/Fluorescence_in_situ_hybridization>`_ experiments.
You can find pdf file with a detailed description on the `openmm_copolymer GitLab repository <https://gitlab.e-cam2020.eu/carrivain/copolymer-using-openmm/blob/master>`_.
The module will be constantly improved with new functionalities.

Building and Testing
____________________

The module openmm_plectoneme comes with an example script as well as a test script (using unittest python module).
The `openmm_copolymer GitLab repository <https://gitlab.e-cam2020.eu/carrivain/copolymer-using-openmm/blob/master>`_ gives usages
and results from simple models.

Source Code
___________

The source code can be found on the `openmm_copolymer GitLab repository <https://gitlab.e-cam2020.eu/carrivain/copolymer-using-openmm/blob/master>`_.

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

