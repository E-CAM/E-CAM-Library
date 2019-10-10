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
    Code Optimization for Exact and Linearized Quantum Dynamics

  Language
    Fortran

  Licence
    GNU Lesser General Public License (http://www.gnu.org/licenses/)

  Documentation Tool
    Doxygen

  Application Documentation
    Not currently available.

  Relevant Training Material
    Not currently available.

  Software Module Developed by
    David Lauvergnat, Mamadou Ndong, Josep Maria Luis, etc.

..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _example:

################################
E-CAM Quantum_Smolyak_MPI module
################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

This module is a MPI parallelization of quantum dynamics code applying the Smolyak algorithm, designed for the quantum simulation of molecules. 

Purpose of Module
_________________

The module is intended to provide an efficient parallelization of quantum ab initial simulation with Smolyak algorithm. The final code will be available for switching between MPI and openMP according to the interested system and the cluster resource available. A MPI-openMP hybrid implementation might be also available according to the progress. 

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

.. Give a brief overview of why the module is/was being created, explaining a little of the scientific background and how
.. it fits into the larger picture of what you want to achieve. The overview should be comprehensible to a scientist
.. non-expert in the domain area of the software module.

.. This section should also include the following (where appropriate):

.. * Who will use the module? in what area(s) and in what context?

.. * What kind of problems can be solved by the code?

.. * Are there any real-world applications for it?

.. * Has the module been interfaced with other packages?

.. * Was it used in a thesis, a scientific collaboration, or was it cited in a publication?

.. * If there are published results obtained using this code, describe them briefly in terms readable for non-expert users.
  If you have few pictures/graphs illustrating the power or utility of the module, please include them with
  corresponding explanatory captions.

.. note::

..  If the module is an ingredient for a more general workflow (e.g. the module was the necessary foundation for later
..  code; the module is part of a group of modules that will be used to calculate certain property or have certain
..  application, etc.) mention this, and point to the place where you specify the applications of the more general
..  workflow (that could be in another module, in another section of this repository, an application’s website, etc.).

.. note::

..  If you are a post-doc who works in E-CAM, an obvious application for the module (or for the group of modules that
..  this one is part of) is your pilot project. In this case, you could point to the pilot project page on the main
..  website (and you must ensure that this module is linked there).

.. If needed you can include latex mathematics like
.. :math:`\frac{ \sum_{t=0}^{N}f(t,k) }{N}`
.. which won't show up on GitLab/GitHub but will in final online documentation.

.. If you want to add a citation, such as [CIT2009]_, please check the source code to see how this is done. Note that
..  citations may get rearranged, e.g., to the bottom of the "page".

.. [CIT2009] This is a citation (as often used in journals).

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module is an MPI implementation of the quantum dynamics with Smolyak algorithm. One should refer to the relevant module for more details of the basic algorithms. 

.. If the modifications are to an existing code base (which is typical) then this would be the place to name that
.. application. List any relevant urls and explain how to get access to that code. There needs to be enough information
.. here so that the person reading knows where to get the source code for the application, what version this information is
.. relevant for, whether this requires any additional patches/plugins, etc.

.. Overall, this module is supposed to be self-contained, but linking to specific URLs with more detailed information is
.. encouraged. In other words, the reader should not need to do a websearch to understand the context of this module, all
.. the links they need should be already in this module.

Building and Testing
____________________

Building the program requires Open MPI v3.0 or above. Open MPI should be built as 64-bit for a full access of the functions.

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

.. Provide the build information for the module here and explain how tests are run. This needs to be adequately detailed,
.. explaining if necessary any deviations from the normal build procedure of the application (and links to information
.. about the normal build process needs to be provided).

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

.. Here link the source code *that was created for the module*. If you are using Github or GitLab and the `Gitflow Workflow
.. <https://www.atlassian.com/git/tutorials/comparing-workflows#gitflow-workflow>`_ you can point to your feature branch.
.. Linking to your pull/merge requests is even better. Otherwise you can link to the explicit commits.

.. * `Link to a merge request containing my source code changes
..  <https://github.com/easybuilders/easybuild-easyblocks/pull/1106>`_

.. There may be a situation where you cannot do such linking. In this case, I'll go through an example that uses a patch
.. file to highlight my source code changes, for that reason I would need to explain what code (including exact version
.. information), the source code is for.

.. You can create a similar patch file by (for example if you are using git for your version control) making your changes
.. for the module in a feature branch and then doing something like the following:

..  Don't forget the white space around the "literal block" (a literal block keeps all spacing and is a good way to
    include terminal output, file contents, etc.)

::

..  [adam@mbp2600 example (master)]$ git checkout -b tmpsquash
..  Switched to a new branch "tmpsquash"

..  [adam@mbp2600 example (tmpsquash)]$ git merge --squash newlines
..  Updating 4d2de39..b6768b2
..  Fast forward
..  Squash commit -- not updating HEAD
..   test.txt |    2 ++
..   1 files changed, 2 insertions(+), 0 deletions(-)

..  [adam@mbp2600 example (tmpsquash)]$ git commit -a -m "My squashed commits"
..  [tmpsquash]: created 75b0a89: "My squashed commits"
..   1 files changed, 2 insertions(+), 0 deletions(-)

..  [adam@mbp2600 example (tmpsquash)]$ git format-patch master
..  0001-My-squashed-commits.patch


.. To include a patch file do something like the following (take a look at the source code of this document to see the
.. syntax required to get this):

..  Below I am telling Sphinx that the included file is C code, if possible it will then do syntax highlighting. I can
    even emphasise partiuclar lines (here 2 and 9-11)

.. .. literalinclude:: ./simple.patch
..      :language: c
..      :emphasize-lines: 2,9-11
..      :linenos:


..  I can't highlight the language syntax of a patch though so I have to exclude
..    :language: c

.. literalinclude:: ./simple.patch
..   :emphasize-lines: 2,9-11
..   :linenos:

.. If the patch is very long you will probably want to add it as a subpage which can be done as follows

.. toctree::
..   :glob:
..   :maxdepth: 1

..   patch

..  Remember to change the reference "patch" for something unique in your patch file subpage or you will have
..    cross-referencing problems

.. you can reference it with :ref:`patch`

.. Here are the URL references used (which is alternative method to the one described above)

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html

