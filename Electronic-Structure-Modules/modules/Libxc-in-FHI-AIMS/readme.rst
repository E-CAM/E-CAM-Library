..  sidebar:: Software Technical Information

  Name
    libxc_in_fhi_aims

  Language
    Fortran, C

  Licence
    Proprietary.

  Documentation Tool
    Doxygen

  Application Documentation
    `FHI-AIMS User Manual <https://mycourses.aalto.fi/pluginfile.php/748992/mod_resource/content/2/FHI-aims-user-manual.pdf>`_

  Relevant Training Material
    Not currently available.

  Software Module Developed by
    Libxc Developers, FHI-AIMS Developers.


.. _libxc_in_fhi_aims:

##########################################################
Support of GGA and MGGA functionals from Libxc in FHI-AIMS
##########################################################

..  contents:: :local:

Libxc provides hundreds of well-tested approaches to calculate interactions between electrons at the atomic scale.
Implementing them individually is long and tedious. With this integration, FHI-AIMS has undergone a paradigm shift
and greatly expanded its capabilities in this domain.


Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

Give a brief overview of why the module is/was being created, explaining a little of the scientific background and how
it fits into the larger picture of what you want to achieve. The overview should be comprehensible to a scientist
non-expert in the domain area of the software module.

This section should also include the following (where appropriate):

* Who will use the module? in what area(s) and in what context?

* What kind of problems can be solved by the code?

* Are there any real-world applications for it?

* Has the module been interfaced with other packages?

* Was it used in a thesis, a scientific collaboration, or was it cited in a publication?

* If there are published results obtained using this code, describe them briefly in terms readable for non-expert users.
  If you have few pictures/graphs illustrating the power or utility of the module, please include them with
  corresponding explanatory captions.

.. note::

  If the module is an ingredient for a more general workflow (e.g. the module was the necessary foundation for later
  code; the module is part of a group of modules that will be used to calculate certain property or have certain
  application, etc.) mention this, and point to the place where you specify the applications of the more general
  workflow (that could be in another module, in another section of this repository, an application’s website, etc.).

.. note::

  If you are a post-doc who works in E-CAM, an obvious application for the module (or for the group of modules that
  this one is part of) is your pilot project. In this case, you could point to the pilot project page on the main
  website (and you must ensure that this module is linked there).

If needed you can include latex mathematics like
:math:`\frac{ \sum_{t=0}^{N}f(t,k) }{N}`
which won't show up on GitLab/GitHub but will in final online documentation.

If you want to add a citation, such as [CIT2009]_, please check the source code to see how this is done. Note that
citations may get rearranged, e.g., to the bottom of the "page".

.. [CIT2009] This is a citation (as often used in journals).


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

Provide the build information for the module here and explain how tests are run. This needs to be adequately detailed,
explaining if necessary any deviations from the normal build procedure of the application (and links to information
about the normal build process needs to be provided).


Source Code
___________

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

