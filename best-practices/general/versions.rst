.. _versions:

Version Control
---------------

Version control is a system that records changes to a file or set of files over time so that you can recall specific
versions later. Many people's version-control method of choice is to copy files into another directory (perhaps a
time-stamped directory, if they're clever). This approach is very common because it is so simple, but it is also
incredibly error prone. It is easy to forget which directory you're in and accidentally write to the wrong file or copy
over files you don't mean to.

`Git`_ is a widely used source code management system for software development. As with most other distributed version
control systems, and unlike most client–server systems, every Git working directory is a full-fledged repository with
complete history and full version-tracking capabilities, independent of network access or a central server.

We will use `Git`_ together with the `E-CAM GitLab service`_  within E-CAM. `Git`_ will be our version control utility
and the `E-CAM GitLab service`_ will help us manage our work-flow by allowing us to create/label/follow/assign issues,
review integration of new features, create milestones, etc (but you could also use `GitHub <https://github.com/>`_ to
get the same basic services).

There are many excellent guides to Git, GitLab and GitHub so we will not go into any great detail in this document. For
detailed information we refer you to `Software Carpentry's git lessons <http://swcarpentry.github.io/git-novice/>`_,
the `GitLab documentation <https://docs.gitlab.com/ce/README.html#getting-started-with-gitlab>`_ and the
`GitHub user guides <https://guides.github.com/>`_.

.. _E-CAM GitLab service: https://gitlab.e-cam2020.eu/
.. _Git: https://git-scm.com

Branching Strategy
^^^^^^^^^^^^^^^^^^

We follow the `GitHub Flow`_ branching strategy which is well
described at that link.

.. _GitHub Flow: https://guides.github.com/introduction/flow/

Versioning of Releases
^^^^^^^^^^^^^^^^^^^^^^

Software versioning is the process of assigning either unique version names or unique version numbers to unique states
of computer software. Adopting a logical system for releasing versions provides information to users that allows them,
for example, to predict whether it is *safe* for them to move to a new release.

We follow the `"Semantic Versioning 2.0.0" <http://semver.org/>`_ strategy. Given a version number
``x.y.z (MAJOR.MINOR.PATCH)``, increment the:

* ``MAJOR`` version ``x`` when you make incompatible API changes,
* ``MINOR`` version ``y`` when you add functionality in a backwards-compatible manner, and
* ``PATCH`` version ``z`` when you make backwards-compatible bug fixes.

The approach relies on bumping the correct component up at the right time. Therefore, determining which type of version
you should be releasing is simple. If you are mostly fixing bugs, then this would be categorized as a patch, in which
case you should bump ``z``. If you are implementing new features in a backward-compatible way, then you will bump ``y``
because this is what's called a minor version. On the other hand, if you implement new stuff that is likely to break the
existing API, you need to bump ``x`` because it is a major version.

Additional labels for pre-release and build meta-data are available as extensions to the ``MAJOR.MINOR.PATCH`` format.
We begin at ``MAJOR`` version ``0`` and API changes may be allowed without incrementing it until we are ready to release
a first stable version. Once we release ``MAJOR`` version ``1`` we intend to strictly follow the API policy.

Creating `releases on GitLab <https://docs.gitlab.com/ce/workflow/releases.html>`_ is well described in their
documentation pages.

Creating Citable Code
^^^^^^^^^^^^^^^^^^^^^

`Zenodo <https://zenodo.org/>`_ is a great resource that allows you to get a
`DOI <https://en.wikipedia.org/wiki/Digital_object_identifier>`_ for your code repository.

Zenodo has direct GitHub integration (and there is a `handy guide for how to use GitHub's Zenodo integration
<https://guides.github.com/activities/citable-code/>`_) but the process can also be done manually. You can upload a
zip-ball of your software to Zenodo, provide some metadata and publish it to get a DOI (just as you would if you
uploaded a paper or data). It's probably best if you make a zip-ball of a tagged release, so that the DOI captures
something *complete*. Additionally you can supplement your record metadata on Zenodo with a "related identifier" (e.g. a
URL) and point back to the tag on your live repository. This way anyone who discovers your software in the future will
also have means to follow your live repository and find the most recent version of the software.