.. _coding-guidelines:

Coding Guidelines and Code Review
---------------------------------

While this document can be used as a starting point for coding best practices, it is really intended for those who want
to contribute code to the E-CAM project, and describes the starting point for our coding standards and code review
checklist. We try to adopt best practices from existing projects with plenty of relative experience. [PyCogent]_ (for
example) has useful coding guidelines that will act as a starting point for us.

Code, scripts, and documentation should have their spelling checked. All plain-text files should have line widths of 120
characters or less unless that is not supported for the particular file format. It is typical in many projects that a
limit of 80 characters is given but we consider this excessively restrictive.

.. [PyCogent] http://pycogent.org/coding_guidelines.html

Variable naming
^^^^^^^^^^^^^^^

* Choose the name that people will most likely guess. Make it descriptive, but not too long: ``curr_record`` is better
  than ``c``, or ``curr``, or ``current_genbank_record_from_database``.
* Good names are hard to find. Don't be afraid to change names except when they are part of interfaces that other people
  are also using. It may take some time working with the code to come up with reasonable names for everything: if you
  have unit tests, its easy to change them, especially with global search and replace.
* Use singular names for individual things, plural names for collections. For example, you'd expect ``self.Name`` to
  hold something like a single string, but ``self.Names`` to hold something that you could loop through like a list or
  dict. Sometimes the decision can be tricky: is ``self.Index`` an int holding a position, or a dict holding records
  keyed by name for easy lookup? If you find yourself wondering these things, the name should probably be changed to
  avoid the problem: try ``self.Position`` or ``self.LookUp``.
* Don't make the type part of the name. You might want to change the implementation later. Use ``Records`` rather than
  ``RecordDict`` or ``RecordList``, etc. Don't use Hungarian Notation either (i.e. where you prefix the name with the
  type).
* Make the name as precise as possible. If the variable is the name of the input file, call it ``infile_name``, not
  ``input`` or ``file`` (which you shouldn't use anyway, since they're keywords), and not ``infile`` (because that looks
  like it should be a file object, not just its name).
* Use ``result`` to store the value that will be returned from a method or function. Use ``data`` for input in cases
  where the function or method acts on arbitrary data (e.g. sequence data, or a list of numbers, etc.) unless a more
  descriptive name is appropriate.
* One-letter variable names should only occur in math functions or as loop iterators with limited scope. Limited scope
  covers things like ``for k in keys: print k``, where ``k`` survives only a line or two. Loop iterators should refer to
  the variable that they're looping through: ``for k in keys, i in items``, or ``for key in keys, item in items``. If
  the loop is long or there are several 1-letter variables active in the same scope, rename them.
* Limit your use of abbreviations. A few well-known abbreviations are OK, but you don't want to come back to your code
  in 6 months and have to figure out what ``sptxck2`` is. It's worth it to spend the extra time typing
  ``species_taxon_check_2``, but that's still a horrible name: what's check number 1? Far better to go with something
  like ``taxon_is_species_rank`` that needs no explanation, especially if the variable is only used once or twice.

Naming Conventions
^^^^^^^^^^^^^^^^^^

It is important to follow the naming conventions because they make it much easier to guess what a name refers to. In
particular, it should be easy to guess what scope a name is defined in, what it refers to, whether it's OK to change its
value, and whether its referent is callable. The following rules provide these distinctions.

* ``lowercase_with_underscore`` for modules and internal variables (including function/method parameters).
* ``MixedCase`` for classes and public properties, and for factory functions that act like additional constructors for a
  class.
* ``mixedCaseExceptFirstWord`` for public methods and functions.
* ``_lowercase_with_leading_underscore`` for private functions, methods, and properties.
* ``__lowercase_with_two_leading_underscores`` for private properties and functions that must not be overwritten by a
  subclass.
* ``CAPS_WITH_UNDERSCORES`` for named constants.

Underscores can be left out if the words read OK run together. ``infile`` and ``outfile`` rather than ``in_file`` and
``out_file``; ``infile_name`` and ``outfile_name`` rather than ``in_file_name`` and ``out_file_name`` or ``infilename``
and ``outfilename`` (getting too long to read effortlessly).

Merge Requests
^^^^^^^^^^^^^^

We want people who contribute back to use a "branch-hack-pull request" cycle, the `GitHub Flow`_. Our website contains
greater detail on the exact steps required (at :ref:`contributing`) but the basic concept is:

* Create your own copy of the repository on the `E-CAM GitLab service`_ (``fork``)
* ``clone`` your repository to your machine
* Create a new ``branch`` for your feature. Feature branches should have descriptive names, like ``animated-menu-items``
  or ``issue-#1061``.
* Hack
* ``push`` your changes back to GitLab
* Create a `Merge Request <https://docs.gitlab.com/ee/gitlab-basics/add-merge-request.html>`_ [#f1]_ against the
  appropriate  of the E-CAM library. This gives other developers an opportunity to review the changes before they become
  a part of the main codebase.

Code review (see below) is a major benefit of merge requests, but merge requests are actually designed to be a generic
way to talk about code. You can think of merge requests as a discussion dedicated to a particular branch. This means
that they can also be used much earlier in the development process. For example, if a developer needs help with a
particular feature, all they have to do is file a merge request. Interested parties will be notified automatically, and
they'll be able to see the question right next to the relevant commits.

Code Review and Checklist
^^^^^^^^^^^^^^^^^^^^^^^^^

Contributors also make good reviewers so we'd like you to be aware of what the review process looks like. We try to
follow the best practices as described in `"11 Best Practices for Peer Code Review"
<http://smartbear.com/SmartBear/media/pdfs/WP-CC-11-Best-Practices-of-Peer-Code-Review.pdf>`_. The most important to
mention are:

* Merge requests should be small, the target is to review fewer than 400 lines of code at a time.
* Authors should document source code before the review.
* Embrace the subconscious implications of peer review. The knowledge that others will be examining their work naturally
  drives people to produce a better product.

Copy and paste the following into a merge request comment when it is ready for review (in our case that will be on
the `E-CAM GitLab service`_). This lists helps ensure that we try to reach many of our targets in terms of::

    - [ ] Is it mergeable? (i.e., there should be no merge conflicts)
    - [ ] Did it pass the tests? (Are there unit/regression tests? Do they pass?)
    - [ ] If it introduces new functionality, is it tested? (Unit tests?)
    - [ ] Is it well formatted? (typos, line length, brackets,...)
    - [ ] Did it change any interfaces? Only additions are allowed without a major version
          increment. Changing file formats also requires a major version number increment.
    - [ ] Is the Copyright year up to date?

.. note:: After you submit the comment you can check and uncheck the individual boxes on the formatted comment in
          GitLab; no need to put x or y in the middle.

.. _E-CAM GitLab service: https://gitlab.e-cam2020.eu/
.. _GitHub Flow: https://guides.github.com/introduction/flow/

.. rubric:: Footnotes

.. [#f1] Merge requests let you tell others about changes you've pushed to a GitLab repository. Once a merge request is
         sent, interested parties can review the set of changes, discuss potential modifications, and even push
         follow-up commits if necessary
