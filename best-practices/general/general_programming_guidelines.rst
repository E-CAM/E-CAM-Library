.. _general_guidelines:

General Programming Guidelines
==============================

Language-independent best practices
-----------------------------------

.. toctree::
    :glob:
    :maxdepth: 1

    ./unix-phil

When we develop software in a community we must consider that our work is not just for ourselves but is expected to be
used, adapted and (even) improved by others in that community. Much of :ref:`unix-phil` is worth considering
when developing in such a context.

:ref:`unix-phil` principles apply to the creation of the software, but there are also some universally recommended best
practices when it comes to the software development work-flow itself [BestPractices]_:

* Use a version management tool,
* Make a build in one step,
* Test suites to make sure what you are working on does what you think it should,
* Test-first programming: writing the test for a new line of code before writing that new line of code.

For our specific use case we will support `Git`_ as our version management tool (with
repositories hosted on the `E-CAM GitLab service`_), `CMake <https://cmake.org/>`_ and
`Autotools <https://www.gnu.org/software/automake/manual/html_node/Autotools-Introduction.html#Autotools-Introduction>`_
(complemented by `EasyBuild <http://easybuild.readthedocs.org/>`_ for HPC environments) as our supported build
environments and unit/regression testing and continuous integration through the `E-CAM GitLab service`_.

.. _E-CAM GitLab service: https://gitlab.e-cam2020.eu/
.. _Git: https://git-scm.com

.. [BestPractices] https://en.wikibooks.org/wiki/Computer_Programming/Standards_and_Best_Practices

Development Guidelines
----------------------

Going a little deeper into the specifics, let's consider some advice with respect to a desirable software development
workflow.

.. toctree::
    :glob:
    :maxdepth: 2

    ./specs_and_design
    ./versions
    ./coding_guidelines
    ./continuous_integration
    ./ide