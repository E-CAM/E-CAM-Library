.. _design:

Designing your software
-----------------------

After some discussion with other `Centres of Excellence <https://exdci.eu/collaboration/coe>`_, we are wary of
over-engineering advice with respect to software design. While tools exist that are designed to assist this process,
such tools typically require repeated use to achieve mastery and this overhead excessive in many cases. For this reason,
we have chosen to align our software specification method with our *acceptance criteria* for software contributions to a
project (source code, testing, documentation, build instructions).

The subsections below are based on the information from `Wikipedia <https://en.wikipedia.org/wiki/Main_Page>`_ (
[SoftwareSpecification]_ and [TDD]_).

.. [SoftwareSpecification] Software requirements specification https://en.wikipedia.org/wiki/Software_requirements_specification
.. [TDD] Test-driven Development https://en.wikipedia.org/wiki/Test-driven_development

Software Requirements Specification
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A software requirements specification (SRS) is a description of a software system to be developed. It lays out
functional and non-functional requirements, and may include a set of use cases that describe user interactions that the
software must provide.

The software requirements specification document lists sufficient and necessary requirements that are required for
developing the project. To derive the requirements, all developers need to have clear and thorough understanding of the
application to be developed. This is achieved through continuous communications between the project team (who are
typically also the *customer* when we are talking about an open source scientific code).

An example of an SRS would be:

* Purpose

  * Definitions
  * System overview
  * References

* Overall description

  * Product perspective

    * System Interfaces
    * User interfaces
    * Hardware interfaces
    * Software interfaces
    * Communication Interfaces
    * Memory Constraints

  * Design constraints

    * Operations
    * Site Adaptation Requirements

  * Product functions
  * User characteristics
  * Constraints, assumptions and dependencies

* Specific requirements

  * External interface requirements
  * Functional requirements
  * Performance requirements
  * Software System attributes

    * Reliability
    * Availability
    * Security
    * Maintainability
    * Portability.

Test-driven Development
^^^^^^^^^^^^^^^^^^^^^^^

The acceptance criteria of E-CAM are explicitly test-focused and we therefore advocate for test-driven development as a
software specification method, where one first decides how a particular development would be tested (and creates the
associated test) before writing the software that would pass this test.

An example of a test-driven development cycle would be:

#. Add a test
#. Run all tests and see if the new test fails
#. Write the code
#. Run tests
#. Refactor code

Such an approach is very task-oriented and if a wider perspective is required (for example if one is beginning a software
project or implementing a redesign) we advise the creation of a `Software Requirements Specification`_ to supplement
this and provide an overarching structure.

In addition, when dealing with numerical methods the creation of adequate tests can be difficult since bit-wise
reproducibility of results is frequently not possible due to floating point precision and/or the use of random numbers.
