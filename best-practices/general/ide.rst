.. _ide:

Integrated Development Environment
----------------------------------

An Integrated Development Environment (IDE) is not a *necessary* development tool but it is a useful one. While we
understand that people are familiar with a particular editor or work-flow, use of an IDE can help the development
process flow more easily. An IDE is a software application that provides comprehensive facilities to computer
programmers for software development. An IDE normally consists of a source code editor, build automation tools and a
debugger. We are making this recommendation purely because IDEs can save you lot of **time**, particularly when you are
contributing to someone else's code. There are many good reasons to use one:

* Integrated source control (this is major since the Git UI is frequently not intuitive)
* Quickly navigating to a type without needing to worry about namespace, project etc.
* Navigating to members by treating them as hyperlinks
* Auto-completion when you can't remember the names of all members by heart
* Automatic code generation
* Refactoring (major advantage)
* Warning-as-you-type (i.e. some errors don't even require a compile cycle)
* Hovering over something to see the documentation (provided by Doxygen)
* Keeping a view of files, errors/warnings/console/unit tests etc. and source code all on the screen at the same time in
  a useful way
* Ease of running unit tests from the same window
* Integrated debugging
* Navigating to where a compile-time error or run-time exception occurred directly from the error details.

If you are developing for a parallel environment for multiple HPC systems the `Eclipse IDE <https://eclipse.org/ide/>`_
even has a plugin specifically designed for this, the `Eclipse Parallel Tools Platform <https://eclipse.org/ptp/>`_.
