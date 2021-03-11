..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    A Load Balancing Library (ALL) - C++ interface

  Language
    C++, Fortran interfaces available

  Licence
    `BSD 3-Clause <https://choosealicense.com/licenses/bsd-3-clause/>`_

  Documentation Tool
    In source provided by Doxygen, additional using Sphinx

  Application Documentation
    http://slms.pages.jsc.fz-juelich.de/websites/all-website/sphinx/api/ALL.html

  Relevant Training Material
    `Webinar (YT) <https://www.youtube.com/watch?v=cUdvsQyxVh0&list=PLmhmpa4C4MzY02eaacXImTts2aGJHrdwQ>`_

  Software Module Developed by
    Rene Halver, Stephan Schulz


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _all_cpp_interface:

#####################
ALL C++ interface
#####################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

Since C++ becomes more common in the HPC environment, therefore the default
interface for ALL is written in that language. The library uses class inheritance
to administrate the different load-balancing methods. Every necessary functionality
to use the library is provided by the interface. In addition there is a Fortran
interface provided (description in module :ref:`all_fortran_interface`).

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module is necessary for all users to couple their code with the ALL library.

It is currently used in all projects, which already include the ALL library to their code and
are based on C and C++.

.. TODO:

.. * If there are published results obtained using this code, describe them briefly in terms readable for non-expert users.
  If you have few pictures/graphs illustrating the power or utility of the module, please include them with
  corresponding explanatory captions.

.. If you want to add a citation, such as [CIT2009]_, please check the source code to see how this is done. Note that
.. citations may get rearranged, e.g., to the bottom of the "page".

.. .. [CIT2009] This is a citation (as often used in journals).

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The interface is part of ALL which can be found at
https://gitlab.version.fz-juelich.de/SLMS/loadbalancing in the ``include``
subdirectory. It is called ``ALL.hpp``

Technical Details
_________________

The C++ interface is a header-based solution, as it uses C++ templating capabilities
to support different types of floating point numbers to describe domain borders and
work loads. Internally class inheritance is used to administrate the different included
load-balnacing methods. This should provide an easy way to include future additions of
new methods into the library.

Building
________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

General installation instructions for ALL can be found in the 
`README.md of the ALL repository <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing#installation-and-requirements>`_ 
(which is also distributed with each release).

As the C++ interface is the default interface of the ALL library, there is
no need to explicitly enable it. The interface provides functions to create
an object which handles the computations of new boundaries based on provided
sets of domain borders and work loads. In addition, if the library is compiled
with VTK support (requires ``CM_ALL_VTK_OUTPUT`` in CMake), functionality to
create VTK descriptions of the domain structure is provided (for all methods
working on orthogonal domains).

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

The source code for this interface consists of:
`include/ALL.hpp <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/-/blob/master/include/ALL.hpp>`_


.. vim: et sw=2 ts=2 tw=74 spell spelllang=en_us:
