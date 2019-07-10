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
    pybop

  Language
    Python (2.7, 3.4, 3.5, 3.6)

  Licence
    `GNU General Public License v3.0 <https://www.gnu.org/licenses/gpl-3.0.en.html>`_

  Documentation Tool
    Sphinx/RST

  Application Documentation
    https://srmnitc.github.io/pybop/html/index.html

  Relevant Training Material
    https://mybinder.org/v2/gh/srmnitc/pybop/master?filepath=examples%2F

  Software Module Developed by
    Sarath Menon


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _example:

#####
pybop
#####

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

**pybop** is a python module for calculation of bond orientational order parameters [#]_. The core functionality of **pybop** is written in C++ with python wrappers using `pybind11 <https://pybind11.readthedocs.io/en/stable/intro.html>`_ . This allows for fast calculations with possibilities for seamless expansion in python. 

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment



Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment



Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment



Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!



.. [#]  Steinhardt, PJ, Nelson, DR, Ronchetti, M. PRB 28, 1983.