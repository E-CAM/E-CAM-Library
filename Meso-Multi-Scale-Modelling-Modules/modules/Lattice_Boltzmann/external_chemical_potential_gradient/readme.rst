..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    Ludwig: A lattice Boltzmann code for complex fluids

  Language
    C

  Licence
    `<https://github.com/ludwig-cf/ludwig/blob/master/LICENSE>`_

  Documentation Tool
    LaTex-generated pdf

  Application Documentation
    `<https://github.com/ludwig-cf/ludwig/tree/master/docs/tutorial>`_

..  Relevant Training Material
    Add a link to any relevant training material. If there currently is none then say 'Not currently available.'

..  Software Module Developed by
    Add the name of the person who developed the software for this module here


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. .. _example:

#######################################################################
Externally imposed chemical potential gradient for binary fluid mixture
#######################################################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

We present a module that implements an externally imposed chemical potential gradient to the Lattice Boltzmann code
`Ludwig <https://github.com/ludwig-cf/ludwig>`_. The gradient is further used in simulation of binary fluid mixture and enables the studies of the related flows
in various porous materials.

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The gradient of chemical potential in a binary fluid mixture gives rise to a flow, whenever the value of the order
parameter differs from 0. In Ludwig, we first implement the gradient as a vectorial physical property, which can,
similarly to all other physical properties, be used anywhere in the code. Further on, we use it in the context of binary
fluid mixture in the subroutines that account for the time evolution of the order parameter (Cahn-Hilliard equation)
and the force, that arises due to the non-zero chemical potential gradient and order parameter.

This module is essential
for studying binary fluid flows in porous materials as well as flows, arising due to the wetting effect of the walls in
nanochannels.

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module implements the externally imposed chemical potential gradient (for binary fluid mixture) in the Ludwig code.
The latter, together with its documentation and tutorial is available on the following link:
`<https://github.com/ludwig-cf/ludwig>`_.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The module is built and run in the same way as any other simulation in Ludwig. A detailed description of the latter is
available at: `<https://github.com/ludwig-cf/ludwig/tree/master/docs/tutorial>`_.

Specifically, an example of the input
file for a binary fluid simulation with externally imposed chemical potential gradient is available at:
`<https://github.com/ludwig-cf/ludwig/blob/develop/tests/regression/d3q19-short/serial-muex-st1.inp>`_.
The externally imposed chemical potential gradient is specified in the input file, by the following lines:

.. code:: bash

    fd_force_divergence 0
    grad_mu 0.00001_0.00002_0.00003

Source code
___________

The related pull requests in Ludwig's github repository can be found at
`<https://github.com/ludwig-cf/ludwig/pull/80>`_
and `<https://github.com/ludwig-cf/ludwig/pull/88>`_.

The changes have been incorporated into the Ludwig's new versions,
for the first time within the release/version ``0.11.0`` (see 
`<https://github.com/ludwig-cf/ludwig/blob/master/CHANGES.md>`_).


