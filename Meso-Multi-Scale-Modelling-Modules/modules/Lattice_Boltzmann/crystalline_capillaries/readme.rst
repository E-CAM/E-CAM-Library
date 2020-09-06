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

####################################################################################################
Implementation of simple cubic, body-centered cubic, and face-centered cubic crystalline capillaries
####################################################################################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

We present a module that implements simple cubic (SCC), body-centered cubic (BCC), and face-centered cubic (FCC)
crystalline geometries as a utility to create capillaries in the Lattice Boltzmann code Ludwig. The thus created
crystalline geometries are used as porous materials in Lattice Boltzmann simulations.

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The crystalline structures represent various types of porous materials. We can attribute to them wetting properties,
via which the solid parts of the geometries interact with the fluid. This module is essential for the studies of various
types of flows through such materials. In particular, we focus on flows of binary fluid mixtures, driven by an
externally imposed chemical potential gradient, through porous media.

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module implements the SCC, BCC, and FCC crystalline capillaries in the Ludwig code. The latter, together with its
documentation and tutorial is available on the following link: `<https://github.com/ludwig-cf/ludwig>`_.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The module is run by specifying the name of the output file, the output type, the crystalline type, the size of the
crystalline cell, and the size of the whole system in the file "capillary.c" (`<https://github.com/ludwig-cf/ludwig/blob
/master/util/capillary.c>`_). The file is then compiled and run by:

.. code:: bash

    make capillary
    ./capillary

This generates the output file (e.g. "capillary.001-001"). The thus generated capillary file, together with the desired
output data, is then specified in the input file of the simulation. An example of this specification is:

.. code:: bash

    porous_media_format BINARY
    porous_media_file	capillary
    porous_media_type	status_with_c_h

The latter line "porous_media_type" should match the output type, defined in the "capillary.c" file, prior to its
compilation.

Source code
___________

The module has been provided as a pull request on Ludwig's github repository `<https://github.com/ludwig-cf/ludwig/pull/
98>`_.


