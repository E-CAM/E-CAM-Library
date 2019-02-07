..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    ``jobqueue_features``

  Language
    Python, YAML

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    In-source documentation

  Application Documentation
    Not currently available. Example usage provided.

  Relevant Training Material
    Not currently available.

  Software Module Developed by
    Adam Włodarczyk (Wrocław Centre of Networking and Supercomputing),
    Alan O'Cais (Juelich Supercomputing Centre)


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _htc_mpi:

####################
HTC Multi-node Tasks
####################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

This module is the second in a sequence that will form the overall capabilities of the HTC library (see :ref:`htc_yaml`
for the previous module). This module deals with enabling tasks to be run over a set of nodes (specifically MPI/OpenMP
tasks).

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The initial goal is to allow the HTC library to control tasks that are executed via the MPI launcher command. The task
tracked by Dask is actually the process created by the launcher. The launcher is a forked process from within the
library.

The implementation is intended to be generic but the specific example implementation provided is for ``srun`` launcher
that is used on
`JURECA <http://www.fz-juelich.de/ias/jsc/EN/Expertise/Supercomputers/JURECA/JURECA_node.html>`_ system.


Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module builds upon the work described in :ref:`htc_yaml`.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The library is a Python module and can be installed with

::

  python setup.py install

More details about how to install a Python package can be found at, for example, `Install Python packages on the
research computing systems at IU <https://kb.iu.edu/d/acey>`_

To run the tests for the decorators within the library, you need the ``pytest`` Python package. You can run all the
relevant tests from the ``jobqueue_features`` directory with

::

  pytest tests/test_mpi_wrapper.py

Specific examples of usage for the JURECA system are available in the ``examples`` subdirectory.

Source Code
___________

The latest version of the library is available on the `jobqueue_features GitHub repository
<https://github.com/E-CAM/jobqueue_features>`_

The code that was originally created specifically for this module can be seen in the
`HTC/MPI Merge Request <https://gitlab.e-cam2020.eu/adam/jobqueue_features/merge_requests/5>`_ which can be found in
the original private repository of the code. Additional, more complex, examples were provided in the
`HTC/MPI examples Merge Request <https://gitlab.e-cam2020.eu/adam/jobqueue_features/merge_requests/7>`_
