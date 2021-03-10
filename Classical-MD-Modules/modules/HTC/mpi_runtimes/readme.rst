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
    Alan O'Cais (Juelich Supercomputing Centre)
    Adam Włodarczyk (Wrocław Centre of Networking and Supercomputing),
    Miłosz Białczak (Wrocław Centre of Networking and Supercomputing),


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _htc_mpi_runtimes:

############################################
Extending available MPI runtime environments
############################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

This module is another in a sequence that form the overall capabilities of the HTC library (see :ref:`htc_mpi4py`
for the most relevant previous module where support for forked MPI workloads was added). This module adds support for
additional MPI runtimes to make the library a more portable solution between HPC systems.

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module extends the supported MPI runtimes of ``jobqueue_features``, beyond the original SLURM and ``mpiexec``, to
OpenMPI, Intel MPI and MPICH. This support includes the relevant arguments to provide reasonable process pinning
arguments to the runtimes based on the system architecture and resources requested for each worker.

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

To date, we have only included MPI launchers that do not require complex configuration (`srun` and `mpiexec`). In order
to extend the supported MPI launchers we also need to be able to take into account the distribution of processes and
threads by the launcher. We have this information since it is dictated by the system configuration file and the
arguments the user provides when creating the Dask cluster to which they submit their tasks.

The main goal here is to make a best effort mapping between the user request and the MPI launcher options that will
distribute and pin the processes/threads across the target system.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The library is a Python module and can be installed with

::

  python setup.py install

More details about how to install a Python package can be found at, for example, `Install Python packages on the
research computing systems at IU <https://kb.iu.edu/d/acey>`_

To run the tests for the MPI launchers within the library, you need the ``pytest`` Python package. You can run all the
relevant tests from the ``jobqueue_features`` directory with

::

  pytest tests/test_mpi_wrapper.py

Source Code
___________

The latest version of the library is available on the `jobqueue_features GitHub repository
<https://github.com/E-CAM/jobqueue_features>`_

The code that was originally created specifically for this module can be seen in the
`Merge Request that added support for OpenMPI and Intel MPI,  <https://github.com/E-CAM/jobqueue_features/pull/34>`_ and
the `Merge Request that added support for MPICH <https://github.com/E-CAM/jobqueue_features/pull/55>`_.
