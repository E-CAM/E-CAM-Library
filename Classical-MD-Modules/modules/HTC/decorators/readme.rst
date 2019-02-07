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
    Python

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    In-source documentation

  Application Documentation
    Not currently available.. Example usage provided.

  Relevant Training Material
    Not currently available.

  Software Module Developed by
    Adam Włodarczyk (Wrocław Centre of Networking and Supercomputing),
    Alan O'Cais (Juelich Supercomputing Centre)


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _htc:

#######################################
E-CAM High Throughput Computing Library
#######################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

E-CAM is interested in the challenge
of bridging timescales. To study molecular dynamics with atomistic detail, timesteps must be used on
the order of a femto-second. Many problems in biological chemistry, materials science, and other
fields involve events that only spontaneously occur after a millisecond or longer (for example,
biomolecular conformational changes, or nucleation processes). That means that around :math:`10^{12}` time
steps would be needed to see a single millisecond-scale event. This is the problem of "rare
events" in theoretical and computational chemistry.

Modern supercomputers are beginning to make it
possible to obtain trajectories long enough to observe some of these processes, but to fully
characterize a transition with proper statistics, many examples are needed. In order to obtain many
examples the same application must be run many thousands of times with varying inputs. To manage
this kind of computation a task scheduling high throughput computing (HTC) library is needed. The main elements of mentioned
scheduling library are: task definition, task scheduling and task execution.

While traditionally an HTC workload is looked down upon in the HPC
space, the scientific use case for extreme-scale resources exists and algorithms that require a
coordinated approach make efficient libraries that implement
this approach increasingly important in the HPC space. The 5 Petaflop booster technology of `JURECA <http://www.fz-juelich.de/ias/jsc/EN/Expertise/Supercomputers/JURECA/JURECA_node.html>`_
is an interesting concept with respect to this approach since the offloading approach of heavy
computation marries perfectly to the concept outlined here.

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module is the first in a sequence that will form the overall capabilities of the library. In particular this module
deals with creating a set of decorators to wrap around the `Dask-Jobqueue <https://jobqueue.dask.org/en/latest/>`_
Python library, which aspires to make the overhead of using it lower for our use cases.


Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The initial motivation for this library is driven by the ensemble-type calculations that are required in many scientific
fields, and in particular in the materials science domain in which the E-CAM Centre of Excellence operates. The scope
for parallelisation is best contextualised by the `Dask <https://dask.org/>`_ documentation:

    A common approach to parallel execution in user-space is task scheduling. In task scheduling we break our program
    into many medium-sized tasks or units of computation, often a function call on a non-trivial amount of data. We
    represent these tasks as nodes in a graph with edges between nodes if one task depends on data produced by another.
    We call upon a task scheduler to execute this graph in a way that respects these data dependencies and leverages
    parallelism where possible, multiple independent tasks can be run simultaneously.

    Many solutions exist. This is a common approach in parallel execution frameworks. Often task scheduling logic hides
    within other larger frameworks (Luigi, Storm, Spark, IPython Parallel, and so on) and so is often reinvented.

    Dask is a specification that encodes task schedules with minimal incidental complexity using terms common to all
    Python projects, namely dicts, tuples, and callables. Ideally this minimum solution is easy to adopt and understand
    by a broad community.

While we were attracted by this approach, Dask did not support \emph{task-level} parallelisation (in particular
multi-node tasks). We researched other options (including Celery, PyCOMPSs, IPyParallel and others) and organised a
workshop that explored some of these (see https://www.cecam.org/workshop-0-1650.html for further details).

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

  pytest tests/test_decorators.py

Examples of usage can be found in the ``examples`` directory.

Source Code
___________

The latest version of the library is available on the `jobqueue_features GitHub repository
<https://github.com/E-CAM/jobqueue_features>`_

The code that was originally created specifically for this module can be seen in the specific commit `4590a0e427112f
<https://gitlab.e-cam2020.eu/adam/jobqueue_features/tree/4590a0e427112fbf51edff6116e34c90e765baf0>`_
which can be found in the original private repository of the code.
