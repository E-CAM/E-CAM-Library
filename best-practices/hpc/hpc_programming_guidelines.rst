.. _hpc_guidelines:

HPC Programming Guidelines
==========================

Once we begin to discuss high performance computing (HPC), we necessarily must begin to discuss not only the latest
hardware technologies, but also the latest software technologies that make exploiting the capabilities of that hardware
easier.

Hardware Developments
---------------------

There are a number of different organisations and projects that are generating roadmaps about the current and future
technologies that are (or may be in the future) available in the HPC space. `Eurolab-4-HPC
<https://www.eurolab4hpc.eu/>`_ has summarised many of these in the `Eurolab-4-HPC Long-Term Vision on
High-Performance Computing <https://www.eurolab4hpc.eu/static/deliverables/D2-2--final-vision.3718a25ff0dd.pdf>`_. Here
we focus on a small subset of the content of these roadmaps (primarily from `Eurolab-4-HPC
<https://www.eurolab4hpc.eu/>`_ and `ETP4HPC <http://www.etp4hpc.eu/>`_) that are most likely to impact the target
community of E-CAM in the 3-5 year horizon.


.. toctree::
    :glob:
    :maxdepth: 2

    ./hpc-hardware
    ./euro-hardware-roadmap

Software Developments
---------------------

It is clear that the hardware developments described above will greatly impact the software development practices of
the E-CAM development community. For this reason, we highlight the the language standards, runtime environments,
workflows and software tools that can help E-CAM developers to deliver high quality, resilient software for current
and next generation machines.

.. toctree::
    :glob:
    :maxdepth: 2

    ./programming-hpc

Accessing Resources
-------------------

All of the above is academic unless you have access to resources to develop and test new software. There are many
potential ways to access HPC resources, we simply highlight a limited set of the possibilities here.

.. toctree::
    :glob:
    :maxdepth: 2

    ./hpc-resources
