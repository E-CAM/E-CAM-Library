..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    European Environment for Scientific Software Installations

  Language
    Ansible for infrastructure, Python for installation framework

  Licence
    `GPL v2 <https://opensource.org/licenses/GPL-2.0>`_

  Documentation Tool
    Markdown (and `MkDocs <https://www.mkdocs.org/>`_)

  Application Documentation
    https://eessi.github.io/docs/

  Relevant Training Material
    https://github.com/EESSI/eessi-demo

  Software Module Developed by
    Alan O'Cais (for contribution described here)

.. _eessi_singularity:

######################################
MPI support for EESSI-based containers
######################################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

The European Environment for
Scientific Software
Installations (`EESSI <https://eessi.github.io/docs/>`_) is a collaboration
between a number of academic and industrial partners in the HPC community to set up a
shared stack of scientific software installations
to avoid the installation and execution of
sub-optimal applications on HPC resources. The software stack is
intended to work on laptops, personal workstations, HPC clusters and in the cloud,
which means the project will need to support different CPUs, networks, GPUs, and so on.


EESSI can be used through via containers, however this requires some additional settings
for MPI workloads. This module outlines the creation of an initialisation script that
can facilitate this while also catering to systems which have no direct connection to
the internet.

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The EESSI architecture is built upon the
`CernVM-FS distributed file system <https://cernvm.cern.ch/fs/>`_ which provides a
scalable, reliable and low-maintenance software distribution service. CernVM-FS uses
a cache so that a client only ever has local copies of the files it actually needs. The
cache is populated over the ``http`` protocol.

If CernVM-FS is not available or configured where a user would like to use EESSI, it is
still possible to use EESSI via a `Singularity <https://sylabs.io/>`_ container. The
container approach, however, requires additional configuration when considering MPI
workloads.

In addition, there are many cases where worker nodes in HPC systems have no connection
to the outside world, which makes it impossible for them to populate their CernVM-FS
cache.

This module describes a script created to address both of these issues.

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The European Environment for
Scientific Software
Installations `EESSI <https://eessi.github.io/docs/>`_ is a collaboration
between a number of academic and industrial partners in the HPC community. Through the
EESSI project, they want to set up a shared stack of scientific software installations
to avoid not only duplicate work across HPC sites but also the execution of
sub-optimal applications on HPC resources.

The software stack is
intended to work on laptops, personal workstations, HPC clusters and in the cloud,
which means the project will need to support different CPUs, networks, GPUs, and so on.
When using singularity containers which leverage EESSI on HPC systems there are
additional requirements to ensure that MPI workloads can be correctly launched and run.

Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The script itself can be downloaded as described in the next section. It includes
extensive
commenting and, at the time of writing, is configured to use the ``2020.12`` version of
the EESSI pilot software stack. You should configure settings in the script according
to the system you have access to.

The script creates two layers of caching for CernVM-FS, a global one and a per-node
cache. The script should be run from a location that has external internet access and
access to the shared file system of the HPC resource. The script will inspect the
architecture where it is run, and fully pre-populate the cache with the software stack
for that architecture. The per-node cache is then dynamically populated from the global
cache.

After running the script, it will tell the user to set a number of environment
variables, e.g.,

.. code-block:: bash

  export EESSI_CONFIG="container:cvmfs2 cvmfs-config.eessi-hpc.org /cvmfs/cvmfs-config.eessi-hpc.org"
  export EESSI_PILOT="container:cvmfs2 pilot.eessi-hpc.org /cvmfs/pilot.eessi-hpc.org"
  export SINGULARITY_HOME="/p/project/cecam/singularity/cecam/ocais1/home:/home/ocais1"
  export SINGULARITY_BIND="/p/project/cecam/singularity/cecam/alien_2020.12:/shared_alien,/tmp:/local_alien,/p/project/cecam/singularity/cecam/ocais1/home/default.local:/etc/cvmfs/default.local"
  export SINGULARITY_SCRATCH="/var/lib/cvmfs,/var/run/cvmfs"


The EESSI pilot stack includes GROMACS and in the following command we show how one
can execute a GROMACS benchwork using the installation found inside EESSI (on
`JUWELS <https://www.fz-juelich.de/ias/jsc/EN/Expertise/Supercomputers/JUWELS/Configuration/Configuration_node.html>`_):

.. code-block:: bash

  [juwels01 ~]$ SLURM_MPI_TYPE=pspmix OMP_NUM_THREADS=2 \
                srun --time=00:05:00 --nodes=1 --ntasks-per-node=24 --cpus-per-task=2 \
                singularity exec --fusemount "$EESSI_CONFIG" --fusemount "$EESSI_PILOT" \
                ~/client-pilot_centos7-2020.08.sif \
                /cvmfs/pilot.eessi-hpc.org/2020.12/software/x86_64/intel/skylake_avx512/software/GROMACS/2020.1-foss-2020a-Python-3.8.2/bin/gmx_mpi \
                mdrun -s ion_channel.tpr -maxh 0.50 -resethway -noconfout -nsteps 10 -g logfile

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

EESSI is still in a pilot phase, and for this reason the final version of this script
cannot be created until the underlying requirements have stabilised. For the time being
the script is contained in an
`issue in the EESSI filesystem layer repository <https://github.com/EESSI/filesystem-layer/issues/37#issue-701122823>`_.

.. _ReST: http://www.sphinx-doc.org/en/stable/rest.html
.. _Sphinx: http://www.sphinx-doc.org/en/stable/markup/index.html
