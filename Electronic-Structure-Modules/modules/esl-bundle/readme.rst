.. _esl-bundle:

##########
ESL Bundle
##########

..  sidebar:: Software Technical Information

  Language
    The building framework is written in Python. For the languages used in the different modules included in the Bundle,
    please check the corresponding documentation.

  Licence
    The building framework is distributed under the `GPL <https://opensource.org/licenses/gpl-license>`_. 
    For the licenses used in the different modules included in the Bundle, please check the corresponding documentation.

  Documentation Tool
    ReStructuredText

  Application Documentation
    `README <https://gitlab.e-cam2020.eu/esl/esl-bundle/blob/master/README.rst>`_

  Relevant Training Material
    Not currently available.

  Software Module Developed by
    The ESL Bundle was created by Damien Caliste, Alin Marin Elena, Micael Oliveira, and Yann Pouillon. The building
    framework is based on a modified version of JHBuild_, which was written by James Henstridge.

..  contents:: :local:

The ESL Bundle aims at incorporating all the `CECAM Electronic
Structure Library <http://esl.cecam.org>`_ modules into a single
package and using a unified framework for compilation and
installation.


Purpose of Module
_________________

The ESL Bundle is a collection of libraries and utilities broadly
used in electronic structure calculations, put together to make their
use easier by researchers and scientific software developers. It
includes a building framework helping users, developers and packagers
in obtaining a working installation of complex combinations of
software packages without having to track the dependencies themselves.


Installation
____________

The ESL Bundle comes with a version of JHBuild_ which has been tuned to fit
the context of the ESL. JHBuild_ supports a wide variety of build systems,
although it is not a build system itself. It is rather a tool designed to ease
the build of collections of related source packages, that it calls "modules". It
was originally written for the `Gnome Project`_, but its use has then been
extended to other situations.

Most of the operations are performed by executing the ``jhbuild.py`` script with
appropriate parameters. The command line syntax is the following:

  jhbuild.py [global-options] command [command-arguments]


The following global options are available:
  
-f, --file config  Use an alternative configuration file instead of the default
                   ~/.config/jhbuildrc.

-m, --moduleset moduleset  Use a module set other than the module set listed in
                           the configuration file. This option can be a
                           relative path if the module set is located in the
                           JHBuild moduleset folder, or an absolute path if
                           located elsewhere.

--no-interact   Do not prompt the user for any input. This option is useful if
                leaving a build unattended, in order to ensure the build is not
                interrupted.

  
In the ESL Bundle, the default module set is ``esl``. This module set provides
a meta-module called ``esl-bundle``, which builds and installs all the packages
included in the bundle. A second meta-module called ``esl-bundle-mpi`` is
provided, that builds the packages with MPI support. Note that not all packages
can be compiled with MPI support. In that case they will be built without it.

The ``jhbuild.py`` script does not need to be invoked from the directory where
it is located.

.. note::

   To keep the source directory clean, we highly recommended the use of a build
   directory.

Therefore, a typical way of installing the collection of ESL libraries is the
following::
    mkdir my_build_dir
    cd my_build_dir
    ../jhbuild.py build

By default, the ``build`` command will compile all the modules from the
``esl-bundle`` meta-module and install them in the current directory. This, and a
few other options, can be changed in the configuration file. Several sample
configuration files are provided in the ``rcfiles`` directory. These files should
be suitable to build the bundle in a variety of systems, but they can also be
used as a starting point to write configuration files more suited to your needs.

The configuration files use Python syntax. Here is a list of some important
options:

- ``modules``: dictionary of modules to build.
- ``prefix``: directory where the modules should be installed.
- ``checkoutroot``: where to unpack the module's sources.

Configuration options to be passed to the modules build systems can also be
specified in the configuration file. Here is an example of how to do this::
   # Set the FC variable when invoking the configure script for all modules
   autogenargs="FC=gfortran"

   # Run make in parallel with two threads
   makeargs="-j2"

   # Here the futile module requires an extra configuration option.
   # Note that this will overwrite the global options set by autogenargs, so we
   # have to add it here explicitly.
   module_autogenargs['futile'] = "--with-ext-linalg='-lopenblas' " + autogenargs


Source Code
___________

The source code is available from the `E-CAM Gitlab`__ under the `esl-bundle`__
project. The ESL Bundle directory can be found `here`__.

.. __: https://gitlab.e-cam2020.eu/
.. __: https://gitlab.e-cam2020.eu/esl/esl-bundle/
.. __: https://gitlab.e-cam2020.eu/esl/esl-bundle/tree/master/


.. Here are the URL references used (which is alternative method to the one described above)

.. _`Gnome Project`: https://www.gnome.org/
.. _JHBuild: https://developer.gnome.org/jhbuild/stable/
