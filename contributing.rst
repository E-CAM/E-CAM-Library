.. sidebar:: General Information

    .. contents:: :local:

    * :ref:`search`

.. _contributing:

==================
How to contribute?
==================

This webpage is actually a repository of files that (typically) document application development efforts during the
pilot projects and Extended Software Development Workshops (ESDWs) of E-CAM. This documentation is completely open
however and we welcome both internal and external contributions. If you would like to contribute to this effort then
please follow the steps below to allow us to include your contribution.

In any case you will simply be adding a simple text file that uses ReST_ and we have prepared an example to help
you get started:

* :ref:`example`

You will find the example within the repository of this documentation under the directory *example_module*. You should
make a copy of this directory (renaming it) and place it in the appropriate scientific area directory.

Contribution Guidelines
=======================

GitLab account
--------------

If you do not have a (free) GitLab account yet on the E-CAM GitLab service, you'll need to get one via
https://gitlab.e-cam2020.eu/.

.. note:: SSH public key @ GitLab

  You should also register an SSH public key (if you have not already done so), so you can easily clone, push to and pull from your repositories. This can
  be done via https://gitlab.e-cam2020.eu/profile/keys if you're logged in on GitLab.

In the following it is assumed that an SSH public key (see note above) has been registered, the possibility of using the HTTPS protocol
to access GitLab is not covered (but is possible).

Fork the repository
-------------------

Firstly, you'll need to fork the repository on GitLab you want to work with. Go to
https://gitlab.e-cam2020.eu/e-cam/E-CAM-Library , and click the grey 'Fork' button either beside or under the repository name (or just click this `fork link <https://gitlab.e-cam2020.eu/e-cam/E-CAM-Library/forks/new>`_).

Clone your fork of the repository
---------------------------------

Clone your fork of the repository to your favorite workstation.

.. code-block:: bash

    git clone ssh://git@gitlab.e-cam2020.eu:10022/<Your GitLab username>/E-CAM-Library.git

Pull the master branch from the main repository:

.. code-block:: bash

    cd E-CAM-Library
    git remote add upstream https://git@gitlab.e-cam2020.eu/e-cam/E-CAM-Library.git
    git pull upstream master

Keep your master branch up-to-date
----------------------------------

Make sure you update it every time you create a feature branch (see below):

.. code-block:: bash

    git checkout master
    git pull upstream master

Branching
---------

Pick a branch name for your work that makes sense, so you can track things easily and make sense if you end up having
several branches in flight at once (each PR is a new branch).

Examples:

    ``update_gromacs_module``

    ``new_esdw_lammps_module``

    ``industry_devel_module``


Create a feature branch for your work (after updating your master), and check it out

.. code-block:: bash

    git checkout master
    git branch BRANCH_NAME
    git checkout BRANCH_NAME

Make sure to always base your features branches on master!

If you are working on several things at the same time, try and keep things isolated in separate branches, to keep it
manageable (both for you, and for reviewing your contributions).

Contributing module documentation
---------------------------------

Your contribution to this repository will primarily be a module documentation file (this repository is not for source
code, the documentation file will link to source code which is usually somewhere else). There are already several
examples of these in the repository, but we provide a template for a generic module as a guide:

.. toctree::
    :glob:
    :maxdepth: 1

    ./example_module/readme

After creating the branch, implement your contributions: new modules, enhancements or updates to existing modules, bug
fixes, structure changes, whatever you like. Make sure you commit your work, and try to do it in bite-size chunks, so
the commit log remains clear.

For example:

.. code-block:: bash

    git add modules/gromacs_gpu/readme.rst
    git commit -m "add details on GPU support within GROMACS"

Checking your contribution locally
----------------------------------

You can locally build the documentation to check that the changes you make look as you expect them. To do this you will
need the Sphinx python package to be installed (see this `installation link <http://www.sphinx-doc.org/en/stable/install.html>`_ for
information on how to install this tool on your operating system).

.. code-block:: bash

    make html # in root directory of repository
    firefox _build/html/index.html # Use your browser to view the end result

If you do not have Latex installed on your system you are likely to get related errors. Other (non-latex) errors are
likely to come from your additions. 

Contributing back your input
----------------------------

When you've finished the implementation of a particular contribution, here's how to get it into the main repository.

Push your branch to *your* copy of the repository on GitLab

.. code-block:: bash

    git push origin <BRANCH_NAME>

Issue a *Merge Request* for your branch into the main repository. To do this go to
https://gitlab.e-cam2020.eu/Your_GitLab_Username/E-CAM-Library/merge_requests and select the *New Merge Request*
button.

Make sure the branch you just pushed is selected (not master!) issue a merge request for your branch to the master
branch of the main repository.

Updating your contribution
--------------------------

It is common for there to be updates required to contributions, you do **not** need to open a new Merge Request to do
this. 

To update your contribution you update the appropriate files on your contribution branch. Firstly you need to ensure
that you are up to date with the remote repository on GitLab. Make sure you are in the directory of the cloned
repository and then check which branch you want to check out:

.. code-block:: bash

    git branch # List all available local branches, to include remote branches add the -r flag
    git checkout <BRANCH_NAME> # Check out the branch we want to update 
    git pull origin <BRANCH_NAME> # Make sure we have any updates we made to our own branch
    git pull upstream master # Also pull in any changes to the main repository

Now that everything is in sync, you can edit your update your files, when you are finished you commit your changes and
push the changes back to GitLab:

.. code-block:: bash

    git add modules/gromacs_gpu/readme.rst
    git commit -m "update documentation on how to trigger the GPU support"
    git push origin <BRANCH_NAME>

The Merge  Request will now be automatically updated with the changed files.
    
.. _ReST: http://docutils.sourceforge.net/docs/user/rst/quickref.html
