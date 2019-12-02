.. _OpenQubit:

#########
OpenQubit
#########

.. sidebar:: Software Technical Information

  Language
    Python 3.5

  License
    MIT license (MIT)

  Documentation Tool
    sphinx

  Software Module Developed by
    Momir Mališ

.. contents:: :local:


Purpose of Module
_________________

**OpenQubit** is a patch to the :ref:`LocConQubit` module which extends the capabilities of the latter module 
with functionalities to generate control pulses in a more realistic systems with dissipating effects. 
The module incorporates the Lindblad master equation into the system propagator upon which the Local 
Control Theory generates a control pulse. For more information on LocConQubit module and Local Control 
Theory see :ref:`LocConQubit`.


Applications of the Module
__________________________

Application of the OpenQubit module can be found at this `link <https://www.e-cam2020.eu/pilot-project-ibm/>`_.


Installation
____________

Before applying the patch LocConQubit code has to be installed  and tested. 
For the installation and testing of LocConQubit code see the corresponding :ref:`documentation <LocConQubit>`. 
`Git <https://git-scm.com/>`_ has to be also installed. 
The OpenQubit patch should be downloaded from the repository_ and made available to insert it into the 
directory containing the LocConQubit module. 
In the directory containing the branch with the LocConQubit module the installation of the OpenQubit is 
performed by applying the OpenQubit patch. 
It is advised to make a new branch from the master branch first. 
The installation should be made by following these instructions: 


::

        (Check that you are on the QC master branch,
         e.g. command 'git status' should display master in output)

        git checkout -b OpenLCTCode

        (Download the OpenQubit.patch file here directly or 
         copy it from a directory containing the previous download:)
         cp [Directory containing the OpenQubit.patch file]/OpenQubit.patch .

        git apply OpenQubit.patch


Special care should be taken when patching the ``test_5.pkl`` binary file. 
If the above operation fails due to problems with patching a binary file, the file can be separately 
downloaded from the patch source code webpage_ and inserted into the ``reference_data`` subdirectory. 

.. _git: https://git-scm.com/
.. _repository: webpage_
.. _webpage: https://gitlab.e-cam2020.eu:10443/Quantum-Dynamics/QC/tree/OpenQubit


Testing
_______

The application of the OpenQubit patch should be verified by executing the 
LocConQubit module standard test, which is performed by executing the command below in the
same directory containing all of the OpenQubit module files


::

        python test_LCT.py


where `python` is an alias for a Python 3.5 version interpreter or higher. 
The test executes five LocConQubit standard test and an additional OpenQubit test (``test_5.pkl``). 
Unit tests are sequentially executed and all must pass successfully in order to use the OpenQubit module. 



Source Code
___________

The OpenQubit patch is located at: https://gitlab.e-cam2020.eu:10443/Quantum-Dynamics/QC/tree/OpenQubit.
This same link contains the ``test_5.pkl`` binary file for download. 



Source Code Documentation
_________________________

The source code is accompanied with `sphinx <http://www.sphinx-doc.org/en/stable/>`_ documentation located in sub-directory ``./doc``. 
Instruction for sphinx installation can be found `here <http://www.sphinx-doc.org/en/stable/tutorial.html#install-sphinx>`_.
The html documentation files can be obtained by executing the following command in the ``./doc`` sub-directory

::

        cd ./doc

        make html

The generated documentation is located in the ``./doc/_build/html/index.html``.

