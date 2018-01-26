.. _OpenQubit:

####################
OpenQubit
####################

.. sidebar:: Software Technical Information

  Language
    Python 3.5

  License
    TBD

  Documentation Tool
    doxygen

.. contents:: :local:

.. This is an example of what a *module* for E-CAM looks like. Please add to this template any additional items that are
.. straightforward to fill out in the general case. You are free add any level of complexity you wish (within the bounds of
.. what ReST_ can do).

.. To add your module, fork this GitLab repository to your account on GitLab. Clone your repository, make a feature branch
.. and add a directory that will contain your module information. Copy this :download:`readme.rst` file there. Push your
.. changes back to GitLab and immediately open a merge request from your feature branch against our repository. We can
.. discuss your module in the merge request and help you get it accepted.

.. Add technical info as a sidebar and allow text below to wrap around it

Purpose of Module
_________________

OpenQubit is a patch to the *LocConQubit* module which extends the capabilities of the latter module 
with functionalities to generate control pulses in a more realistic systems with dissipating effects. 
The module incorporates the Lindblad master equation into the system propagator upon which the Local 
Control Theory generates a control pulse. For more information on LocConQubit and Local Control Theory 
see here_ .



Applications of the Module
__________________________

Application of the OpenQubit module can be found here_ .

.. _here: https://www.e-cam2020.eu/pilot-project-ibm/



Installation
____________

Before applying the patch LocConQubit code has to be installed. 
git_ has to be also installed. 
For the installation of LocConQubit see the corresponding LocConQubit documentation_ .
In the LocConQubit directory the installation of the OpenQubit is performed by applying the OpenQubit 
patch to the LocConQubit code following these instructions:


::

        git apply OpenQubit.patch


Special care should be taken when patching the *test_5.pkl* binary file. 
If the operation fails due to problems with patching a binary file, the file can be separately downloaded from the 
patch source code webpage_ and inserted into the *reference_data* subdirectory. 

.. _documentation: ../LocConQubit/readme.html
.. _git: https://git-scm.com/
.. _webpage: https://gitlab.e-cam2020.eu:10443/Quantum-Dynamics/QC/tree/OpenQubit



Testing
_______

The application of the OpenQubit patch should be verified by executing the 
LocConQubit module standard test, which is performed by executing the below command in the
same directory containing all of the OpenQubit module files


::

        python test_LCT.py


where `python` is an alias for a Python 3.5 version interpreter or higher. 
The test executes five LocConQubit standard test and an additional OpenQubit test (*test_5.pkl*). 
Unit tests are sequentially executed and all must pass successfully in order to use the OpenQubit module. 



Source Code
___________

The OpenQubit patch is located at: https://gitlab.e-cam2020.eu:10443/Quantum-Dynamics/QC/tree/OpenQubit
This same link contains the *test_5.pkl* binary file for download. 



Source Code Documentation
_________________________

The source code documentation is given at https://gitlab.e-cam2020.eu:10443/Quantum-Dynamics/QC/doc
The documentation files (html and latex format) are obtained by executing the following command in the doc directory

::

        cd ./doc

        doxygen LocConQubit_doxygen_settings


