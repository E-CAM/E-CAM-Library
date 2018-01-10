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

OpenQubit is a patch to the LocConQubit module which extends the capabilities of the latter module 
with functionalities to generate contorl pulses in a more realistic systems whit dissipating effects. 
The module incorporates the Lindblad master equation into the system propagator upon which the Local 
Control Theory generates a control pulse. For more information on LocConQubit and Local Control Theory 
see here_ .



Applications of the Module
__________________________

Application of the LCT module can be found here_ .

.. _here: https://www.e-cam2020.eu/pilot-project-ibm/



Installation
____________

Before applying the patch LocConQubit code has to be installed. 
For the installation of LocConQubit see the corresponding LocConQubit documentation_ .
Installation of OpenQubit is performed by applying the OpenQubit patch to the LocConQubit code following these instructions:





.. _documentation: here_



.. Testing
.. ._______
.. 
.. The successfulness of OpenQubit patch application should be verified by executing the 
.. codes standard test, which is performed by executing the below command in the
.. directory containing all OpenQubit module files
.. 
.. ::
.. 
..               python test_OpenQubit.py
.. 
.. where `python` is an alias for a Python 3.5 version interpreter or higher. Unit tests are sequentially executed and all must pass
.. successfully in order to use OpenQubit module.
.. 



Source Code
___________

The OpenQubit patch is located at: https://gitlab.e-cam2020.eu:10443/Quantum-Dynamics/QC


Source Code Documentation
_________________________

.. The source code documentation is given at https://gitlab.e-cam2020.eu:10443/Quantum-Dynamics/QC/doc
.. The documentation files (html and latex format) are obtained by executing the following command in the doc directory
.. 
.. ::
.. 
..         cd ./doc
.. 
..         doxygen LocConQubit_doxygen_settings
.. 
.. The source code documentation is 
.. 


