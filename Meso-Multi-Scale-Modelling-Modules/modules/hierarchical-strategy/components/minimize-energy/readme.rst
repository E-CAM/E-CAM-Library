.. _components_MinimizeEnergy:

##########################################################################################
Minimize Energy : A Component of the Hierarchical Equilibration Strategy for Polymer Melts
##########################################################################################

.. sidebar:: Software Technical Information

  The information in this section describes ESPResSo++ as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Languages:
    Python (2.7) and C++

  Documentation Tools:
    Sphinx and Doxygen

  Application Documentation:
    http://espressopp.github.io/

  Relevant Training Material:
    https://github.com/espressopp/espressopp/tree/master/examples

  Licence
    GNU General Public License

.. contents:: :local:

The module is an implementation of a part of a hierarchical strategy
[1]_ for the equilibration of simple one-component polymer melts in
ESPResSO++.

.. Add technical info as a sidebar and allow text below to wrap around it

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

To study the properties of polymer melts by numerical simulations,
equilibrated configurations must be prepared. However, the relaxation
time for high molecular weight polymer melts is huge and increases,
according to reptation theory, with the third power of the molecular
weight. Hence, an effective method for decreasing the equilibration
time is required. The hierarchical strategy pioneered in Ref. [1]_ is
a particularly suitable way to do this. The present module provides
a part of that method.

When the microscopic monomers are re-inserted into the soft blobs, the
polymer configurations should satisfy a local energy minimum
to avoid overlap between particles.
This module provides a steepest-descent method <https://en.wikipedia.org/wiki/Gradient_descent>`_
which is a typical energy minimization method. 

This module is a modification of the already existing class `espressopp.integrator.MinimizeEnergy`. The modifications are as follows:

1. Corrected the procedure of particle redistribution to cells.

2. A variable relaxation of the energy per step :math:`\gamma` is implemented.
   In this case, the position of particles is updated following the equation:
   .. math::
   p_{i+1} = p_i + (d_{max}/f_{max}) F_i
   where :math:`d_{max}` is the maximum update of particle coordinates
   in a single steepest-descent step and :math:`\gamma`
   is adjusted via :math:`\gamma=d_{max}/f_{max}` where :math:`f_{max}` is
   the maximum force in a single step.

These modifications significantly stabilize the procedure of redistributing particles
to cells, and any value :math:`d_{max}` less than half the skin parameter of the Verlet
list can be used.

Background Information
______________________

The implementation of this module is based on ESPResSO++. You can
learn about ESPResSO++ from the following links:

* ESPResSO++ documentation: http://espressopp.github.io/ESPResSo++.pdf
* ESPResSO++ source code: https://github.com/espressopp/espressopp

Testing
_______

Explanation of installation:

* https://github.com/espressopp/espressopp

After installing this module, it can be tested by a Python script
found under the following link:

* https://github.com/espressopp/espressopp/tree/master/testsuite/minimize_energy

Source Code
___________

This module has been merged into ESPResSo++:

* https://github.com/espressopp/espressopp/pull/89
* https://github.com/espressopp/espressopp/pull/90

References
___________
.. Here are the URL references used
.. [1] : http://pubs.acs.org/doi/abs/10.1021/mz5000015
