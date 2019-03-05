.. _simple_one-component_melts:

############################################################
Hierarchical Strategy for Simple One-Component Polymer Melts
############################################################

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

  Author
    Hideki Kobayashi

.. contents:: :local:

The module is an implementation of the existing hierarchical strategy
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
a particularly suitable way to do this. The present module provides an
integration of that method into the package ESPResSO++.

To decrease the relaxation time, microscopic monomers are
coarse-grained by mapping each subchain with :math:`N_{b}` monomers
onto a soft blob. A polymer chain, originally consisting of :math:`N`
monomers, is replaced by a coarse-grained (CG) chain consisting of
:math:`N/N_{b}` soft blobs linked by a harmonic bond potential,
:math:`V_{bond}=3 k_{B}T d^{2}/2b_{CG}^2`, and an angular bond-bending
potential :math:`V_{bend}=k_{B}T k_{bend}(1 + \cos(\theta))/2`. Here
:math:`d` is the distance and :math:`\theta` is the angle between
consecutive bonds. The interactions between non-bonded soft blobs are
taken into account by a repulsive pair potential :math:`V_{nb}=k_{B}T
\epsilon U_{G}(r_{ij})`. Here :math:`r_{ij}` is the center-to-center
distance between the two blobs, :math:`U_{G}(r_{ij})` is a Gaussian
function with variance :math:`\overline{\sigma}^2 = \sigma_{i}^2 + \sigma_{j}^2` and
:math:`\sigma_{i}` is the gyration radius of blob number
:math:`i`. The gyration radius :math:`\sigma` is in turn
fluctuating. This fluctuation is controlled by the potential
:math:`V_{sphere}=k_{B}T \, (a_{1}N_{b}^3\sigma^{-6} +
a_{2}N_{b}^{-1}\sigma^{2} + a_{3}\sigma^{-3})`.

After equilibrating a configuration at very coarse resolution, each CG
polymer chain is replaced with a more fine-grained (FG) chain. In this
procedure, a CG blob is divided into several FG blobs. The center of
mass (COM) of the FG blobs coincides with the position of the CG
blob's center, and is being kept fixed during the relaxation of the
local conformation of the FG monomers within the CG blob.

To develop this module, the following classes have been implemented or
modified (that may have been described in more detail elsewhere):

* A ``VSpherePair`` class for calculating
  :math:`V_{nb}=k_{B}T \epsilon U_{G}(r_{ij})`
      
* A ``LangevinThermostatOnRadius`` class for simulating
  the fluctuations of the radii of the blobs

* A ``VSphereSelf`` class for calculating :math:`V_{sphere}=k_{B}T \,
  (a_{1}N_{b}^3\sigma^{-6} + a_{2}N_{b}^{-1}\sigma^{2} +
  a_{3}\sigma^{-3})`

* A ``FixedLocalTupleList`` class for storing the N-tuple
  of particles consisting of both real and virtual particles

* A ``ConstrainCOM`` class for conserving the COM of N FG
  blobs with the CG blob

Background Information
______________________

The implementation of this module is based on ESPResSO++. You can
learn about ESPResSO++ from the following links:

* ESPResSO++ documentation: http://espressopp.github.io/ESPResSo++.pdf
* ESPResSO++ source code: https://github.com/espressopp/espressopp

Testing
_______

Explanation of installation of ESPResSO++ can be found at:

* https://github.com/espressopp/espressopp

After installing this module, an example can be run from `hierarchical_strategy_for_one-component` subdirectory of the `examples` folder using the `run_example` script to be found there.

* https://github.com/hidekb/espressopp/tree/hierarchical-strategy/examples/hierarchical-strategy/simple_one-component

Source Code
___________

This module was merged into ESPResSo++ in the Pull Request:

* https://github.com/espressopp/espressopp/pull/213 


References
___________
.. Here are the URL references used
.. [1] : http://pubs.acs.org/doi/abs/10.1021/mz5000015
