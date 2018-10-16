.. _simple_one-constrain-rg:

##########################################################################
Hierarchical Strategy for Simple One-Component Polymer Melts: constrain-rg
##########################################################################

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

Reference [1]_ describes the principles of a hierarchical strategy to
equilibrate simple one-component polymer melts described in terms of
atomistic or coarse-grained (bead-spring) models. The present module
is part of our implementation of this method in ESPResSO++.

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
a particularly suitable way to do this. The present module is part of
a suite of programs which realize this method within the framework of
the simulation package ESPResSO++.

To decrease the relaxation time, microscopic monomers are
coarse-grained (CG) by mapping each subchain with :math:`N_{b}` monomers
onto a soft blob. The CG system is then characterized by a much
lower molecular weight and thus is equilibrated quickly. One thus
obtains a configuration that is equilibrated on large scales
but does not provide information about the structure on smaller
(i.e. more fine-grained (FG)) scales.

To obtain the latter, the resolution is step-by-step increased by
recursively applying a fine-graining procedure to the previous (more
coarse-grained) level. In such a fine-graining step, each CG polymer
chain is replaced with a more fine-grained chain, by dividing a CG
blob into several FG blobs. In the last step, microscopic monomers are
reinserted into their CG blobs.

The resulting set of FG blobs is set up in such a way that its
conformation is consistent with the conformation at the more
coarse-grained level. After this setup, the local FG conformation is
relaxed into a local equilibrium, again consistent with the (fixed) CG
blobs.

In the last step (the reinsertion of microscopic monomers) it is
useful to avoid an initial overstretching of microscopic bonds as much
as possible. To this end, the algorithm makes sure that the gyration
radii of the subchains coincide with the gyration radii of the
corresponding blobs, during an initial equilibration period.

The present module provides the C++ class for applying a suitable
constraint that conserves the gyration radius of `N` microscopic
monomers.

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

* https://github.com/espressopp/espressopp/tree/master/testsuite/constrain_rg

Source Code
___________

This module has been merged into ESPResSo++:

  * https://github.com/espressopp/espressopp/pull/182

References
___________
.. Here are the URL references used
.. [1] http://pubs.acs.org/doi/abs/10.1021/mz5000015,
   preprint available via
   https://arxiv.org/abs/1610.07511
   
