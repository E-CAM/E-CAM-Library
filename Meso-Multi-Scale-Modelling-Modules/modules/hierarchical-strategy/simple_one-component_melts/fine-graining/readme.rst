..  sidebar:: Software Technical Information

  The information in this section describes ESPResSo++ as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Name
    Fine-graining procedure for one component melts

  Language
    Python (2.7) and C++

  Licence
    `GPL <https://opensource.org/licenses/gpl-license>`

  Documentation Tool
    Sphinx and Doxygen

  Application Documentation
    http://espressopp.github.io/

  Relevant Training Material
    https://github.com/espressopp/espressopp/tree/master/examples

.. _one-component_polymer-melts_FG:

#######################################################################################
Fine-graining: A Component of the Hierarchical Equilibration Strategy for Polymer Melts
#######################################################################################

..  contents:: :local:

The module is an implementation of the existing hierarchical strategy
[1]_ for the equilibration of simple one-component polymer melts in
ESPResSO++.

.. Add technical info as a sidebar and allow text below to wrap around it

Purpose of Module
_________________

To study the properties of polymer melts by numerical simulations,
equilibrated configurations must be prepared. However, the relaxation
time for high molecular weight polymer melts is huge and increases,
according to reptation theory, with the third power of the molecular
weight. Hence, an effective method for decreasing the equilibration
time is required. The hierarchical strategy pioneered in Ref. [1]_ is
a particularly suitable way to do this. The present module provides
a part of that method described below.

To decrease the relaxation time, microscopic monomers are coarse-grained (CG)
by mapping each subchain with :math:`N_{b}` monomers onto a soft blob.
The CG system is then characterized by a much lower molecular weight and
thus is equilibrated quickly. One thus obtains a configuration that is
equilibrated on large scales but does not provide information about
the structure on smaller (i.e. more fine-grained (FG)) scales.

To obtain the latter, the resolution is step-by-step increased by recursively
applying a fine-graining procedure to the previous (more coarse-grained) level.
In such a fine-graining step, each CG polymer chain is replaced with
a more fine-grained chain, by dividing a CG blob into several FG blobs.

The present module provides the python script which performs this fine-graining procedure.
The implementation detail is in following below.

1. The microscopic configuration of :math:`N` polymers consisted of :math:`M` monomers is prepared. The system size :math:`L` is determined by the number of density :math:`\rho= (N \times M) /L^3 \approx 0.85`. :math:`m` and :math:`\sigma` stands for the mass and the diameter of monomers.

   We presuppose that equilibrated CG chain at :math:`N_{b}` is already obtained.

2. The softblobs at :math:`N_{b}` is divided into 2 softblobs at :math:`N_{b}/2` under the constraint conditions defined as


   :math:`\mathbf{R}_i^{N_{b}} = \frac{\mathbf{R}_{2i-1}^{N_{b}/2} + \mathbf{R}_{2i}^{N_{b}/2}}{2} \equiv \mathbf{r}_{\rm{com}}^i`,

   :math:`R_{g, N_{b}/2}^{(2i-1)} = R_{g, N_{b}/2}^{2i} = \frac{R_{g, N_{b}}^{i}}{\sqrt{2}}`,

   where :math:`\mathbf{R}_{i}^{N_{b}}` stands for
   :math:`\mathbf{R}_{i}` at :math:`N_{b}` and :math:`R_{g, N_{b}}^{i}`
   stand for :math:`R_{g}^{i}` at :math:`N_{b}`.
   Namely, the center of mass of 2 softblobs at :math:`N_{b}/2` is identical
   with the position of a softblob at :math:`N_{b}`.
	       
3. For equilibrating a local configuration at :math:`N_{b}/2`, NVT
   MD simulation is performed.

   In the beginning, a MD simulation takes
   into account bonding potential :math:`V_{\rm{bond}}`,

   the potential for fluctuating radius of gyration :math:`V_{sphere}`
   and the constrain potentials for center of mass described as

   :math:`V_{\rm{com}}(\mathbf{r}_{\rm{com}}^i) = k_{\rm{com}}(\mathbf{r}_{\rm{com}}^i - \mathbf{R}_i^{N_{b}})^2`.

   Each :math:`16\tau_{\rm{blob}}`, MD simulation is including
   the bending interactions :math:`V_{\rm{angle}}`

   and non bonding interactions :math:`V_{\rm{nb}}` in this order.

   Where :math:`\tau_{\rm{blob}}=\sqrt{m N_{b}\sigma^2/k_{\rm{B}}T}`.

4. After including all interactions, MD simulation is performed during :math:`16\tau_{\rm{blob}}`.

5. In order to obtain the snapshot which has the ideal mean square internal distance (MSID) :math:`<R(n)^2>`,
   MD simulation is continued to carry out. Where MSID :math:`<R(n)^2>` is defined as

   :math:`<R(n)^2> \equiv \frac{1}{M/N_{b} -n}\sum^{N-1}_{j=0}\sum^{M/N_{b} -n}_{i=1}(\mathbf{R}_{i+(M/N_{b})j} - \mathbf{R}_{i+(M/N_{b})j+n})^2`.

   This is calculated in each :math:`\tau_{\rm{blob}}`.

   After obtaining good snapshot at :math:`N_{b}/2`, fine-graining procedure is finished.

Background Information
______________________

The implementation of this module is based on ESPResSO++. You can
learn about ESPResSO++ from the following links:

* ESPResSO++ documentation: http://espressopp.github.io/ESPResSo++.pdf
* ESPResSO++ source code: https://github.com/espressopp/espressopp


Building and Testing
____________________

Explanation of installation:

* https://github.com/espressopp/espressopp

After installing this module, it can be tested according to the README file
found under the following link:

* https://github.com/espressopp/espressopp/tree/master/examples/hierarchical_strategy_for_one-component/


Source Code
___________

This module has been merged into ESPResSo++:

* https://github.com/espressopp/espressopp/pull/213

References
___________
.. Here are the URL references used
.. [1] http://pubs.acs.org/doi/abs/10.1021/mz5000015
