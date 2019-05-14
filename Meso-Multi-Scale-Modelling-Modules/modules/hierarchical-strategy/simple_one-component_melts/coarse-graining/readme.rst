..  sidebar:: Software Technical Information

  The information in this section describes ESPResSo++ as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Name
    Coarse-graining procedure for one component melts

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

.. _one-component_polymer-melts_CG:

#########################################################################################
Coarse-Graining: A Component of the Hierarchical Equilibration Strategy for Polymer Melts
#########################################################################################

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
thus is equilibrated quickly.
The present module provides a python script which performs this
coarse-graining procedure. The implementation detail is in following below.

1. The microscopic configuration of :math:`N` polymers consisted of :math:`M` monomers is prepared. The system size :math:`L` is determined by the number of density :math:`\rho= (N \times M) /L^3 \approx 0.85`. :math:`m` and :math:`\sigma` stands for the mass and the diameter of monomers.
	
2. The configuration of :math:`N` CG chain at :math:`N_{b}=100` is generated from the microscopic configuration.

   The position of :math:`i`-th softblobs :math:`\mathbf{R}_{i}` is determined by :math:`\mathbf{R}_{i} = \frac{1}{N_{b}}\sum^{N_{b}}_{j=1} \mathbf{r}_{(j + (i-1)N_{b})}`, where :math:`\mathbf{r}_{i}` stands for the position of :math:`i`-th monomers.

   The radius of gyration :math:`i` th softblobs :math:`R_{g}^i` is also determined by :math:`{R_{g}^i}^2 = \frac{1}{N_{b}}\sum^{N_{b}}_{j=1} (\mathbf{r}_{(j + (i-1)N_{b})} - \mathbf{R}_{i})^2`.

3. The CG configuration is equilibrated by NVT MD simulation with mass of softblobs :math:`M = N_{b}m` during the equilibration time :math:`\tau_{\rm{r}}` defined as :math:`\tau_{\rm{r}} \sim\ \left( \frac{M}{N_{b}}\right)^2 \tau_{\rm{blob}}`, where :math:`\tau_{\rm{blob}}=\sqrt{m N_{b}\sigma^2/k_{\rm{B}}T}`.

   Hence, CPU time :math:`\tau_{100}` for softblobs with :math:`N_{b}=100` is estimated as :math:`\tau_{100} \sim\ N \times \left( \frac{M}{N_{b}}\right)^3 \tau_{\rm{blob}}`.

4. After equilibrating a configuration, we continue to carry out MD simulation for adopting the snapshot which show ideal mean square internal distance (MSID) :math:`<R(n)^2>` represented as :math:`\frac{1}{M/N_{b} -n}\sum^{N-1}_{j=0}\sum^{M/N_{b} -n}_{i=1}(\mathbf{R}_{i+(M/N_{b})j} - \mathbf{R}_{i+(M/N_{b})j+n})^2`.

   Ideal MSID means the MSID of CG chains generated from fully equilibrated microscopic configurations.

   A snapshot is captured in each :math:`\tau_{\rm{blob}}`.



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
