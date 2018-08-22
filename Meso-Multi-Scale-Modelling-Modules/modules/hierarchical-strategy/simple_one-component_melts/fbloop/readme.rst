..  sidebar:: Software Technical Information

  The information in this section describes ESPResSo++ as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Name
    Feedback control mechanism for one component melts

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

.. _one-component_polymer-melts_Fbloop:

####################################################################################################
Feedback control mechanism: A Component of the Hierarchical Equilibration Strategy for Polymer Melts
####################################################################################################

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

In the last step, microscopic monomers are reinserted into CG blobs.
This reinsertion procedure is divided into 2 parts. Firstly, monomers
are treated as mass points without non-bonded interaction. Starting
from this state, repulsive non-bonded interactions are gradually
introduced according to the feedback control mechanism explained in
Ref. [2]_. This procedure makes sure that the final fine-grained
conformation is consistent with the conformation at the more
coarse-grained level.

The present module provides the python script which performs the
feedback control mechanism.
The implementation detail is in following bellow.

1. The microscopic configuration of :math:`N` polymers consisted of :math:`M` monomers is prepared. The system size :math:`L` is determined by the number of density :math:`\rho= (N \times M) /L^3 \approx 0.85`. :math:`m` and :math:`\sigma` stands for the mass and the diameter of monomers.
	
   We presupposed that a configuration is already equilibrated at a coarse-grained level and is not equilibrated at a microscopic level.

2. NVT MD simulation is carried out with bonding potential :math:`V_{\rm{FENE}}` and force-capped-LJ potential :math:`V_{\rm{fc-LJ}}` defined as
   
   :math:`V_{\rm{fc-LJ}}=(r-r_{fc})V_{LJ}(r_{fc})+V_{LJ}(r_{fc})` for :math:`r<r_{fc}`,
	 
   :math:`V_{\rm{fc-LJ}}=V_{LJ}(r)` for :math:`r>r_{fc}`,

   for preventing too strong repulsive forces. At first we set :math:`r_{fc}=2^{1/6}\sigma`.
	 
3. The excluded volume interaction can be gradually introduced with gradually decreasing :math:`r_{fc}`.
   In order to obtain the equilibrated structure of polymer melts, :math:`r_{fc}` is controlled by the difference between the mean-square internal distances of the current configuration and that of the ideal curve in the intermediate region. This difference is defined as
    
   :math:`I=\int^{50}_{20} [(<R^2(n)>/n)_{ideal} - (<R^2(n)>/n)_{current}]dn`,
	  
   where :math:`R(n)` is an internal distance of chain segment of length  :math:`n`.
   For :math:`I < 0`, :math:`r_{fc}` is increased. In contrast, for :math:`I > 0`, :math:`r_{fc}` is decreased.

4. After performing during :math:`650 \tau`, MD simulation is finished. Where :math:`\tau=\sqrt{m \sigma^2/k_{\rm{B}}T}`.

More detail of this feedback control mechanism is explained in Ref [2]_.

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
.. [2] http://onlinelibrary.wiley.com/doi/10.1002/mats.201500013/full
