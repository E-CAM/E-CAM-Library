..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
.. Trotter Based Quantum Classocal Surface Hopping Propagator  Single Path

  Authors  

  Language
    C++ (GNU 2011 or higher)

  Licence
    Specify the licence under which the software is released. Provide a link to the full online description of the
    licence. You'll find descriptions of the most common licences at https://opensource.org/licenses .
    An example here would be: `GPL <https://opensource.org/licenses/gpl-license>`_ or (the more permissive)
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    Doxygen
    `Ford < FORD>`_, for Python ReST_, etc.
     `Donkey < https://gitlab.e-cam2020.eu/Quantum-Dynamics/Surface-Hoping-Multi-Path/blob/master/doc/html/index.html>
  Application Documentation
    Provide a link to any documentation for the application.

  Relevant Training Material
    Add a link to any relevant training material. If there currently is none then say 'Not currently available.'

  Software Module Developed by
    Add the name of the person who developed the software for this module here


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _example:

#######################################################################
Trotter Based Quantum Classical Surface Hopping Propagator  Single Path
#######################################################################

Abstract
________
The present module is a highly refactored version of a code based on a highly cited algorithm published by D. Mackernan, G.Ciccotti and R. Kapral "Trotter-Based Simulation of Quantum-Classical Dynamics", J. Chem. Phys  J. Phys. Chem. B 2008, 112, 424-432. The module software has been entirely refactored in modern C++ (version 2014 or later) so as to: (a) run with high efficiency on massively parallel platforms under openmp or mpi; and (b) be at the core of additional software modules  aimed at addressing important issues such as improving the speed of convergence of estimates using correlated sampling, and much more realistic treatment of the classical bath, and connecting to other problems such as constant pH simulation through an effective Hamiltonian.

Purpose of Module
_________________
Quantum rate processes in condensed phase systems are
often computed by combining quantum and classical descriptions of
the dynamics including non-adiabatic coupling, using propagators which
amount to quantum path integrals in a partial Wigner phase space representation, such as
the mixed quantum-classical Dyson equation and variants thereof, or the Trotter decomposition of the quantum-classical propagator.  


Background Information
_____________________
An understanding of the dynamical properties of condensed phase
quantum systems underlie the description of a variety of quantum
phenomena in chemical and biological systems. These phenomena
include, among others, nonadiabatic chemical rate processes
involving electronic, vibrational or other degrees of freedom,
decoherence in open quantum systems and quantum transport
processes. Quantum effects underlie the study of ultra-fast rate
processes in solution. The development of schemes for the efficient and
accurate simulation of the quantum dynamics of such systems is an
an active area of research in chemical
physics, and is essential if problems of chemical interest involving
complex molecular species in the condensed phase are considered.

In investigations of the dynamical properties of quantum
statistical mechanical systems, one is often interested in the
average value of some operator when the system evolves from a
given initially prepared distribution described by the density
matrix :math:`\hat{\rho}(0)`. In such cases the quantum mechanical
average value of an operator :math:`\hat{B}` is given by
:math:`\overline{B(t)}= Tr \hat{B} \hat{\rho}(t)=  Tr\hat{B}(t) \hat{\rho}(0)`. Here,
:math:`\hat{B}(t)` evolves in time through the Heisenberg equation of motion.
In many applications, it is useful to partition the system into a subsystem and
a bath. A phase space description of the bath can be obtained by
taking a partial Wigner transform over the bath coordinate :math:`\{Q\}` representation
of the full quantum system. In this partial Wigner representation the expectation value of math:`\hat{B}(t)` takes the

.. math::
   \overline{B(t)}=  Tr' \int dR dP\;  {B}_W(R,P,t) {\rho}_W(R,P)

where the prime on the trace indicates a trace over the subsystem
degrees of freedom. 

The software module developed here is based on a  Trotter-based scheme for simulating
quantum-classical Liouville dynamics in terms of an ensemble of surface-hopping trajectories. The method can be used to compute the dynamics for longer times with fewer trajectories than the
sequential short-time propagation (SSTP) algorithm, which is also based on surface-hopping trajectories. The full derivation of the algorithm is given in the J.Chem Paper cited above. Here the software focus is to refactor the original code which until now was a purely serial so that it can be used efficiently on massively parallel machines. For mathematical details, we refer the reader to eq.30-35 of the paper.

Algorithms and Software Implementation
______________________________________
Sean, can you summarise the changes to the code compared with the original version


Checking for accuracy
__________________________________________
The output of this software module, was compared with the original serial code. These comparisons were made both in an openmp and mpi version of the module. All results are the statistically the same 
(i.e. within error bars). 


Testing, Performance and Scaling
_______________________
The code scales  perfectly with increasing number of computing core in the context if openmp uintil the limit of node is reached, and  in the context of mpi no change in perfeect scalimg was observed to
the limit tested.

Provide the build information for the module here an explain how tests are run.

Source Code
___________

The source codes are at 

Here link the source code that was created for the module. In this example I'm using a patch file for that reason I need
to explain what code (including exact version information), the source code is for.

You can create the patch file by (for example if you are using git for your version control) making your changes for the
module in a feature branch and then doing something like the following:

::

  [adam@mbp2600 example (master)]$ git checkout -b tmpsquash
  Switched to a new branch "tmpsquash"

  [adam@mbp2600 example (tmpsquash)]$ git merge --squash newlines
  Updating 4d2de39..b6768b2
  Fast forward
  Squash commit -- not updating HEAD
   test.txt |    2 ++
   1 files changed, 2 insertions(+), 0 deletions(-)

  [adam@mbp2600 example (tmpsquash)]$ git commit -a -m "My squashed commits"
  [tmpsquash]: created 75b0a89: "My squashed commits"
   1 files changed, 2 insertions(+), 0 deletions(-)

  [adam@mbp2600 example (tmpsquash)]$ git format-patch master
  0001-My-squashed-commits.patch

To include a patch file do something like the following:

.. literalinclude:: ./simple.patch
   :language: c
   :emphasize-lines: 2,9-11
   :linenos:

If the patch is very long you will probably want to add it as a subpage so let's do that now

.. toctree::
   :glob:
   :maxdepth: 1

   patch

.. Here are the URL references used
.. _ReST: http://docutils.sourceforge.net/docs/user/rst/quickref.html

