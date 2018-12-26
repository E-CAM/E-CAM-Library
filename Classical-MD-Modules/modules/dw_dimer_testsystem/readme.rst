..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).


..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    OpenMMTools

  Language
    Python (3.6, 3.7)

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    Sphinx

  Application Documentation
    http://openmmtools.readthedocs.org

  Relevant Training Material
    Add a link to any relevant training material. If there currently is none then say 'Not currently available.'

  Software Module Developed by
    David W.H. Swenson


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _dw_dimer_testsystem:

#############################
Double-Well Dimer Testsystems
#############################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."

One of the common systems used to study rare events is the double-well dimer
in a bath of repulsive particles. In this system, two particles are linked
by a "bond" that allows condensed and extended metastable states. This
module adds this system, and tools for created several variants of it, to
the OpenMMTools package. 

Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

The symmetric double-well dimer is a widely-used model for developing new
rare events methodologies. However, implementing simple models in software
packages that are designed for biological systems, such as OpenMM, can be
difficult for a novice user. As a result, many developers of new methods
will implement their methods twice: first to interface with simple models
such as the double-well dimer using in-house MD codes, then a second time to
interface with more powerful tools, such as OpenMM, to simulate complex
systems such as biomolecules.  This module provides tools that facilitate
setting up custom versions of the double-well dimer for OpenMM, allowing
users to develop their new methodologies directly for the same platform that
they will use for larger practical applications.

The widely-used double-well dimer model is a symmetric quartic potential,
given by:

.. math::
   V_{dw}(r) = h \left(1 - \left(\frac{r - r_0 - w}{w}\right)^2\right)^2

where :math:`r` is the distance between the particles, :math:`h` is the
height of the barrier, :math:`r_0` is the energy minimum for the condensed
metastable state, and :math:`w` sets the distance for the extended
metastable state according to :math:`r_{ex} = r_0 + 2w`.

This "bonded" interaction is added for specific pairs of particles, on top
of a background of WCA (purely repulsive) "nonbonded" interactions between
all particles. The WCA interaction is:

.. math::
  V_\text{WCA}(r) =
  \begin{cases}
    4 \epsilon \left( \left( \frac{\sigma}{r} \right)^{12} - \left( \frac{\sigma}{r}
    \right)^6 \right) + \epsilon & \text{if $r\le 2^{1/6} \sigma$} \\
    0 & \text{if $r> 2^{1/6} \sigma$}
  \end{cases}
where :math:`\sigma` is a characteristic distance and :math:`\epsilon` is a
characteristic energy scale.

The quartic double well is a simple model of rare events, where the expected
reaction coordinate is obvious. However, it is can be very useful for
benchmarking new methods. The OpenMMTools package includes a suite of
systems to be used in testing and benchmarking, and was a natural place to
add these.

Although the most widely-used approach has been to have a single dimer in
the bath of WCA particles, this module provides two possible extensions that
have been previously used in the literature.
The first extension is to allow multiple independent dimers, as was done in
[Swenson2014]_. This is done by changing the ``ndimers``
parameter in the ``DoubleWellDimer_WCAFluid`` test system.
The other extension is to create a polymer chain of double-well bonds, as
was done in [Rogal2008]_. This is done by changing the
``nchained`` parameter in the ``DoubleWellChain_WCAFLuid`` test system.

.. [Swenson2014] D.W.H. Swenson and P.G. Bolhuis, J. Chem. Phys. **141**,
    044101 (2014); https://doi.org/10.1063/1.4890037
.. [Rogal2008] J. Rogal and P.G. Bolhuis. J. Chem. Phys. **129**, 224107
    (2008); https://doi.org/10.1063/1.3029696

Background Information
______________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This builds on the ``testsystems`` module of OpenMMTools. The OpenMMTools
source is hosted at http://github.com/choderalab/openmmtools. These
contributions will be included in OpenMMTools 0.17.0.


Building and Testing
____________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This has been incorporated into OpenMMTools. Up-to-date installation
information can be found in the OpenMMTools documentation; as of this
writing it simply requires installing conda, and then using the command
``conda install -c omnia openmmtools``. Note that this requires a Python
3-based environment; OpenMMTools does not support Python 2.

Tests require nose, and the full suite of tests can be run from the
``openmmtools`` directory with the command ``nosetests``.

Source Code
___________

.. Notice the syntax of a URL reference below `Text <URL>`_ the backticks matter!

The source for this module was contributed to OpenMMTools. The relevant pull
request is:

* http://github.com/choderalab/openmmtools/pull/389

