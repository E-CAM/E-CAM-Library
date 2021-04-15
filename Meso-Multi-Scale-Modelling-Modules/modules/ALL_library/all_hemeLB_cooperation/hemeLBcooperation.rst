..  In ReStructured Text (ReST) indentation and spacing are very important (it is how ReST knows what to do with your
    document). For ReST to understand what you intend and to render it correctly please to keep the structure of this
    template. Make sure that any time you use ReST syntax (such as for ".. sidebar::" below), it needs to be preceded
    and followed by white space (if you see warnings when this file is built they this is a common origin for problems).

..  We allow the template to be standalone, so that the library maintainers add it in the right place

..  Firstly, let's add technical info as a sidebar and allow text below to wrap around it. This list is a work in
    progress, please help us improve it. We use *definition lists* of ReST_ to make this readable.

..  sidebar:: Software Technical Information

  Name
    A Load Balancing Library (ALL)/GMPM-PoC

  Language
    Fortran/C/C++

  Licence
    `BSD 3-Clause <https://choosealicense.com/licenses/bsd-3-clause/>`_   

  Documentation Tool
    In source documentation using Doxygen, additional man pages and plain
    text

  Application Documentation
    `HemeLB website <http://hemelb.org.s3-website.eu-west-2.amazonaws.com/>`_

  Relevant Training Material
    `General ALL web-based seminar <https://www.youtube.com/playlist?list=PLmhmpa4C4MzY02eaacXImTts2aGJHrdwQ>`_

  Software Module Developed by
    Rene Halver


..  In the next line you have the name of how this module will be referenced in the main documentation (which you  can
    reference, in this case, as ":ref:`example`"). You *MUST* change the reference below from "example" to something
    unique otherwise you will cause cross-referencing errors. The reference must come right before the heading for the
    reference to work (so don't insert a comment between).

.. _all_hemeLB_cooperation:

#######################
Cooperation with HemeLB
#######################

..  Let's add a local table of contents to help people navigate the page

..  contents:: :local:

..  Add an abstract for a *general* audience here. Write a few lines that explains the "helicopter view" of why you are
    creating this module. For example, you might say that "This module is a stepping stone to incorporating XXXX effects
    into YYYY process, which in turn should allow ZZZZ to be simulated. If successful, this could make it possible to
    produce compound AAAA while avoiding expensive process BBBB and CCCC."


"HemeLB is a high performance lattice-Boltzmann solver optimized for simulating blood flow through sparse geometries, 
such as those found in the human vasculature." [1]_
The code is used within the CompBioMed HPC Centre of Excellence H2020 project [2]_, and is already highly optimized
for HPC usage. As a 
consequence of the initial workshop about the ALL library
hosted by JSC in the context of E-CAM, a cooperation was set up in order analyse and test whether the use of ALL could 
improve the existing scalability of the code.


Purpose of Module
_________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

This module describes the cooperation between the ALL library and the HemeLB code, from the CompBioMed HPC Centre of
Excellence.  It provides details about the work performed an the results of the cooperation.

.. TODO:

.. * If there are published results obtained using this code, describe them briefly in terms readable for non-expert users.
  If you have few pictures/graphs illustrating the power or utility of the module, please include them with
  corresponding explanatory captions.

.. If you want to add a citation, such as [CIT2009]_, please check the source code to see how this is done. Note that
.. citations may get rearranged, e.g., to the bottom of the "page".

.. .. [CIT2009] This is a citation (as often used in journals).

Background Information / Bibliography
_____________________________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

More information about HemeLB can be found at [1]_, while more information about CompBioMed can be found at [2]_.

.. [1] http://hemelb.org/#about, 03.02.2021
.. [2] https://www.compbiomed.eu/


Cooperation content
___________________

As ALL was designed to work with particle codes, it was interesting to apply the library to an lattice-Boltzmann solver, 
which usually is not particle-based. Therefore the
different grid points of the solution grid were designated as particles and since each of the grid-points already was 
assigned a workload, the sum of grid-point workloads
could be used as domain work load. Since the creation and change of domain boundaries during the simulation was deemed
not feasible,
`an example code within ALL <https://gitlab.version.fz-juelich.de/SLMS/loadbalancing/-/blob/master/include/ALL_test.cpp>`_
was used
to create initial, balanced domain decompositions for various systems and the results compared to the already existing 
load-balancing solution within HemeLB. Since test runs
were performed on a large scale, e.g. SuperMUC, it was necessary to also provide MPI-I/O based output for better 
parallel I/O efficiency.


Cooperation results
___________________

.. Keep the helper text below around in your module by just adding "..  " in front of it, which turns it into a comment

As a result it can be stated that the domain compositions provided by ALL show a better theoretical load distribution. 
Tests to check if this translates into better
code performance are inconclusive as yet, due to hardware related issues on the testing platforms. These are currently 
under furtherinvestigation, and more definitive results about
the performance of the ALL-provided domain decompositions can be expected in the near future.

The results were part of a publication about HemeLB, which was published in 2020 [Coveney]_.

.. [Coveney] Coveney, P. V. et al., Towards blood flow in the virtual human: efficient self-coupling of HemeLB,
   Interface focus 11(1), 20190119


.. vim: et sw=2 ts=2 tw=74 spell spelllang=en_us:
