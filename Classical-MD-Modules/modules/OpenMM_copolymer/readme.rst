:orphan:

..  sidebar:: Software Technical Information

  Name
    openmm_copolymer

  Language
    Python 3.7, OpenMM API

  Licence
    `MIT <https://opensource.org/licenses/mit-license>`_

  Documentation Tool
    sphynx

  Application Documentation
    `pydoc3.7 <https://gitlab.com/pcarrivain/openmm_copolymer/-/blob/master/openmm_copolymer_functions.py>`_

  Relevant Training Material
    `<https://gitlab.com/pcarrivain/openmm_copolymer>`_

  Software Module Developed by
    Pascal Carrivain

.. _openmm_copolymer:

#############################
E-CAM openmm_copolymer module
#############################

The *openmm_copolymer* is a module that sample conformation of
a *block-copolymer* given an *epigenome* state file.
This module takes advantage of the
`OpenMM software <http://openmm.org>`_
and GPU acceleration.
It builds a `Kremer-Grest <https://aip.scitation.org/doi/10.1063/1.458541>`_
polymer model with uni-dimensional epigenetic information and construct
the epigenetic interactions based on the model you design.
You simply need to feed the module with an *epigenome* state file,
the interaction model and the mechanical properties of the polymer.
You can imagine to model small part of an *epigenome* or
the whole genome confined inside the cell nucleus.

Purpose of Module
_________________

The epigenetic and the tri-dimensional structure of fly genome
is studied by means of *block-copolymer*.
The *block-copolymer* is a polymer made of more
than one monomer species.
The epigenetic information do not involve alterations in the DNA but
`histone <https://en.wikipedia.org/wiki/Histone>`_ tails modifications.
This uni-dimensional information can be projected along
the contour of a *block-copolymer* model.
Then, every pairwise of monomers interacts according to the
epigenetic states leading to specific pattern of interactions.
The interaction patterns can be visualized using contacts map:
two-dimensional map with position along the polymer and a third
dimension with color scale for the intensity of contacts.
Since 2000, biologists can produce the same kind of data thanks
to the *high-throughput-sequencing* methods 3C, 4C, 5C and Hi-C:
`Chromosome-Conformation-Capture <https://en.wikipedia.org/wiki/Chromosome_conformation_capture>`_.
Recently, biologists shown the interactions pattern is correlated
with the epigenetic information.
However, the strength and model of interactions between
epigenetic states are not always clearly known.

In addition to the *high-throughput-sequencing* methods,
we can study the spatial distances inside part
of the genome with the help of
`FISH <https://en.wikipedia.org/wiki/Fluorescence_in_situ_hybridization>`_
and high-resolution methods.
All the spatial distances can be simply extracted from
the model built with the help of *openmm_copolymer* module.

The module we propose uses the
`OpenMM software <http://openmm.org>`_
with GPU
acceleration to sample as many as possible epigenetic parameters.
It is possible to use effective interactions
(gaussian overlap or
`Lennard-Jones potential <https://en.wikipedia.org/wiki/Lennard-Jones_potential>`_)
to model the epigenetic.
The module introduces the possibility to replace
effective epigenetic interactions with
`binders model <https://www.ncbi.nlm.nih.gov/pubmed/22988072>`_ too.
In this case, the binder is like a protein that can
bind to a specific site of the genome.
A simple input file is enough to tell the script about
the binder-binder and monomer-binder interactions.

The module includes pairing potential, nucleus confinement potential
as-well-as genome examples.

..
   The present module assists the creation of polymer described
   by FENE bond and WCA repulsive potential
   to resolve the excluded volume constraints.
   On top of that, it builds the epigenetic interactions
   based on a simple input file.

It can be used by polymer physicists, biophysicists
for epigenetic modeling, to understand the link between
epigenetic and tri-dimensional structure
of a genome, to estimate first-passage-time encounter of two locii.
It is used in a scientific collaboration to study
a specific promoter-enhancer
system in the fruit-fly organism (ENS Lyon, France).
However, the publication is not currently available.

Background Information
______________________

We use the OpenMM toolkit for molecular dynamics.
We implemented functionalities to build a
`Kremer-Grest <https://aip.scitation.org/doi/10.1063/1.458541>`_
polymer system with uni-dimensional epigenetic information.
We also implement functions to build the quantities biologists extract from
`high-throughput-sequencing <https://en.wikipedia.org/wiki/Chromosome_conformation_capture>`_
and
`FISH <https://en.wikipedia.org/wiki/Fluorescence_in_situ_hybridization>`_
experiments.
You can find pdf file with a detailed description on the
`openmm_copolymer GitLab repository <https://gitlab.com/pcarrivain/openmm_copolymer>`_.
The module will be constantly improved with new functionalities.

Building and Testing
____________________

The instructions to install, test and run the module can be find on the
`openmm_copolymer GitLab repository <https://gitlab.com/pcarrivain/openmm_copolymer>`_.

Source Code
___________

The source code can be found on the
`openmm_copolymer GitLab repository <https://gitlab.com/pcarrivain/openmm_copolymer>`_.
