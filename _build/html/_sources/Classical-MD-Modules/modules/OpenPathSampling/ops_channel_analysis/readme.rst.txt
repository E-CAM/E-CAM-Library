.. _channel_analysis:

####################
OPS Channel Analysis
####################

.. sidebar:: Software Technical Information

  The information in this section describes OpenPathSampling as a whole.
  Information specific to the additions in this module are in subsequent
  sections.

  Language
    Python (2.7)

  Documentation Tool
    Sphinx, numpydoc format (ReST)

  Application Documentation
    http://openpathsampling.org

  Relevant Training Material
    http://openpathsampling.org/latest/examples/

  Licence
    LGPL, v. 2.1 or later

.. contents:: :local:

In many cases, more than one channel is available to a system -- either
because there are multiple channels between two states, or because there are
multiple states, and transitions between each pair is a different channel.
This module provides tools to identify which paths are in each channel, and
to study the statistical behavior of the switching between channels.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

In practical examples, more than one channel (i.e., mechanism) can occur
during a path sampling simulation. This is inherently the case if you have
multiple stable states, since the transition between each pair of states
will be of separate interest. It can also be the case when you have a single
pair of states, but multiple channels that connect the states.

This module uses the OPS ``Ensemble.split`` function to study how a simulation
samples these channels. The user must provide a list of possible channels,
described as OPS ``Ensemble`` objects. From this, each path is analyzed, and
various statistical behavior about the sampling process can be determined.

The main object added in this module is the ``ChannelAnalysis`` object,
which performs this analysis and stores the results. Once the analysis has
been performed, several properties can be extracted, including:

* ``switching_matrix``: how many times a switch from one channel to another
  occurred
* ``residence_times``: the number of MC steps spent with the path in each
  channel (returns the entire list so the user can calculate distribution
  properties with, e.g., ``numpy``)
* ``total_time``: total number of MC steps spent in each channel
* ``status(step_num)``: the channel the simulation was in for a given step
  number

In principle, a path might satsify the requirement for more than one channel
at a time. This analysis class allows for that, and gives the user the
option of setting its ``treat_multiples`` attribute:

* ``newest``: use the most recent channel entered
* ``oldest``: use the least recent channel entered
* ``multiple``: treat multiple channels as a new type of channel, e.g., 'a'
  and 'b' because 'a,b'
* ``all``: treat each channel individually, despite overlaps. For ``status``
  this is the same as 'multiple'


Background Information
______________________

This module builds on OpenPathSampling, a Python package for path sampling
simulations. To learn more about OpenPathSampling, you might be interested in
reading:

* OPS documentation: http://openpathsampling.org
* OPS source code: http://github.com/openpathsampling/openpathsampling


Testing
_______

Tests in OpenPathSampling use the `nose`_ package.

.. IF YOUR MODULE IS IN OPS CORE:

This module has been included in the OpenPathSampling core. Its tests can
be run by setting up a developer install of OpenPathSampling and running
the command ``nosetests`` from the root directory of the repository.

.. IF YOUR MODULE IS IN A SEPARATE REPOSITORY

.. The tests for this module can be run by downloading its source code, 
.. installing its requirements, and running the command ``nosetests`` from the
.. root directory of the repository.

Examples
________

* https://gitlab.e-cam2020.eu/dwhswenson/ops_additional_examples/blob/master/channel_analysis.ipynb

Source Code
___________

.. link the source code

.. IF YOUR MODULE IS IN OPS CORE

This module has been merged into OpenPathSampling. It is composed of the
following pull request:

* https://github.com/openpathsampling/openpathsampling/pull/658

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/

