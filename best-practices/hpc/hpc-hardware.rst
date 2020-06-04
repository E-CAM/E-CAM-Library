.. _hpc_hardware:

Currently Available Hardware
----------------------------

For the last decade, power and thermal management has been of high importance. The entire market focus has moved
from achieving better performance through single-thread optimizations, e.g., speculative execution, towards simpler
architectures that achieve better performance per watt, provided that vast parallelism exists. The HPC community,
particularly at the higher end, focuses on the flops/watt metric since the running cost of high-end HPC systems are
so significant. It is the potential power requirements of exa-scale systems that are the limiting factor (given
currently available technologies).

The practical outcome of this is the rise of accelerating co-processors and many-core systems. In the following sections
we will discuss three such technologies that are likely to form the major computing components of the first
generation of exa-scale machines:

.. toctree::
    :glob:
    :maxdepth: 2

    ./hardware/intel-many-core
    ./hardware/nvidia
    ./hardware/fpga

We will outline the current generation of technologies in this space and also describe the (currently) most-productive
programming model for each. We will not discuss other new CPU technologies (such as Power 9, Intel Skylake, or
ARMv8) since in comparison to these technologies they would be expected to only provide ~10% or less of the compute
power of potential exa-scale systems.

The problem with the current three-pronged advance is that it is not always easy to develop parallel programs for these
technologies and, moreover, those parallel programs are not always performance portable between each technology,
meaning that each time the architecture changes the code may have to be rewritten. While there are open standards
available for each technology, each product currently has different preferred standards which are championed by the
individual vendors (and therefore the best performing).

In general, we see a clear trend towards more complex systems, which is expected to continue over the next decade.
These developments will significantly increase software complexity, demanding more and more intelligence across
the programming environment, including compiler, run-time and tool intelligence driven by appropriate programming
models. Manual optimization of the data layout, placement, and caching will become uneconomic and time
consuming, and will, in any case, most likely soon exceed the abilities of the best human programmers.

Impact of Deep Learning
~~~~~~~~~~~~~~~~~~~~~~~

Traditional machine learning uses handwritten feature extraction and modality-specific machine learning algorithms
to label images or recognize voices. However, this method has several drawbacks in both time-to-solution and accuracy.
Today’s advanced deep neural networks use algorithms, big data, and the computational power of the GPU (and
other technologies) to change this dynamic. Machines are now able to learn at a speed, accuracy, and scale that are
driving true artificial intelligence and AI Computing.

Deep learning is used in the research community and in industry to help solve many big data problems such as computer
vision, speech recognition, and natural language processing. Practical examples include:

* Vehicle, pedestrian and landmark identification for driver assistance
* Image recognition
* Speech recognition and translation
* Natural language processing
* Life sciences

The influence of deep-learning on the market is significant with the design of commodity products such as the Intel
MIC and NVIDIA Tesla being heavily impacted. Silicon is being dedicated to deep learning workloads and the scientific
workloads for these products will need to adapt to leverage this silicon.
