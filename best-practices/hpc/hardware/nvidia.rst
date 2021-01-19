.. _gpu:

NVIDIA GPU
----------

The new NVIDIA `Tesla V100 <https://www.nvidia.com/en-us/data-center/volta-gpu-architecture/>`_ accelerator
incorporates the new Volta GV100 GPU. Equipped with 21 billion transistors, Volta delivers over 7.5 Teraflops per
second of double precision performance, ∼1.5x increase compared to the its predecessor, the Pascal GP100 GPU. Moreover,
architectural improvements include:

* A tensor core is unit that multiplies two 4×4 FP16 matrices, and then adds a third FP16 or FP32 matrix to the
  result by using fused multiply–add operations, and obtains an FP32 result that could be optionally demoted to
  an FP16 result. Tensor cores are intended to speed up the training of neural networks.
* Tesla V100 uses a faster and more efficient HBM2 implementation. HBM2 memory is composed of memory
  stacks located on the same physical package as the GPU, providing substantial power and area savings compared
  to traditional GDDR5 memory designs, thus permitting more GPUs to be installed in servers. In addition
  to the higher peak DRAM bandwidth on Tesla V100 compared to Tesla P100, the HBM2 efficiency on V100 GPUs
  has been significantly improved as well. The combination of both a new generation HBM2 memory from Samsung,
  and a new generation memory controller in Volta, provides 1.5x delivered memory bandwidth versus
  Pascal GP100, and greater than 95% memory bandwidth efficiency running many workloads.
* NVlink 2.0, which is a high-bandwidth bus between multiple GPUs, and between the CPU and GPU. Compared to NVLink
  on Pascal, NVLink 2.0 on V100 increases the signaling rate from 20 to 25 Gigabits/second. Each link now provides
  25 Gigabytes/second in each direction. The number of links supported has been increased from four to six pushing
  the supported GPU NVLink bandwidth to 300 Gigabytes/second. The links can be used exclusively for GPU-to-GPU
  communication as in the DGX-1 with V100 topology shown in Figure 2, or some combination of GPU-to-GPU and
  GPU-to-CPU communication as shown in Figure 3 (currently only available in combination with Power8/9 processors).

The tensor core of the Volta was explicitly added for deep learning workloads. The `NVIDIA Deep Learning SDK
<https://developer.nvidia.com/deep-learning-software>`_ provides
powerful tools and libraries for designing and deploying GPU-accelerated deep learning applications. It includes
libraries for deep learning primitives, inference, video analytics, linear algebra, sparse matrices, and multi-GPU
communications.

Feedback for software developers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Several approaches have been developed to exploit the full power of GPUs: from parallel computing platform and
application programming interface specific for NVidia GPU, like `CUDA 9.0
<http://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html>`_, to the latest version of `OpenMP 4.5
<http://www.openmp.org/updates/openmp-4-5-specs-released/>`_ which
contains directives to offload computational work from the CPU to the GPU. While CUDA currently is likely to achieve
best performance from the device, OpenMP allows for better portability of the code across different architectures.
Finally, the `OpenACC <https://www.openacc.org/specification>`_  open standard is an intermediate between the two, more
similar to OpenMP than CUDA, but allowing better usage of the GPU. Developers are strongly advised to look into these
language paradigms.

Moreover, it is fundamental to consider that there the several issues linked to hybrid architectures, like CPU-GPU and
GPU-GPU bandwidth communication (the latest greatly improved through NVlink), direct access through `Unified Virtual
Addressing <https://devblogs.nvidia.com/parallelforall/beyond-gpu-memory-limits-unified-memory-pascal/>`_, the presence
of new APIs for programming (such as `Tensor Core
<https://devblogs.nvidia.com/parallelforall/cuda-9-features-revealed>`_ multiplications specifically designed for deep
learning algorithms).

Finally, it is important to stress the improvements made by NVidia on the implementation of `Unified Memory
<https://devblogs.nvidia.com/parallelforall/beyond-gpu-memory-limits-unified-memory-pascal/>`_. This
allows the system to automatically migrate data allocated in Unified Memory between host and device so that it looks
like CPU memory to code running on the CPU, and like GPU memory to code running on the GPU making programmability
greatly simplified.

At this stage, GPU programming is quite mainstream and there are many training courses available online, see for
example the `NVidia education site <https://developer.nvidia.com/cuda-education>`_ for material related to CUDA and
OpenACC. Material for OpenMP is more limited, but as an increasing number of compilers begin to support the OpenMP 4.5
standard, we expect the amount of such material to grow (see `this presentation on performance of the Clang OpenMP 4.5
implementation on NVIDIA gpus
<http://on-demand.gputechconf.com/gtc/2016/presentation/s6510-jeff-larkin-targeting-gpus-openmp.pdf>`_ for a status
report as of 2016).
