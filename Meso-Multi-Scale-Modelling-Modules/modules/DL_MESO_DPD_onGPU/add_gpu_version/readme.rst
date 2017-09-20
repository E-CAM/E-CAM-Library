##################################################
First version of DL_MESO_DPD code for NVidia GPU
##################################################

.. sidebar:: Software Technical Information

  The information in this section describes the DL_MESO_DPD GPU versions as a whole.

  Language
    Fortran/CUDA-C (cuda toolkit 7.5)

  Documentation Tool
    ReST files

  Application Documentation
    See the `DL_MESO Manual <http://www.scd.stfc.ac.uk/SCD/resources/PDF/USRMAN.pdf>`_

  Relevant Training Material
    See `DL_MESO webpage <http://www.scd.stfc.ac.uk/SCD/support/40694.aspx>`_

  Licence
    BSD, v. 2.7 or later

.. contents:: :local:

This module implements the first version of the DL_MESO_DPD code on NVidia Graphical Processing Unit (GPU). More details about it can be found in the following sections.

Purpose of Module
_________________

.. Give a brief overview of why the module is/was being created.

In order to accelerate the DL_MESO_DPD code on the latest and future exascale hardware, a first version for NVidia GPU has been developed. This is only a starting point, it does NOT cover all the possible cases and it does NOT yet support multiple GPUs. However, it represent an HPC milestone for the application, complementing the already present parallel versions developed for shared and distributed memory (MPI/OpenMP).

In this version, the full computational workload is offloaded to the GPUs with the 
"H_MAINLOOP" call present in the "dlmesodpd.f90" file. In this way, the initialisation IO operation are left unaltered and fully compatible with the serial version. 
A major change compared to the serial version is in the algorithm used to find the particle-particle interaction forces: in order to guarantee better coalescent access for the CUDA-threads, the algorithm has been re-adapted to the GPU architecture reordering the cell-linked list arrays.

The FORTRAN files (mainly unaltered from the serial version) are saved in the "src" folder, while the CUDA files with their corresponding headers files are in stored in the "src_cuda" folder and "headers", respectively. The folder "bin" contains the Makefile. This arrangement of the folders
allow to use the hidden Eclipse IDE project files (.cproject, .project, .settings).



.. references would be nice here...

Background Information
______________________

This module is part of the DL_MESO_DPD code. Full support and documentation is available at:

* http://www.scd.stfc.ac.uk/SCD/support/40694.aspx
* http://www.scd.stfc.ac.uk/SCD/resources/PDF/USRMAN.pdf

To download the DL_MESO_DPD code you need to register at https://ccpforge.cse.rl.ac.uk/gf/. Please contact Dr. Micheal Seaton at Daresbury Laboratory (STFC) for further details.



Testing
_______

The DL_MESO code is developed using git version control. Currently the GPU version is under a branch named "add_gpu_version". After downloading the code, checkout to the GPU branch and look into the "DPD/gpu_version" folder, i.e:

* git clone DL_MESO_repository_path
* cd dl_meso
* git checkout gpu_version
* cd ./DPD/gpu_version
* make all

To compile and run the code you need to have installed the CUDA-toolkit and have a CUDA enabled GPU device (see http://docs.nvidia.com/cuda/#axzz4ZPtFifjw).

The current version has been tested ONLY for the Mixture_Large test case available in the DEMO/DPD folder. To run the case, compile the code using the "make all" command from the "bin" directory, copy the "FIELD" and "CONTROL" files in this directory and run "./dpd_gpu.exe".

Attention: the HISTORY file produced is currently NOT compatible with the serial version, because this is written in the C binary data format (Fortran files are organised in records, 
while C not. See https://scipy.github.io/old-wiki/pages/Cookbook/FortranIO.html). 

However, you can compare the "OUTPUT" and the "export" files to verify your results. For more details see the README.rst file in the "gpu_version" folder.



Performance
___________
Below is a table about the performance of the Mixture_Large case on different GPU cards compared to the serial version on a single core: 

==========================================  ==================== ========================= ============
            CPU or GPU card                  compute capability      time per cycle [s]     speedup
==========================================  ==================== ========================= ============
     Intel Ivy Bridge E5-2697v2  @2.7GHz             none               0.4740                  1.0 

     NVidia Tesla C1060                              1.3                0.2280                  2.1

     NVidia Tesla C2075                              2.0                0.1830                  2.6

     NVidia Tesla K40                                3.5                0.1011                  4.7

     NVidia Tesla K80                                3.7                0.0898                  5.3

     NVidia Tesla M60                                5.2                0.0978                  4.8

     NVidia Tesla P100                               6.0                0.0390                 12.2
==========================================  ==================== ========================= ============



Examples
________

See the Mixture_Large case in the DL_MESO manual.


Source Code
___________

.. link the source code

This module has been merged into DL_MESO code. It is composed of the
following commits (you need to be registered as developer):

* https://ccpforge.cse.rl.ac.uk/gf/project/dl_meso/scmgit/?action=ScmCommitDetail&scm_commit_id=110906
* https://ccpforge.cse.rl.ac.uk/gf/project/dl_meso/scmgit/?action=ScmCommitDetail&scm_commit_id=111357

.. IF YOUR MODULE IS A SEPARATE REPOSITORY

.. The source code for this module can be found in: URL.

.. CLOSING MATERIAL -------------------------------------------------------

.. Here are the URL references used

.. _nose: http://nose.readthedocs.io/en/latest/
