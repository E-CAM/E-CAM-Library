.. _fpga:

FPGA
----

Field Programmable Gate Arrays (FPGAs) are semiconductor devices that are based around a matrix of configurable
logic blocks (CLBs) connected via programmable interconnects. FPGAs can be reprogrammed to desired application
or functionality requirements after manufacturing. This feature distinguishes FPGAs from Application Specific
Integrated Circuits (ASICs), which are custom manufactured for specific design tasks.

Xilinx Ultrascale FPGAs and ARM processors have been proposed by the EuroEXA project as a new path towards exascale.
EuroEXA is an EU Funded 20 Million Euro for an `ARM+FPGA Exascale Project
<https://www.hpcwire.com/2017/09/07/eu-funds-20-million-euro-armfpga-exascale-project/>`_ which will lead Europe
towards exascale, together with `ExaNeSt <http://www.exanest.eu/>`_, `EcoScale <http://www.ecoscale.eu/>`_ and `ExaNoDe
<http://www.exanode.eu/>`_ projects, scaling peak performance to 400 PFLOP in a peak
system power envelope of 30MW; over four times the performance at four times the energy efficiency of today’s HPC
platforms.

Feedback for software developers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Despite their high efficiency in performance and power consumption, FPGA are known for being difficult to program.
`OpenCL for FPGA <https://www.altera.com/support/training/course/oopncl100.html>`_ is an example of programming language
for FPGA which we recommend, particularly considering that these new technologies will be soon available within the
E-CAM community through the EuroEXA project.
