.. _performance:


PaPIM code performance analysis
===============================

The core ingredient for calculating time-correlation functions in PaPIM is 
Monte Carlo sampling of independent phase space points to be used as initial 
conditions for the dynamics. 
Results of the calculations converge with a, large, number of sampled points 
so the computational efficiency is highly dependent on the optimization and 
parallelization of the sampling procedure. 
In the current version of the code, the sampling procedure was parallelized 
using the Message Passing Interface (MPI), and a thorough analysis of the 
code's performance was made.

Three independent performance analyses of the PaPIM code were conducted. 
Two were carried out by the E-CAM programmers at 
`Maison de la Simulation <http://www.maisondelasimulation.fr/en/index.php?a>`_
and `IDRIS <http://www.idris.fr>`_ and at 
`Juelich Supercopmuting Center <http://www.fz-juelich.de/ias/jsc/EN/Home/home
\_node.html>`_, 
while the third was conducted by the Performance Optimisation and Productivity 
team of the `POP Center of Excellenc <https://pop-coe.eu/>`_.
The three conducted performance analysis are referred as PA1, PA2 and PA3, 
respectively, in the following text. 

In PA1 a strong scaling analysis of the PaPIM code using the 
`Scalasca <http://www.scalasca.org>`_ analysis tools, as well as the internal 
PaPIM code calculation time outputs has been performed. 
The analysis was conducted on the :math:`\text{CH}_{5}^{+}` system and performed 
on the `JURECA <http://www.fz-juelich.de/ias/jsc/EN/Expertise/Supercomputers/
JURECA/Configuration/Configuration_node.html>`_ 
cluster at the JSC. 
Figure \ref{} displays the results of the performed strong scaling 
tests.

.. image:: ./PaPIM_CH5+_analysis.png
   :width: 50 %
   :align: center


In PA2 a parallel efficiency test on the `JUQUEEN <http://www.fz-juelich.de/
ias/jsc/EN/Expertise/Supercomputers/JUQUEEN/Configuration/Configuration_node.html>`_ 
cluster at JSC has been conducted. 
Results are displayed in Figure \ref{PaPIM_fig2}. 
In PA2 the code has been successfully executed on 131,072 processor cores.

.. image:: ./PaPIM_parallel_efficiency.png
   :width: 50 %
   :align: center




In PA3 a detailed analysis of the code's performance was conducted using 
the `extrae 3.4.1 <https://tools.bsc.es/extrae>`_ software. 
The analysis was made on the OH system, and calculations were performed on the 
Fionn cluster of the `Irish Centre for High-End Computing (ICHEC) <https://www.ichec.ie/
about/infrastructure/fionn>`_ with 4 and 32 processor cores. 
The main analysis results obtained with 4 and 32 processor cores 
are shown in Figure \ref{PaPIM_fig3}.

