.. _eurohpc:

Future HPC Hardware in Europe
-----------------------------

The European HPC Technology Platform, ETP4HPC, is an industry-led think-tank comprising of European HPC technology
stakeholders: technology vendors, research centres and end-users. The main objective of ETP4HPC is to define
research priorities and action plans in the area of HPC technology provision (i.e. the provision of supercomputing
systems). It has been responsible for the production and maintenance of the `European HPC Technology Strategic Research
Agenda (SRA) <http://www.etp4hpc.eu/en/%20news/18-strategic-research-agenda-update.html>`_, a document that serves as a
mechanism to provide contextual guidance to European researchers and businesses as well as to guide EU priorities for
research in the Horizon 2020 HPC programme, i.e. it represents a roadmap for the achievement of European exascale
capabilities.

We have had numerous discussions of the E-CAM community software needs through our exchanges with ETP4HPC
during the course of our contributions to the SRA. The particular contribution from our discussion related to the
software needs for exascale computing within the ETP4HPC SRA report is shown in the paragraphs below:

  E-CAM has not committed itself to a single set of applications or use cases that can represented in such a
  manner, it is instead driven by the needs of the industrial pilot projects within the project (as well as the
  wider community). Taking into consideration the CECAM community and the industrial collaborations
  targeted by E-CAM, probably the largest exa-scale challenge is ensuring that the skillsets of the application
  developers from academia and industry are sufficiently up to date and are aligned with programming
  best practices. This means that they are at least competent in the latest relevant language specification
  (Fortran 2015, C++17,...) and aware of additional tools and libraries that are necessary (or useful) for application
  development at the exa-scale. For application users, this means that they have sufficient knowledge
  of architecture, software installation and typical supercomputing environment to build, test and run
  application software optimised for the target.

  While quite specific "key applications" are under continuous support by other CoEs, this is not the current
  model of E-CAM. E-CAM is more likely to support and develop a software installation framework (such as
  `EasyBuild <http://easybuild.readthedocs.io/en/latest/>`_) that simplifies building the (increasingly non-trivial)
  software stack of a particular application in a reliable, reproducible and portable way. Industry has already shown
  significant interest in this and E-CAM is particularly interested in extending the capabilities of EasyBuild to EsD
  architectures, performance analysis workflows and to new scientific software packages. Such an effort could easily be
  viewed as transversal since such developments could be leveraged by any other CoE.

One important focus of the SRA is the development of the “Extreme-Scale Demonstrators” (EsDs) that are vehicles to
optimise and synergise the effectiveness of the entire HPC H2020 programme, through the integration of R&D outcomes
into fully integrated HPC system prototypes.

The work in developing the EsDs will fill critical gaps in the H2020 programme, including the following activities:

* Bring technologies from FET-HPC projects closer to commercialisation.
* Combined results from targeted R&D efforts into a complete system (European HPC technology ecosystem).
* Provide the missing link between the three HPC pillars: technology providers, user communities (e.g. E-CAM)
  and infrastructure.

As one of the CoEs, E-CAM should aim to provide insight and input into the requirements of future exascale systems
based on lessons learnt from activities within E-CAM (e.g. software development and relevant performance optimisation
and scaling work). This would entail further knowledge and understanding within E-CAM on exploiting current
multi-petaflop infrastructures, what future exascale architectures may look like, as well as interaction and close
collaboration between E-CAM and other projects (i.e. the projects shown in Figure 12); these are also covered in
subsequent sections of this paper.

Emerging hardware architectures relevant to exascale computing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The European Commission supports a number of projects in developing and testing innovative architectures for next
generation supercomputers, aimed at tackling some of the biggest challenges in achieving exascale computing. They
often involve co-design involving HPC technologists, hardware vendors and code developer/end-user communities
in order to develop prototype systems. Some of these projects include:

* The `DEEP <http://www.deep-project.eu>`_ (Dynamic Exascale Entry Platform) projects (DEEP, DEEP-ER and DEEP-EST)
* The `Mont-Blanc <http://montblanc-project.eu/>`_ projects (Mont-Blanc 1, 2 and 3)
* The `PRACE PCP <http://www.prace-ri.eu/pcp/>`_ (Pre-Commercial Procurement) initiative

