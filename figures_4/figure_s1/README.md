## figure_S1

This folder contains the code to replicate figure S1. 

0. <code>req_lib/</code>: directory where local installation of the libraries necessary for the simulations are saved. Automatically created when running <code>001_requirements.R</code>.

1. <code>001-004</code>: files containing all the required packages and custom functions for the simulations. 

2. File <code>122_SimulationScript.R</code>: file that contains the calls for simulations. All outputs are saved in the file <code>206_figS1.R</code>.

## Instructions:

0. To install all packages for the simulation, run the following command line:

    - <code>Rscript 001_requirements.R</code>

    Alternatively, open the file <code>001_requirements.R</code> in RStudio with the working directory <code>./figures_4/figure_s1/</code>, and run the entire script.


1. To perform the simulations, run the following line: 

    - <code>Rscript 122_SimulationScript.R</code>

    Alternatively, open the file <code>122_SimulationScript.R</code> in RStudio with the working directory <code>./figures_4/figure_s1/</code>, and run the entire script.

