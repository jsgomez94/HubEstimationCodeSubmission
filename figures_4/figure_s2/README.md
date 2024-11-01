## figure_s2

This folder contains the code to replicate figure S2. This can be reproduced in a personal computer.

0. <code>req_lib/</code>: directory where local installation of the libraries necessary for the simulations are saved. Automatically created when running <code>001_requirements.R</code>.

1. <code>001-003</code>: files containing all the required packages and custome functions for the simulations. 

2. File <code>004</code>: file that contains the calls for simulations. All outputs are saved in the file <code>005_figS2.R</code>.

## Instructions:

0. To install all packages for the simulation, run the following line:

    - <code>Rscript 001_requirements.R</code>

    Alternatively, open the file <code>001_requirements.R</code> in RStudio with the working directory <code>./figures_4/figure_s2/</code>, and run the entire script.

1. To perform the simulations, run the following line: 

    - <code>Rscript 004_ExploringEigengap.R</code>

    Alternatively, open the file <code>004_ExploringEigengap.R</code> in RStudio with the working directory <code>./figures_4/figure_s2/</code>, and run the entire script.
