## figure_s3

This folder contains the code to replicate figure S3. To run, it is necessary to have the github repository to a cluster with SLURM scheduling system. The overall structure is the following:

0. <code>req_lib/</code>: directory where local installation of the libraries necessary for the simulations are saved.

1. <code>001-004</code>: files containing all the required packages and custome functions for the simulations. 

2. Directory <code>100_ScreeningSimulations</code>: directory that contains the calls for simulations. All outputs are saved in the directory <code>100_ScreeningSimulations/outputs1/</code>.

## Instructions:

0. To install all packages for the simulation, run the following line:

    - <code>Rscript 001_requirements.R</code>

1. To perform the simulations, run the following line: 

    - <code>sbatch 100_ScreeningSimulations/3_ClusterPass100.sh</code>

2. To generate plots, run the following line of code:

    - <code>Rscript 501_ProcessingOutputsAestheticPlot.R</code>