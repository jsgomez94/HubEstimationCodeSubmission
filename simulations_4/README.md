# HubEstimationCodeSubmission

This folder contains the code corresponding to the numerical simulations results found in Section 5 in the main paper, and Section 5.4 of the Supplementary Material. First, we here provide a description of the overall structure of the files. Second, we describe in detail the steps required to replicate the simulations, 

## Overall Structure of Files/Directories:

- Directory <code>req_lib</code>: directory where the required packages will be saved as a local library. 

- Files 001-051: these files contain all the required functions to perform simulations. They are divided into specific tasks, such as importing required packages (<code>001_requirements.R</code>), calculating our IPC-HD method (<code>013_Method_IPCHD.R</code>), calculating the GLASSO, HGL and HWGL (<code>022_Method_GLASSO.R</code>, <code>023_Method_HWGLASSO.R</code>, <code>024_Method_HGLASSO.R</code>) or generating the parameters for our simulations (<code>051_CreatingParameters.R</code>).

- Files in directories 100-400: These directories contain the calls for the numerical simulations. Each folder corresponds to one method. The files can be used to run (A) a small debugging example to ensure things run smoothly, (B) reduced experiments with a total of 20 simulation replicates, (C) full simulations with 100 simulation replicates.

- Files/Directories 500: After reduced experiments are run, the file <code>511_Outcomes_DataAggregationExperiments.R</code> aggregates the experiments data, and saves it to the directory <code>500_AggregatedDataExperiments</code>. Then, files <code>512_Outcomes_ResultPlotsExperiments.R</code>, <code>513_Outcomes_TimeResultPlotsExperiments.R</code> and <code>514_Outcomes_DegreeByMethodPlotsExperiments.R</code> generate plots with the aggregated data. 

- Files/Directories 600: After reduced experiments are run, the file <code>611_Outcomes_DataAggregationFull.R</code> aggregates the full simulation data, and saves it to the folder <code>600_AggregatedDataFull</code>. Then, files <code>612_Outcomes_ResultPlotsFull.R</code>, <code>613_Outcomes_TimeResultPlotsFull.R</code>, and <code>614_Outcomes_DegreeByMethodPlotsFull.R</code> generate plots with the aggregated data. 

## Simulation Rerunning Instructions:

The simulations are run in a linux cluster with SLURM scheduling system. 


0. **Clone Repository:** First, clone the github repository <em>HubEstimationCodeSubmission</em> to a linux cluster with SLURM scheduling system.

1. **Install Requirements:** Install all packages required for our simulations by running

    - <code>Rscript 001_requirements.R</code>

    Ensure that all packages are properly installed by reviewing the directory <code>req_lib/</code>.

2. **Debugging Pretraining of HGL and HWGL:** the methods HGL and HWGL are designed specially for hub recovery, but require a careful fine-tuning of their tuning parameters, more demanding than the GLASSO method. To obtain these pre-training parameters, you must first run the pretraining scripts. 

    In the linux cluster terminal, enter the directory <code>./simulations_4/</code>. To ensure that things run smoothly, run the command,

    - <code>sbatch 000_HglHwglPretraining/110_ClusterPassPretraining0.sh</code>

    This generates the pretraining of the HGL and HWGL for a toy example of index 0. Verify that directories <code>/pretrainings1/</code> are created inside of the folders <code>000_HglHwglPretraining/</code>, <code>200_SimWithHglMethod/</code>, and <code>300_SimExploringHwglSparsity/</code>. In each of the  <code>/pretrainings1/</code> folders, you should find the directory <code>data/</code> containing the pretuning parameters in RData format, and <code>logs/</code> with the command-line logs helping with debugging.

3. **Pretraining the HGL and HWGL:** To pre-train all simulation scenarios considered, run the following commands:

    - <code>sbatch 000_HglHwglPretraining/111_ClusterPassPretraining100.sh</code>
    - <code>sbatch 000_HglHwglPretraining/112_ClusterPassPretraining200.sh</code>
    - <code>sbatch 000_HglHwglPretraining/113_ClusterPassPretraining500.sh</code>

    This should save the pre-trained tuning parameters in the <code>/pretrainings1/</code> folders of each of the directories <code>000_HglHwglPretraining/</code>, <code>200_SimWithHglMethod/</code>, and <code>300_SimExploringHwglSparsity/</code>. Check the log files in <code>/pretrainings1/logs/</code> for any additional debugging needed.

4. **Debugging Systematic Simulations:** Once pretraining is completed, in the linux cluster terminal, and enter the directory  <code>./simulations_4/</code>. To ensure that things run smoothly, run the commands,

    - <code>sbatch 100_SimOnlyIpchdMethods/130_ClusterPassExperiment0.sh</code>
    - <code>sbatch 200_SimWithHglMethod/130_ClusterPassExperiment0.sh</code>
    - <code>sbatch 300_SimExploringHwglSparsity/130_ClusterPassExperiment0.sh</code>
    - <code>sbatch 400_SimExploringGlSparsity/130_ClusterPassExperiment0.sh</code>
    
    Check the log file <code>100_SimOnlyIpchdMethods/experiments1/logs/output0.out</code> to verify that the IPC-HD method ran properly. Often, warnings related to the packages may occur, but check for errors. Check whether the data file <code>100_SimOnlyIpchdMethods/experiments1/data/output0_0.RData</code> is created, which ensures that the simulation was completed and successful. You can explore the log and data files in the 200, 300 and 400 directories to ensure the GLASSO, HGL and HWGL methods also ran successfully. 


3. **Running Simulation Experiments:** Once debugging experiments ran succesfully, you can run preliminary simulation experiments. We have a total of 288 simulation scenarios, corresponding to different choices of dimension p, sample size n, hub strength ph, among other parameters. For each of these simulation scenarios, we perform 20 simulation replicates. To do this, we request 10 cluster nodes, and ask each to perform 2 simulation replicates per simulation scenarios. To run this, in a cluster terminal, enter the directory <code>./simulations_4/</code>, and run the lines,

    - <code>sbatch 100_SimOnlyIpchdMethods/131_ClusterPassExperiment100.sh</code>
    - <code>sbatch 100_SimOnlyIpchdMethods/132_ClusterPassExperiment200.sh</code>
    - <code>sbatch 100_SimOnlyIpchdMethods/133_ClusterPassExperiment500.sh</code>
    - <code>sbatch 200_SimWithHglMethod/131_ClusterPassExperiment100.sh</code>
    - <code>sbatch 200_SimWithHglMethod/132_ClusterPassExperiment200.sh</code>
    - <code>sbatch 200_SimWithHglMethod/133_ClusterPassExperiment500.sh</code>
    - <code>sbatch 300_SimExploringHwglSparsity/131_ClusterPassExperiment100.sh</code>
    - <code>sbatch 300_SimExploringHwglSparsity/132_ClusterPassExperiment200.sh</code>
    - <code>sbatch 300_SimExploringHwglSparsity/133_ClusterPassExperiment500.sh</code>
    - <code>sbatch 400_SimExploringGlSparsity/131_ClusterPassExperiment100.sh</code>
    - <code>sbatch 400_SimExploringGlSparsity/132_ClusterPassExperiment200.sh</code>
    - <code>sbatch 400_SimExploringGlSparsity/133_ClusterPassExperiment500.sh</code>
    
    The logs for the IPC-HD reduced experiments are saved in <code>100_SimOnlyIpchdMethods/experiments1/logs/</code>, indexed from 10 to 2889. The simulation data is saved in RData files in <code>100_SimOnlyIpchdMethods/experiments1/data/</code> 1-288. Similar log and data directories for the GLASSO, HGL and HWGL can be found in the folders 200, 300, 400. 

4. **Generating Simulation Experiment Plots:** Once all preliminary simulation experiments are complete and the data is saved, you can generate simulation plots. For this, in the command line terminal enter the directory <code>./simulations_4/</code>, and run

    - <code>Rscript 511_Outcomes_DataAggregationExperiments.R</code> 
    
    This saves aggregated data in the directory <code>500_AggregatedDataExperiments/data_all</code>. Plots that verify performance in terms of true positive rate (TPR) and false positive rate (FPR) of hub detection are saved in the directory <code>500_AggregatedDataExperiments/plots_all/</code> by running
    
    - <code>Rscript 512_Outcomes_ResultPlotsExperiments.R</code> 
    
    Plots that verify performance in terms of computational time are saved in the directory <code>500_AggregatedDataExperiments/time_all/</code> by running the script 
    
    - <code>Rscript 513_Outcomes_TimeResultPlotsExperiments.R</code> 
    
    Finally, plots that compare the degrees of hubs and non-hubs by method are saved in the directory <code>500_AggregatedDataExperiments/bymethod_all/</code> generated by running,

    - <code>Rscript 514_Outcomes_DegreeByMethodPlotsExperiments.R</code>


5. **Running Full Simulations:** You can also run full numerical simulations. For each of the 288 simulation scenarios, we perform 100 simulation replicates. To do this, we request 10 cluster nodes, and ask each to perform 10 simulation replicates per simulation scenarios. To run this, in a cluster terminal, enter the directory <code>./simulations_4/</code>, and run the lines,
    
    - <code>sbatch 100_SimOnlyIpchdMethods/141_ClusterPassFull100.sh</code>
    - <code>sbatch 100_SimOnlyIpchdMethods/142_ClusterPassFull200.sh</code>
    - <code>sbatch 100_SimOnlyIpchdMethods/143_ClusterPassFull500.sh</code>
    - <code>sbatch 200_SimWithHglMethod/141_ClusterPassFull100.sh</code>
    - <code>sbatch 200_SimWithHglMethod/142_ClusterPassFull200.sh</code>
    - <code>sbatch 200_SimWithHglMethod/143_ClusterPassFull500.sh</code>
    - <code>sbatch 300_SimExploringHwglSparsity/141_ClusterPassFull100.sh</code>
    - <code>sbatch 300_SimExploringHwglSparsity/142_ClusterPassFull200.sh</code>
    - <code>sbatch 300_SimExploringHwglSparsity/143_ClusterPassFull500.sh</code>
    - <code>sbatch 400_SimExploringGlSparsity/141_ClusterPassFull100.sh</code>
    - <code>sbatch 400_SimExploringGlSparsity/142_ClusterPassFull200.sh</code>
    - <code>sbatch 400_SimExploringGlSparsity/143_ClusterPassFull500.sh</code>
    
    The logs for the IPC-HD full simulations are saved in <code>100_SimOnlyIpchdMethods/outputs1/logs/</code>, indexed from 10 to 2889. The simulation data is saved in RData files in <code>100_SimOnlyIpchdMethods/outputs1/data/</code> 1-288. Similar log and data directories for the GLASSO, HGL and HWGL can be found in the folders 200, 300, 400. 

6. **Generating Simulation Experiment Plots:** Once all full simulations are complete, you can generate simulation plots. For this, in the command line terminal enter the directory <code>./simulations_4/</code>, and run
    
    - <code>Rscript 611_Outcomes_DataAggregationFull.R</code>
    
    This saves aggregated data in the directory <code>600_AggregatedDataFull/data_all</code>. Plots that verify performance in terms of true positive rate (TPR) and false positive rate (FPR) of hub detection are generated by running
    
    - <code>Rscript 612_Outcomes_ResultPlotsFull.R</code>
    
    Plots are saved in the directory <code>600_AggregatedDataFull/plots_all</code>. Plots that verify performance in terms of computational time are generated by running the  
    
    - <code>Rscript 613_Outcomes_TimeResultPlotsFull.R</code> 
    
    Plots are saved in the directory <code>600_AggregatedDataFull/time_all</code>. Finally, plots that compare the degrees of hubs and non-hubs by method are generated by running,

    - <code>Rscript 614_Outcomes_DegreeByMethodPlotsFull.R</code>
