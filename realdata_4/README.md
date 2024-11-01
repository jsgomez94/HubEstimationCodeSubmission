## Real Data Analysis.

In the directory <code>./realdata_4/</code>, we provide the code to reproduce the real data analysis of hub detection for gene-expression levels. In this document, we provide an overall description of the directory/file structure. Then, we provide instructions on how to reproduce the results.

### Directory structure

As can be noted, both the files and the directories are numbered with numbers from 000 to 350. The natural order to reproduce the results is to run files in increasing order, starting with file <code>realdata_4\100_ProcessingGeneData\111_ProstateCancer.Rmd</code>, and ending with the file <code>realdata_4\300_RealDataAnalysis\351_ProstateAnalysis5.Rmd</code>. Here, we provide a description of each of the directories.

- Directory <code>100</code>: Contains the original gene-expression data prior to processing, and the code for processing it down to a clean and reduced dataset. The resulting clean dataset is saved in the files <code>112_GeneDataClean.csv</code>. Due to changes in the TCGA protocols of gene-counts, and changes in the names of genes and isoforms, the data cannot be fully retreived from the original source. Despite this, we provide the original downloaded files in CSV format in this directory.

- Directory <code>200</code>: Contains the instructions for how to download the clinical data associated with the genetic measurements, code to clean the clinical data, and code to merge the clinical and genetic data into a single dataframe.  

- Directory <code>300</code>: Contains the files that perform our old IPC-HD analysis without controlling for clinical variables, as well as our new analysis controlling for clinical variables.  

### Instructions

As mentioned before, the results can be reproduced simply by running each of the data files in increasing numeric order. Each code file is designed to save its outputs indexed with a higher number, and for the next script to load the data of the last step. For example, the file <code>200_ProcessingClinicalData/211_CleaningClinicalData.Rmd</code> outputs the file <code>200_ProcessingClinicalData/212_clinical_reduced.csv</code>, which is then loaded by the next script <code>200_ProcessingClinicalData/221_MatchingClinicalGenetic.Rmd</code>. 

The only step that does not follow from running the files in sequential order is the step of retreiving the clinical data from the TCGA online repository, which is necessary to run file <code>200_ProcessingClinicalData/211_CleaningClinicalData.Rmd</code>. While we provide the raw-clinical data in our repository, it can be retreived again (as of November 2024) by following the instructions provided in the file <code>200_ProcessingClinicalData/README.pdf</code>. 