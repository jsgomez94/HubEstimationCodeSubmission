#################################################
#################################################
#################################################
##
## In the following document, we introduce the
## script that has to be run to get the 
## simulation outputs.
##
#################################################
#################################################
#################################################


#################################################
## Sourcing:
#################################################
source("001_requirements.R"); search()
source("002_GeneratingMatrixSparse.R")
source("003_BasicMatrixTransformations.R")
source("004_ModulePlotting.R")

source("011_Method_MatrixThresholding.R")
source("012_Method_VariableScreening.R")
source("013_Method_IPCHD.R")

source("021_Method_BicGlasso.R")
source("022_Method_GLASSO.R")
source("023_Method_HWGLASSO.R")
source("024_Method_HGLASSO.R")

source("031_Pretuning_HWGLASSO.R")
source("032_Pretuning_HGLASSO.R")

source("041_Estimation_HubSelection.R")
source("051_Simulation_CreatingParameters.R")

index  <- 1
main_folder <- "000_HglHwglPretraining/"
source(paste0(main_folder, index, "02_PretrainingFunction.R"))




#################################################
#################################################
## Step 1: Read imputs:
print("#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("#-----------------------Reading inputs")

input                <- commandArgs(trailingOnly=TRUE)
id_task              <- as.numeric(input[1])
runtype              <- as.numeric(input[2])

args <- CreateParameters(id_task, runtype)
print("#----------------------Verifying Inputs")
print(args)

#################################################
#################################################
## Step 2: Setup run numbers and create folders
print("#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("#-----------------------Creating Folders")

print(getwd())
## Runtype: pretrainings.
runtype_name         <- c("pretrainings", "experiments", "outputs")[runtype]
subfolder            <- paste0(main_folder, runtype_name, index, "/")
subfolder_data       <- paste0(subfolder, "data/")
subfolder_logs       <- paste0(subfolder, "logs/")
subfolder_plots      <- paste0(subfolder, "plots/")
if (!dir.exists(subfolder)) {
       dir.create(subfolder)
}
if (!dir.exists(subfolder_data)) {
       dir.create(subfolder_data)
}
if (!dir.exists(subfolder_logs)) {
       dir.create(subfolder_logs)
}
if (!dir.exists(subfolder_plots)) {
       dir.create(subfolder_plots)
}

## Creating pretraining folders in 200_SimWithHglMethod
main_folder200       <- "200_SimWithHglMethod/"
subfolder200         <- paste0(main_folder200, runtype_name, index, "/")
subfolder_data200    <- paste0(subfolder200, "data/")
subfolder_logs200    <- paste0(subfolder200, "logs/")
subfolder_plots200   <- paste0(subfolder200, "plots/")
if (!dir.exists(subfolder200)) {
       dir.create(subfolder200)
}
if (!dir.exists(subfolder_data200)) {
       dir.create(subfolder_data200)
}
if (!dir.exists(subfolder_logs200)) {
       dir.create(subfolder_logs200)
}
if (!dir.exists(subfolder_plots200)) {
       dir.create(subfolder_plots200)
}

## Creating pretraining folders in 300_SimExploringHwglSparsity
main_folder300       <- "300_SimExploringHwglSparsity/"
subfolder300         <- paste0(main_folder300, runtype_name, index, "/")
subfolder_data300    <- paste0(subfolder300, "data/")
subfolder_logs300    <- paste0(subfolder300, "logs/")
subfolder_plots300   <- paste0(subfolder300, "plots/")
if (!dir.exists(subfolder300)) {
       dir.create(subfolder300)
}
if (!dir.exists(subfolder_data300)) {
       dir.create(subfolder_data300)
}
if (!dir.exists(subfolder_logs300)) {
       dir.create(subfolder_logs300)
}
if (!dir.exists(subfolder_plots300)) {
       dir.create(subfolder_plots300)
}




#################################################
#################################################
## Step 2: Run simulation scenarios:
print("#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("#------------------Running simulations")
assign(paste0("output", id_task), pretraining_fun(args = args))

#################################################
#################################################
## Step 3: Save all the outputs
print("#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("#-----------------------Saving Outputs")
tmp.env <- new.env()
assign(paste0("output", id_task),
       get(paste0("output", id_task)),
       pos = tmp.env)
assign(paste0("args", id_task),
       args,
       pos = tmp.env)

print(paste0(subfolder_data, "output", id_task, ".RData"))
## Saving results in 000_HglHwglPretraining
save(
       list = ls(all.names = TRUE, pos = tmp.env), envir = tmp.env, 
       file = paste0(subfolder_data, "output", id_task, ".RData"))
## Saving results in 200_SimWithHglMethod
save(
       list = ls(all.names = TRUE, pos = tmp.env), envir = tmp.env, 
       file = paste0(subfolder_data200, "output", id_task, ".RData"))
## Saving results in 300_SimExploringHwglSparsity
save(
       list = ls(all.names = TRUE, pos = tmp.env), envir = tmp.env, 
       file = paste0(subfolder_data300, "output", id_task, ".RData"))

warnings()
