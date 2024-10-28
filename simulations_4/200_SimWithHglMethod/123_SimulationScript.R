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
main_folder <- "200_SimWithHglMethod/"
source(paste0(main_folder, index, "22_SimulationFunction.R"))


#################################################
#################################################
## Step 1: Read imputs:
print("#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("#-----------------------Reading inputs")

input                <- commandArgs(trailingOnly = TRUE)
id_task              <- as.numeric(input[1]) %/% 10
id_mircorun          <- as.numeric(input[1]) %% 10
runtype              <- as.numeric(input[2])


args <- CreateParameters(id_task, runtype)
print("#----------------------Verifying Inputs")
print(args)

#################################################
#################################################
## Step 2: Setup run numbers and create folders
print("#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("#-----------------------Creating Folders")

## runtype = 1 (experiment), runtype = 2 (full simulation)
runtype_name         <- c("pretrainings", "experiments", "outputs")[runtype]
subfolder            <- paste0(main_folder, runtype_name, index, "/")
subfolder_data       <- paste0(subfolder, "data/")      ## Folder with simulation performance outcomes.
subfolder_logs       <- paste0(subfolder, "logs/")      ## Folder with console log files. Useful for debugging.
subfolder_plots       <- paste0(subfolder, "plots/")    ## Folder to save preliminary performance plots of HWGL.

if (!dir.exists(subfolder_data)) {
       dir.create(subfolder_data)
}
if (!dir.exists(subfolder_logs)) {
       dir.create(subfolder_logs)
}
if (!dir.exists(subfolder_plots)) {
       dir.create(subfolder_plots)
}

#################################################
#################################################
## Step 2: Run simulation scenarios:
print("#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("#------------------Running simulations")
assign(paste0("output", id_task), FullSimulation(args = args, index = index))


#################################################
#################################################
## Step 3: Save SIM outputs
print("#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("#-----------------------Saving Outputs")
tmp_env_sim <- new.env()
assign(paste0("output", id_task,"_", id_mircorun),
       get(paste0("output", id_task)),
       pos = tmp_env_sim)
assign(paste0("args", id_task),
       args,
       pos = tmp_env_sim)

print(paste0(subfolder_data, "output", id_task,"_", id_mircorun, ".RData"))
save(
       list = ls(all.names = TRUE, pos = tmp_env_sim), envir = tmp_env_sim,
       file = paste0(subfolder_data, "output", id_task,"_", id_mircorun, ".RData"))


warnings()