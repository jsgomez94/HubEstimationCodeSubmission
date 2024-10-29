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

source("001_requirements.R")
source("002_GeneratingMatrixSparse.R")
source("003_BasicMatrixTransformations.R")
source("004_EstimationMeasures.R")

source("100_ScreeningSimulations/1_SimulationFunction.R")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")


#################################################
#################################################
## Step 1: Read imputs:
print("#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("#-----------------------Reading inputs")

input = commandArgs(trailingOnly=TRUE)
id_task = as.numeric(input)
args = CreateParameters(id_task)
print("#----------------------Verifying Inputs")
print(args)

#################################################
#################################################
## Step 2: Run simulation scenarios:
print("#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("#------------------Running simulations")
assign(paste0("output",id_task), FullSimulation(args = args))

#################################################
#################################################
## Step 3: Save all the outputs
print("#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("#-----------------------Saving Outputs")
tmp.env <- new.env() 
assign(paste0("output",id_task), 
       get(paste0("output",id_task)), 
       pos = tmp.env)
assign(paste0("args",id_task),
       args,
       pos = tmp.env)
print(paste0("100_ScreeningSimulations/outputs1/output", id_task, ".RData"))
save(list=ls(all.names=TRUE, pos = tmp.env), envir = tmp.env,
     file = paste0("100_ScreeningSimulations/outputs1/output", id_task, ".RData"))

