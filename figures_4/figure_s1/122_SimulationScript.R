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
source("004_ModulePlotting.R")
source("121_SimulationFunction.R")


cbPalette <- c("#999999", "#E69F00", "#56B4E9",
               "#009E73", "#F0E442", "#0072B2",
               "#D55E00", "#CC79A7")

set.seed(20)

#################################################
#################################################
## Step 1: Read imputs:
print("#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("#-----------------------Reading inputs")

## Parameters of simulation:
args <- list(
    p = 500, T0 = 500, r = 5,
    diagonal_shift = 5,
    ph = 0.4, pnh = 0.05, pneff = 0.01,
    hmin = 4, hmax = 6,
    nhmin = 4, nhmax = 6,
    neffmin = 4, neffmax = 6,
    shuffle = FALSE, type = "unif",
    threshold = 100)

print("#----------------------Verifying Inputs")
print(args)

#################################################
#################################################
## Step 2: Run simulation scenarios:
print("#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("#------------------Running simulations")
FullSimulation(args = args)

