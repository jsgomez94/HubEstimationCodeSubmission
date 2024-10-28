#################################################
#################################################
#################################################
##
## In the following document, we introduce the
## functions that run the simulations and
## estimate and turn them into manageable outputs
##
#################################################
#################################################

#################################################
#################################################
#################################################
#################################################
#################################################
#################################################

#################################################
#################################################
## CreateParameters:
##    Function that, given a particular simulation
##    ID id_task, returns the simulation parameters
##    corresponding to such task ID. 
##
##  INPUTS
##    id_task  : ID of the current simulation task.
##                If id_task==0, is a small debugging example.
##                If id_task%in$1:288, simulations with
##                  different choices of p,T,n, ph, pnh, etc.
##    runtype     : numeric. determines if the run is for 
##                    runtype== 0: pretraining. 
##                    runtype== 1: reduced size experiment (2 repetitions)
##                    runtype== 1: full size simulation (10 repetitions).
##    
##  OUTPUT:
##    id_task : saving the original input id_task into the output.
##    p       : Total dimension of pxp matrix.
##    T0      : Size of T0xT0 highly connected submatrix.
##    r       : number of hubs.
##    ph      : Probability of hub connectivity 
##    pnh     : Probability of connection for variables 
##                in the high-connection T0xT0 submatrix.
##    pneff   : Probability of connection for all
##                variables outside of the T0xT0 
##                sumatrix.
##    diagonal_shift : 
##                Size of minimum eigenvalue of the 
##                precision matrix.
##    shuffle : If true, shuffles rows/columns to a random
##                position.
##    type    : whether the distribution signed-uniform
##                or zero-mean normal.
##    hmin    : Used when type = "unif". Minimum absolute 
##                value of hub entries.
##    hmax    : Used when type = "unif". Maximum absolute 
##                value of hub entries. 
##    nhmin   : Used when type = "unif". Minimum absolute 
##                value of entries in T0xT0 submatrix 
##                not related to hub variables.
##    nhmax   : Used when type = "unif". Maximum absolute 
##                value of entries in T0xT0 submatrix 
##                not related to hub variables.
##    neffmin : Used when type = "unif". Minimum absolute 
##                value of entries outside of the 
##                T0xT0 submatrix 
##    neffmin : Used when type = "unif". Minimum absolute 
##                value of entries outside of the 
##                T0xT0 submatrix 
##    T0_prop : ratio T0 / p.
##    n_prop  : ratio n / p.
##    nsim    : number of simulation repetitions used in this node.
##    running_days : time limit (in days) for task in the cluster call.
##    threshold : Deprecated. IPC-HD overestimation of s. 
##    M       : Deprecated. Number of resamples for parameter tuning.
##
CreateParameters <- function(id_task, runtype = c(1, 2, 3)) {
  ## id_task = 0 
  ##  corresponds to a reduced experiment that is useful
  ##  for debugging the code.
  if(id_task == 0) {
    args <- list(
      p               = 20,
      T0_prop         = 0.75,
      n_prop          = 0.75,
      r               = c(5),
      diagonal_shift  = c(5),

      ph              = c(0.8),
      pnh             = c(0.05),
      pneff           = c(0.01),
      nsim            = ifelse(runtype == 1, 1, 2),
      M               = 100,

      hmin          = 4,
      hmax          = 6,
      nhmin         = 4,
      nhmax         = 6,
      neffmin       = 4,
      neffmax       = 6,
      shuffle       = FALSE,
      type          = "unif",
      running_days  = 1,
      threshold     = 2)
    args$n <- as.integer(args$p * args$n_prop)
    args$T0 <- as.integer(args$p * args$T0_prop)
    args$id_task <- 0
    
    return(args)
  }
  ## id_task in 1-288
  ##  corresponds to the parameters used for our 
  ##  systematic simulations.

  ## TABLE OF ALL PARAMETER COMBINATIONS.
  sim_par_table <- expand.grid(
    M               = 100,
    hmin          = 4,
    hmax          = 6,
    nhmin         = 4,
    nhmax         = 6,
    neffmin       = 4,
    neffmax       = 6,
    shuffle       = FALSE,
    type          = "unif",
    running_days  = ifelse(runtype <= 2, 1, 5),
    threshold     = 2,
    
    r               = c(5),
    pneff           = c(0.01),
    pnh             = c(0.05),
    ph              = c(0.4, 0.8),
    
    nsim            = ifelse(runtype <= 2, 2, 10),
    diagonal_shift  = c(1,2,5,10),
    n_prop          = c(0.25, 0.5, 0.75, 1),
    T0_prop         = c(0.5, 0.75, 1),
    p               = c(100, 200, 500, 1000))

  ## Function returns row of index id_task.
  args <- sim_par_table[id_task, ]
  args_list <- list()
  for (i in 1:ncol(args)) {
    if (class(args[, i]) == "factor") {
      args_list[[i]] <- as.character(args[1,i])
    } else args_list[[i]] <- args[1,i]
  }
  names(args_list) <- colnames(args)
  
  args_list$id_task <- id_task
  args_list$n <- as.integer(args$p * args$n_prop)
  args_list$T0 <- as.integer(args$p * args$T0_prop)
  
  return(args_list)

}

