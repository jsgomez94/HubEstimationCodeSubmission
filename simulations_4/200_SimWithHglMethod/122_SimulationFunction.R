
#################################################
#################################################
## CreateParameters:
##    Function that, given a set of simulation parameters
##     args, loads the pretraining values for the HGL 
##      method corresponding to these parameters. 
##
##  INPUTS
##    args    : object output from function CreateParameters.
##                provides all info for simulation run.
##    index   : version of simulation. Usually index = 1.
##    
##  OUTPUT:
##    tp_hwgl_corr
##    pretuning_time_HWGL_corr
##    tp_hwgl_cov
##    pretuning_time_HWGL_corr
##    tp_hgl_corr
##    tp_hgl_cov
load_pretraining <- function(args, index) {
  pretraining_subfolder <- paste0(main_folder, "pretrainings", index, "/data/")
  print(paste("ID-Task:", args$id_task))

  pretraining_file <- paste0(
    pretraining_subfolder,
    "output", args$id_task, ".RData")
    
  print(paste0("pretraining file: ", pretraining_file))
  load(pretraining_file)

  output <- list()

  hwgl_corr <- get(paste0("output", args$id_task))
  output$tp_hwgl_corr <- hwgl_corr$pretuning_par_HWGL_corr
  output$pretuning_time_HWGL_corr <- hwgl_corr$pretuning_time_HWGL_corr
    
  hwgl_cov <- get(paste0("output", args$id_task))
  output$tp_hwgl_cov <- hwgl_cov$pretuning_par_HWGL_cov
  output$pretuning_time_HWGL_cov <- hwgl_cov$pretuning_time_HWGL_cov
    
  hgl_corr <- get(paste0("output", args$id_task))
  output$tp_hgl_corr <- hgl_corr$pretuning_par_HGL_corr
  pretuning_time_HGL_corr <- hgl_corr$pretuning_time_HGL_corr

  hgl_cov <- get(paste0("output", args$id_task))
  output$tp_hgl_cov <- hgl_cov$pretuning_par_HGL_cov
  output$pretuning_time_HGL_cov <- hgl_cov$pretuning_time_HGL_cov

  return(output)
}



#################################################
#################################################
## FullSimulation:
##    Function that, given a set of simulation parameters
##     args, performs args$nsim simulation replicates of 
##     HGL methods for hub estimation with parameters in
##     args.
##
##  INPUTS
##    args    : object output from function CreateParameters.
##                provides all info for simulation run.
##    index   : numeric from 0-9. 
##    
##  OUTPUT:
##    output  : data-frame of simulation outputs for HGL. 
##
FullSimulation <- function(args, index) {

  #####################################################
  ## Runtype 2, 3:
  ## Load pre-training to environment.
  #####################################################
  
  pretraining_vals <- load_pretraining(args, index)
  list2env(pretraining_vals, globalenv())

  #####################################################
  ## Runtype 2, 3, 4:
  ## Define output saving object.
  #####################################################
  
  method_names <- c(
    "HGL.CORR.d",   ## HGL, Measuring column-wise degrees
    "HGL.CORR.ad",  ## HGL, Measuring column-wise L2 norms.
    "HGL.CORR.an")  ## HGL, Measuring column-wise L2 norms without diagonal value.
  
  n_methods <- length(method_names)

  ## Output of simulation:
  ## Dataframe with 
  output <- data.frame(
    ID      = 1:(args$nsim * n_methods),
    method  = character(args$nsim * n_methods),
    measure = character(args$nsim * n_methods),
    sim     = numeric(args$nsim * n_methods),
    time    = numeric(args$nsim * n_methods))
  
  for (var in 1:args$p) {
    output[[var + 5]]         <- numeric(args$nsim * n_methods)
    colnames(output)[var + 5] <- paste0("var", var)
  }
  attach(output)
  
  #################################################
  #################################################
  ## Cycle:
  sim_ind <- 1
  loop_start_time <- Sys.time()
  count <- 1

  while (sim_ind < args$nsim + 1) {
    
    ############################
    ######## Generate data:
    ########
    {
      print(paste("Generating PM/IC and data (",
                  round(100 * (sim_ind - 1) / args$nsim, 2),
                  "%", ")"))

      # Generate PM.
      pm <- r.sparse.pdhubmat(
        p = args$p, T0 = args$T0, r = args$r,
        ph = args$ph, pnh = args$pnh, pneff = args$pneff,
        diagonal_shift = args$diagonal_shift,
        shuffle = args$shuffle,
        type = args$type,
        hmin = args$hmin, hmax = args$hmax,
        nhmin = args$nhmin, nhmax = args$nhmax)

      sigma     <- solve(pm)
      rho       <- .COVtoCOR(sigma)
      ic        <- .PMtoIC(pm)
      trueHubs  <- ((1:args$p) <= args$r)

      # Generate data.
      X         <- rmvnorm(
        n = args$n, sigma = sigma, method = "svd")
    }
    ############################
    ######## Finding PM/IC matrix:
    ########
    {
      print(paste("Preparing Inputs (",
                  round(100 * (sim_ind - 1) / args$nsim, 2),
                  "%", ")"))

      sigmaHat   <- cov(X)
      rhoHat     <- cor(X)
    }
    ############################
    ######## HGL CORR 7, 8, 9
    ########
    {
      start_time <- Sys.time()
      output_hgl <- BIChgl.tuned(
        mat = rhoHat,
        percent = 0.05,
        rho1 = tp_hgl_corr$rho1,
        rho2 = tp_hgl_corr$rho2,
        rho3 = tp_hgl_corr$rho3,
        rho1length = 3,
        rho2length = 3,
        rho3length = 3,
        cval = 0.5, p = args$p, n = args$n)$optimal.model
      output_mat <- output_hgl$Theta
      end_time <- Sys.time()
      
      ## Degree:
      output[count, 2]      <- "HGL.CORR.d"
      output[count, 3]      <- "Deg"
      output[count, 4]      <- sim_ind
      output[count, 5]    <- difftime(
          time1 = end_time, time2 = start_time, units = "s") %>%
          as.numeric()
      output[count, -(1:5)] <- .degrees(output_mat)
      count <- count + 1

      ## Alpha Values:
      output[count, 2]      <- "HGL.CORR.ad"
      output[count, 3]      <- "A_D"
      output[count, 4]      <- sim_ind
      output[count, 5]    <- difftime(
          time1 = end_time, time2 = start_time, units = "s") %>%
          as.numeric()
      output[count, -(1:5)] <- .alphavals(output_mat)
      count <- count + 1

      ## Alpha Values (no-diag):
      output[count, 2]      <- "HGL.CORR.an"
      output[count, 3]      <- "A_ND"
      output[count, 4]      <- sim_ind
      output[count, 5]    <- difftime(
          time1 = end_time, time2 = start_time, units = "s") %>%
          as.numeric()
      output[count, -(1:5)] <- .alphavals(output_mat - diag(diag(output_mat)))
      count <- count + 1

    }
    #####################################################
    #####################################################
    ## Step 12: Add stopping condition. 
    {
      print(paste0("Step ", sim_ind,": Time Analysis."))
      
      ## If it will take more than X days to run,
      ## save results and leave.
      time_stamp <- Sys.time()
      current.rt.hour   <- 
        difftime(time_stamp, loop_start_time, units = "hours") %>%
        as.numeric()
      current.rt.days   <- 
        difftime(time_stamp, loop_start_time, units = "days") %>%
        as.numeric()
      mean.rt.days      <- current.rt.days / sim_ind
      expected.rt.days  <- current.rt.days + 1.5 * mean.rt.days
      ncompleted        <- sim_ind
      
      if (expected.rt.days >= args$running_days) { ## days.
        print(paste("---> Expected running time (+1):", 
                    round(expected.rt.days, digits = 4),
                    "days."))
        print("---> Stopping process...")
        sim_ind = args$nsim + 1
      }
      sim_ind <- sim_ind + 1
    }

  }

  return(output)

}
