
#####################################################
#####################################################
#####################################################

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
##     HWGL methods for hub estimation with parameters in
##     args.
##
##  INPUTS
##    args    : object output from function CreateParameters.
##                provides all info for simulation run.
##    index   : numeric from 0-9. 
##    
##  OUTPUT:
##    output_sim : data-frame of simulation outputs for HWGL. 
##    output_bic : data-frame of BIC results for HWGL. 
##
FullSimulation <- function(args, index) {

  #####################################################
  ## Runtype 2, 3:
  ## Load pre-training to environment.
  #####################################################
  
  pretraining_vals <- load_pretraining(args, 1)
  list2env(pretraining_vals, globalenv())

  #####################################################
  ## Runtype 2, 3, 4:
  ## Define output saving object.
  #####################################################
  
  method_names <- c(
    "HWGL.CORR.d",  ## HWGL, Measuring column-wise degrees
    "HWGL.CORR.ad", ## HWGL, Measuring column-wise L2 norms.
    "HWGL.CORR.an") ## HWGL, Measuring column-wise L2 norms without diagonal value.
  n_methods <- length(method_names)

  ## Output of simulation:
  ## Dataframe with 
  output_sims <- data.frame(
    ID      = 1:(args$nsim * n_methods),
    method  = character(args$nsim * n_methods),
    measure = character(args$nsim * n_methods),
    sim     = numeric(args$nsim * n_methods),
    time    = numeric(args$nsim * n_methods))
  
  for (var in 1:args$p) {
    output_sims[[var + 5]]         <- numeric(args$nsim * n_methods)
    colnames(output_sims)[var + 5] <- paste0("var", var)
  }
  attach(output_sims)
  
  #################################################
  #################################################
  ## BIC output setting:
  
  n_bic <- ifelse(args$n_prop < 1, 5, 5)
  
  ind_tuning <- list( ## HWGL tuning parameter ranges.
    10^(seq(from = -2, to = 0, length.out = n_bic)),        ## Tuning parameter range n = 0.25p
    10^(seq(from = -2.33, to = -0.33, length.out = n_bic)), ## Tuning parameter range n = 0.5p
    10^(seq(from = -2.66, to = -0.66, length.out = n_bic)), ## Tuning parameter range n = 0.75p
    10^(seq(from = -3,    to = -1, length.out = n_bic)))    ## Tuning parameter range n = p
    
  tuning_hwgl   <- tp_hwgl_corr * ind_tuning[[4 * args$n_prop]]
  rho_min_vec   <- rep(0, 10)

  output_bic <- data.frame(
    sim = 1:args$nsim)
    
  for (bic_ind in 1:n_bic) {
    output_bic[[bic_ind + 1]]         <- numeric(args$nsim)
    colnames(output_bic)[bic_ind + 1] <- paste0("bic", bic_ind)
  }
  for (bic_ind in 1:n_bic) {
    output_bic[[bic_ind + n_bic + 1]]         <- numeric(args$nsim)
    colnames(output_bic)[bic_ind + n_bic + 1] <- paste0("rho", bic_ind)
  }
  print(output_bic)

  #################################################
  #################################################
  ## Cycle:
  loop_start_time <- Sys.time()
  sim_ind <- 1
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
      
      # Finding PM/IC matrix:
      sigmaHat   <- cov(X)
      rhoHat     <- cor(X)
    
    }
    ############################
    ######## HWGL CORR 13, 14, 15
    ########
    {
      start_time    <- Sys.time()
      output_hwgl   <- BIChwglasso(
        mat = rhoHat, rho = tuning_hwgl,
        p = args$p, n = args$n,
        penalize.diagonal = FALSE)
      output_mat    <- (output_hwgl$optimal.model)$wi
      end_time      <- Sys.time()
      
      ## Time:
      
      ## Degree:
      output_sims[count, 2]      <- "HWGL.CORR.d"
      output_sims[count, 3]      <- "Deg"
      output_sims[count, 4]      <- sim_ind
      output_sims[count, 5]    <- difftime(
          time1 = end_time, time2 = start_time, units = "s") %>%
          as.numeric()
      output_sims[count, -(1:5)] <- .degrees(output_mat)
      count <- count + 1

      ## Alpha Values:
      output_sims[count, 2]      <- "HWGL.CORR.ad"
      output_sims[count, 3]      <- "A_D"
      output_sims[count, 4]      <- sim_ind
      output_sims[count, 5]    <- difftime(
          time1 = end_time, time2 = start_time, units = "s") %>%
          as.numeric()
      output_sims[count, -(1:5)] <- .alphavals(output_mat)
      count <- count + 1

      ## Alpha Values (no-diag):
      output_sims[count, 2]      <- "HWGL.CORR.an"
      output_sims[count, 3]      <- "A_ND"
      output_sims[count, 4]      <- sim_ind
      output_sims[count, 5]    <- difftime(
          time1 = end_time, time2 = start_time, units = "s") %>%
          as.numeric()
      output_sims[count, -(1:5)] <- .alphavals(output_mat - diag(diag(output_mat)))
      count <- count + 1

      output_bic[sim_ind, 2:(n_bic + 1)]                <- output_hwgl$BIC
      output_bic[sim_ind, (n_bic + 2):(2 * n_bic + 1)]  <- tuning_hwgl
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

  output <- list(
    output_sim  = output_sims,
    output_bic  = output_bic)
    
  return(output)

}
