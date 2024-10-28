#################################################
#################################################
## FullSimulation:
##    Function that, given a set of simulation parameters
##     args, performs args$nsim simulation replicates of 
##     GLASSO methods for hub estimation with parameters in
##     args.
##
##  INPUTS
##    args    : object output from function CreateParameters.
##                provides all info for simulation run.
##    index   : numeric from 0-9. 
##    
##  OUTPUT:
##    output_sim : data-frame of simulation outputs for GLASSO. 
##    output_bic : data-frame of BIC results for GLASSO. 
##
FullSimulation <- function(args, index) {

  #####################################################
  ## Runtype 2, 3:
  ## Define output saving object.
  #####################################################
  
  method_names <- c(
    "GL.CORR.d",  ## GLASSO, Measuring column-wise degrees
    "GL.CORR.ad", ## GLASSO, Measuring column-wise L2 norms.
    "GL.CORR.an") ## GLASSO, Measuring column-wise L2 norms without diagonal value.
  n_methods <- length(method_names)

  ## Output of simulation:
  ## Dataframe with 
  output_sims <- data.frame(
    ID      = 1:(args$nsim * n_methods),
    method  = character(args$nsim * n_methods),
    measure = rep("inf_meas", args$nsim * n_methods),
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
  
  n_rho <- ifelse(args$n_prop < 1, 20, 20)
  
  ind_tuning <- list( 
    10^(seq(from = -2, to = 1, length.out = n_rho)), ## Tuning parameter range n = 0.25p
    10^(seq(from = -2, to = 1, length.out = n_rho)), ## Tuning parameter range n = 0.5p
    10^(seq(from = -2, to = 1, length.out = n_rho)), ## Tuning parameter range n = 0.75p
    10^(seq(from = -2, to = 1, length.out = n_rho))) ## Tuning parameter range n = p
  
  output_bic <- data.frame(
    sim = 1:args$nsim)
    
  for (bic_ind in 1:n_rho) {
    output_bic[[bic_ind + 1]]         <- numeric(args$nsim)
    colnames(output_bic)[bic_ind + 1] <- paste0("bic", bic_ind)
  }
  for (bic_ind in 1:n_rho) {
    output_bic[[bic_ind + n_rho + 1]]         <- numeric(args$nsim)
    colnames(output_bic)[bic_ind + n_rho + 1] <- paste0("rho", bic_ind)
  }
  print(output_bic)
  
  #################################################
  #################################################
  ## Cycle:
  loop_start_time <- Sys.time()
  sim_ind   <- 1
  count     <- 1
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

      start_time <- Sys.time()
      tuning_gl  <- ind_tuning[[4 * args$n_prop]]
      output_gl  <- BICglasso(
        mat = rhoHat, rho = tuning_gl,
        p = args$p, n = args$n,
        penalize.diagonal = FALSE)
      output_mat <- (output_gl$optimal.model)$wi
      end_time <- Sys.time()

      ## Time:
      
      ## Degree:
      output_sims[count, 2]      <- "GL.CORR.d"
      output_sims[count, 3]      <- "Deg"
      output_sims[count, 4]      <- sim_ind
      output_sims[count, 5]    <- difftime(
          time1 = end_time, time2 = start_time, units = "s") %>%
          as.numeric()
      output_sims[count, -(1:5)] <- .degrees(output_mat)
      count <- count + 1

      ## Alpha Values (diag):
      output_sims[count, 2]      <- "GL.CORR.ad"
      output_sims[count, 3]      <- "A_D"
      output_sims[count, 4]      <- sim_ind
      output_sims[count, 5]    <- difftime(
          time1 = end_time, time2 = start_time, units = "s") %>%
          as.numeric()
      output_sims[count, -(1:5)] <- .alphavals(output_mat)
      count <- count + 1
      
      ## Alpha Values (no-diag):
      output_sims[count, 2]      <- "GL.CORR.an"
      output_sims[count, 3]      <- "A_ND"
      output_sims[count, 4]      <- sim_ind
      output_sims[count, 5]    <- difftime(
          time1 = end_time, time2 = start_time, units = "s") %>%
          as.numeric()
      output_sims[count, -(1:5)] <- .alphavals(output_mat - diag(diag(output_mat)))
      count <- count + 1

      output_bic[sim_ind, 2:(n_rho + 1)]                <- output_gl$BIC
      output_bic[sim_ind, (n_rho + 2):(2 * n_rho + 1)]  <- tuning_gl

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
    output_sim = output_sims,
    output_bic = output_bic)
  print(output_bic)

  return(output)

}
