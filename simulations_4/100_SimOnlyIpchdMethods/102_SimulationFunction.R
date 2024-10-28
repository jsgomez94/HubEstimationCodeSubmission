

#####################################################
#####################################################
#####################################################


#################################################
#################################################
## FullSimulation:
##    Function that, given a set of simulation parameters
##     args, performs args$nsim simulation replicates of 
##     IPC-HD methods for hub estimation with parameters in
##     args.
##
##  INPUTS
##    args    : object output from function CreateParameters.
##                provides all info for simulation run.
##    
##  OUTPUT:
##    output  : data-frame of simulation outputs. 
##
FullSimulation <- function(args) {

  main_folder <- "100_SimOnlyIpchdMethods/"


  #####################################################
  ## Setup output-saving objects
  #####################################################
  
  ## What methods do you want to run?
  method_names <- c(
    "COR_Scr_IPCHD",              ## IPCHD with screening
    "COR_ThrP_IPCHD",             ## IPCHD with thresholding
    "ParCor", "CORRad", "CORRan") ## Direct inversion methods.
  n_methods <- length(method_names)

  ## Output of simulation:
  ## Dataframe with 
  output <- data.frame(
    ID      = 1:(args$nsim * n_methods),
    method  = character(args$nsim * n_methods),
    measure = rep("inf_meas", args$nsim * n_methods),
    sim     = numeric(args$nsim * n_methods),
    time    = numeric(args$nsim * n_methods))
  
  for (var in 1:args$p) {
    output[[var + 5]] <- numeric(args$nsim * n_methods)
    colnames(output)[var + 5] <- paste0("var", var)
  }
  attach(output)

  #################################################
  #################################################
  ## Cycle:
  sim_ind           <- 1
  loop_start_time   <- Sys.time()
  count             <- 1

  while (sim_ind < args$nsim + 1) {
    
    ############################
    ## Generate data:
    {
      print(paste("Generating PM/IC and data (",
                  round(100 * (sim_ind - 1) / args$nsim, 2),
                  "%", ")"))
      
      # Generate PM.
      pm        <- r.sparse.pdhubmat(
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
      
      # Generate data.
      X         <- rmvnorm(n = args$n, sigma = sigma, method = "svd")
    }
    ############################
    ######## Preparing inputs:
    {
      print(paste("Preparing Inputs (",
                  round(100 * (sim_ind - 1) / args$nsim, 2),
                  "%", ")"))

      rhoHat      <- cor(X)

      input_cor   <- list(
        X = X, mat_type = "cor",
        mat = rhoHat, var_inds = 1:(args$p))
    }
    ############################
    ######## Scr + IPCHD
    {
      ## COR
      start_time                  <- Sys.time()
      result_cor      <- input_cor %>%
        c(list(true_mat = NULL, method = "max")) %>%
        do.call(sta_screened_mat, .) %>%
        c(list(overest_type = "frac")) %>%
        do.call(sta_ipchd, .)
      end_time                    <- Sys.time()

      ## Saving things!
      output[count, 2]      <- "COR_Scr_IPCHD"
      output[count, 3]      <- "inf_meas"
      output[count, 4]      <- sim_ind
      output[count, 5]      <- difftime(
          time1 = end_time, time2 = start_time, units = "s") %>%
          as.numeric()
      output[count, -(1:5)] <- result_cor
      
      count <- count + 1
    }
    ############################
    ######## ThrP + IPCHD
    {
      ## COR
      start_time                  <- Sys.time()
      result_cor                  <- input_cor %>%
        c(list(true_mat = NULL, perc = 0.7)) %>%
        do.call(sta_thresholding_perc, .) %>%
        c(list(overest_type = "frac")) %>%
        do.call(sta_ipchd, .)
      end_time                    <- Sys.time()
      
      ## Saving things!
      output[count, 2]      <- "COR_ThrP_IPCHD"
      output[count, 3]      <- "inf_meas"
      output[count, 4]      <- sim_ind
      output[count, 5]      <- difftime(
          time1 = end_time, time2 = start_time, units = "s") %>%
          as.numeric()
      output[count, -(1:5)] <- result_cor
      
      count <- count + 1
    }
    ############################
    ######## partial correlation thresholding 19
    ########
    {
      start_time <- Sys.time()
      ichat <- ginv(rhoHat)
      pchat <- .COVtoCOR(ichat)

      maxrow <- apply(pchat - diag(diag(pchat)),
                      MARGIN = 1,
                      function(x) max(abs(x)))
      sort.maxrow <- sort(maxrow, decreasing = TRUE)
      threshold <- mean(sort.maxrow[args$r:(args$r + 1)])
      output_mat <- pchat * (abs(pchat) > threshold)
      end_time <- Sys.time()

      ## Saving things!
      output[count, 2]      <- "ParCor"
      output[count, 3]      <- "Deg"
      output[count, 4]      <- sim_ind
      output[count, 5]      <- difftime(
          time1 = end_time, time2 = start_time, units = "s") %>%
          as.numeric()
      output[count, -(1:5)] <- .degrees(output_mat)
      
      count <- count + 1
    }
    ############################
    ######## Raw CORR inversion: 20
    ########
    {
      print(paste("Raw Corr inversion (",
                  round(100 * (sim_ind - 1) / args$nsim, 2),
                  "%", ")"))

      ## Raw alpha method:
      start_time  <- Sys.time()
      output_mat  <- ginv(rhoHat)
      end_time    <- Sys.time()

      ## Saving things!
      output[count, 2]      <- "CORRad"
      output[count, 3]      <- "A_D"
      output[count, 4]      <- sim_ind
      output[count, 5]      <- difftime(
          time1 = end_time, time2 = start_time, units = "s") %>%
          as.numeric()
      output[count, -(1:5)] <- .alphavals(output_mat)
      
      count <- count + 1

      ## Saving things!
      output[count, 2]      <- "CORRan"
      output[count, 3]      <- "A_ND"
      output[count, 4]      <- sim_ind
      output[count, 5]      <- difftime(
          time1 = end_time, time2 = start_time, units = "s") %>%
          as.numeric()
      output[count, -(1:5)] <- .alphavals(output_mat - diag(diag(output_mat)))
      
      count <- count + 1

    }
    ############################
    ######## Time analysis:
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

  #################################################
  ## Return output:

  return(output)

}
