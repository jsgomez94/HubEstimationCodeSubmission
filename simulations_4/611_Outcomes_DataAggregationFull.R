
############################################
############################################
## SETTING PARAMETERS:
wd <- getwd()
.libPaths(paste0(wd,"/req_lib"))
library(readr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(tibble)
library(dplyr)
library(stringr)
library(lazyeval)


###################### Parameter table:
runtype       <- 3 # FOR FULL RUNS
index_old     <- 1 # run index to use
sim_par_table <- expand.grid(
    M             = 100,
    hmin          = 4,
    hmax          = 6,
    nhmin         = 4,
    nhmax         = 6,
    neffmin       = 4,
    neffmax       = 6,
    shuffle       = FALSE,
    type          = "unif",
    running_days  = ifelse(runtype == 1, 0.5, 2),
    threshold     = 2,
    
    r               = c(5),
    pneff           = c(0.01),
    pnh             = c(0.05),
    ph              = c(0.4, 0.8),
    
    nsim            = ifelse(runtype == 1, 25, 100),
    diagonal_shift  = c(1,2,5,10),
    n_prop          = c(0.25, 0.5, 0.75, 1),
    T0_prop         = c(0.5, 0.75, 1),
    p               = c(100, 200, 500, 1000))
attach(sim_par_table)


###################### Creating folders:
subfolder_new        <- paste0("600_AggregatedDataFull/")
subfolder_data_new   <- paste0(subfolder_new, "data_all/")
subfolder_plots_new  <- paste0(subfolder_new, "plots_all/")

if (!dir.exists(subfolder_new)) {
       dir.create(subfolder_new)
}
if (!dir.exists(subfolder_data_new)) {
       dir.create(subfolder_data_new)
}
if (!dir.exists(subfolder_plots_new)) {
       dir.create(subfolder_plots_new)
}

##################################################################
##################################################################
## LOAD + MERGE RESULTS:
##################################################################
##################################################################
# Which simulations are we using?
##  RUNTYPE = 2: experiment partial runs.
##  RUNTYPE = 3: full simulation runs. 
run_info <- list(
  list(
    main_dir       = "100_SimOnlyIpchdMethods/",
    run_index      = 1,
    runtype        = runtype,
    abrev_name     = "ipc"),

  list(
    main_dir       = "200_SimWithHglMethod/",
    run_index      = 1,
    runtype        = runtype,
    abrev_name     = "hgl"),
    
  list(
    main_dir       = "300_SimExploringHwglSparsity/",
    run_index      = 1,
    runtype        = runtype,
    abrev_name     = "hwgl"),

  list(
    main_dir       = "400_SimExploringGlSparsity/",
    run_index      = 1,
    runtype        = runtype,
    abrev_name     = "gl"))

###########################
## LOOP OVER ALL 288 SIMULATION PARAMETER COMBINATIONS
for (id_task in 1:288) {
  print(paste("XXXXXXXXXXXXXXXX", id_task))

  output <- NULL
  for(id_microrun in 0:9) {
    ###########################
    ## Merge all data, from all methods into output.
    for (method_ind in c(1,2,3,4)) {
      main_dir     <- run_info[[method_ind]]$main_dir
      run_index    <- run_info[[method_ind]]$run_index
      runtype      <- run_info[[method_ind]]$runtype
      runtype_name <- c("pretrainings","experiments","outputs")[runtype]
      abrev_name   <- run_info[[method_ind]]$abrev_name

      load(paste0(
        main_dir, runtype_name, run_index,
        "/data/output", id_task, "_", id_microrun, ".RData"))

      ## Add prefix:
      assign(
        paste0("output_", abrev_name, "_", id_task),
        get(paste0("output", id_task, "_", id_microrun)))
      rm(list = paste0("output", id_task, "_", id_microrun))

      ## Visualize current data:
      print(paste(
        abrev_name,
        nrow(get(paste0("output_", abrev_name, "_", id_task))),
        ncol(get(paste0("output_", abrev_name, "_", id_task)))))

      ## Add data to output:
      output <- rbind(
        output,
        get(paste0("output_", abrev_name, "_", id_task)))
      rm(list = paste0("output_", abrev_name, "_", id_task))
    }
  }
  

  ###########################
  ## Saving data:
  args   <- get(paste0("args", id_task))
  output <- output %>% 
    mutate(
      p = args$p, ph = args$ph, 
      T0 = args$T0, n = args$n, .before = method)
  print(paste(
      "TOTAL",
      nrow(output),
      ncol(output)))

  tmp.env <- new.env()
  assign(
    paste0("output", id_task),
    output,
    pos = tmp.env)
  assign(
    paste0("args", id_task),
    args,
    pos = tmp.env)
  save(
    list = ls(all.names = TRUE, pos = tmp.env), envir = tmp.env, 
    file = paste0(subfolder_data_new, "output", id_task, ".RData"))
  
  rm(output)
  rm(args)
  rm(list = paste0("args", id_task))
}

ls()
rm(list = ls())
