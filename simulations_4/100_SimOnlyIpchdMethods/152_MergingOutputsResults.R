############################################
############################################
## SETTING PARAMETERS:
.libPaths("/nas/longleaf/home/jsgomez/github/HubEstimation/SimulationsDimRed_PaperRevision5/req_lib")
library(readr)
library(plot.matrix)
library(ggplot2)
library(tidyr)
library(tibble)
library(dplyr)
library(stringr)
library(lazyeval)




###################### Merging Results for p = 100:
## We have results split in 4 files.
runtype <- 3
index   <- 1
for (sim_ind in 1:288) {
  
  ## Create directory where results will be saved:
  runtype_name    <- c("pretraining", "experiments", "outputs")[runtype]
  results_dir     <- paste0(runtype_name, index, "/data/")
  
  load(paste0(results_dir, "output", sim_ind, "_0.RData"))
  output_0  <- get(paste0("output", sim_ind, "_0"))
  args_0    <- get(paste0("args", sim_ind))
  nsim      <- args_0$nsim
  for (microrun_ind in 1:9) {
    load(paste0(results_dir, "output", sim_ind, "_", microrun_ind,".RData"))
    output_0 <- get(paste0("output", sim_ind, "_", microrun_ind)) %>%
      mutate(sim = sim + nsim * microrun_ind) %>%
      {bind_rows(output_0, .)}
    
      output_0 %>% 
      bind_rows(get(paste0("output", sim_ind, "_", microrun_ind)))
    print(dim(output_0))
  }
  
  paste0("Result:", dim(output_0))
  tmp_env_sim <- new.env()
  assign(
    paste0("output", sim_ind),
    output_0,
    pos = tmp_env_sim)
assign(
  paste0("args", sim_ind),
  args_0,
  pos = tmp_env_sim)

  print(paste0(results_dir, "output", sim_ind, ".RData"))
  save(
    list = ls(all.names = TRUE, pos = tmp_env_sim), envir = tmp_env_sim,
    file = paste0(results_dir, "output", sim_ind, ".RData"))

  rm(tmp_env_sim, output_0)
}



rm(list = ls())
load(paste0("outputs1/data/output5.RData"))
dim(output5)
output5 %>% select(-starts_with("var"))
rm(list = ls())

