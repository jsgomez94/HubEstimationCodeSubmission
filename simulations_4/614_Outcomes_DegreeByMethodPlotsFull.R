############################################
############################################
## SETTING PARAMETERS:
wd <- getwd()
.libPaths(paste0(wd,"/req_lib"))
library(readr)
library(ggplot2)
library(tidyr)
library(tibble)
library(dplyr)
library(stringr)
library(lazyeval)
library(ggh4x)
library(gridExtra)
library(ggpubr)
plot_version <- "3.1T"

source("004_ModulePlotting.R")


###################### Parameter table:
runtype <- 2 # for threshold simulations
index   <- 1 # run index to use
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
subfolder_new        <- "600_AggregatedDataFull/"
subfolder_plots_new  <- paste0(subfolder_new, "bymethod_all/")

if (!dir.exists(subfolder_new)) {
  dir.create(subfolder_new)
}
if (!dir.exists(subfolder_plots_new)) {
  dir.create(subfolder_plots_new)
}


##################################################################
##################################################################
###################### Generating plots: NO LINE
##################################################################
##################################################################

for(p_val in c(100, 200, 500)) {
    
  ## Set simulation parameters:
  d_shift         <- 5
  sim_ind_load    <- which(diagonal_shift == d_shift & p == p_val)
  type            <- "bymethod"
  results_dir     <- paste0(subfolder_new, "plots_", type, "/")
  
  ## Load data corresponding to specified parameters:
  for (sim_ind in sim_ind_load) {
    load(paste0(
      subfolder_new, "data_all/",
      "output", sim_ind, ".RData"))
  }
  method_names <- c(
    "CORRad",             ## Direct inversion methods.
    "GL.CORR.d",          ## GLASSO-methods
    "HGL.CORR.d",         ## HGL-methods.
    "HWGL.CORR.d",        ## HWGL-methdos.
    "COR_ThrP_IPCHD", "COR_Scr_IPCHD")  ## IPC-HD
  ## Choosing data to load
  method_names_clean <- c(
    "Raw-Inv.",    ## Direct inversion methods.
    "GLASSO",      ## GLASSO-methods
    "HGL",         ## HGL-methods.
    "HWGL",        ## HWGL-methdos.
    "Thr.-IPCHD", "Scr.-IPCHD")  ## IPC-HD
  
  ## Merge all data into a single data-frame.
  output_merged   <- distinct(bind_rows(mget(ls(pattern = '^output\\d+')))) %>%
    filter(method %in% method_names) %>%
    filter(measure != "A_ND") %>%
    mutate(method = str_replace_all(method, setNames(method_names_clean, method_names))) %>%
    mutate(method = str_replace_all(method, setNames("HGL","HGLASSO"))) %>%
    mutate(method = str_replace_all(method, setNames("HWGL","HWGLASSO"))) %>%
    mutate(method = factor(method, levels = method_names_clean))
  table(output_merged$method)
  
  ## Data cleaning:
  output_summarized <- output_merged %>% 
    pivot_longer(
      cols = starts_with("var"),
      names_to = "var_ind",
      names_prefix = "var",
      values_to = "var_deg") %>%
    group_by(p, ph, T0, n, var_ind, method) %>%
    summarize(mean = mean(var_deg), sd = sd(var_deg)) %>%
    mutate(
      var_ind = as.numeric(var_ind),
      hub = (var_ind < 6),
      GLASSO_Method = ifelse(method %in% method_names_clean[2:4], "GLASSO-Based", "Non-GL-Based"))
  
  
  ## Generating plots filtering by method:
  plist <- list()
  ylab    <- c("Mean Weighted Degree", "Mean Degree", "Mean Degree", "Mean Degree", "Mean Inf. Measure", "Mean Inf. Measure")
  leg_pos <- c("none", "none", "none", "bottom","none", "bottom")
  for (i in 1:6) {
    
    plist[[i]] <- output_summarized %>%
      filter(method == method_names_clean[i]) %>%
      mutate(
        T0_name = factor(paste0("T ==", T0), levels= c(paste0("T ==", p_val * c(0.5, 0.75, 1)))),
        ph_name = ifelse(ph == 0.4, "p[h] == 0.4", "p[h] == 0.8"),
        Truth = factor(
          ifelse(hub, "Hub Variable", "Non-hub Variable"),
          levels = c("Non-hub Variable", "Hub Variable"))) %>%
      ggplot(aes(x = n, y = mean, group = var_ind)) +
      geom_line(aes(col = Truth, group = var_ind), linewidth = 0.75) + 
      geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = Truth, alpha = Truth)) +
      scale_alpha_discrete(range = c(0.1 / sqrt(p_val), 0.07)) +
      ylab(ylab[i]) +
      theme(
        legend.title = element_blank(),
        legend.position = leg_pos[i],
        plot.margin = margin(t = 20, r = 5)) +
      facet_grid(ph_name ~ T0_name, labeller = label_parsed, scales = 'free_y')
  }
  
  ## Saving plots in PDF format.
  file_name <- paste0(
    results_dir, p_val,"_d", d_shift,"_DegByMethod_Exp.pdf")
  pdf(file_name, width = 8, height = 11)
  
  print(ggarrange(
    plist[[2]], plist[[1]], 
    plist[[3]], plist[[5]],
    plist[[4]], plist[[6]],
    labels = c("GLASSO", "Raw-Inv.", "HGL", "Thr. IPC-HD", "HWGL", "Scr. IPC-HD"),
    heights = c(1,1,1.2),
    ncol = 2, nrow = 3))
  dev.off()
  
  
  ## Saving plots in jpeg format.
  file_name <- paste0(
    results_dir, p_val,"_d", d_shift,"_DegByMethod_Exp.jpg")
  jpeg(file_name, width = 7.5, height = 11, units = "in", res = 288)
  
  print(ggarrange(
    plist[[2]], plist[[1]], 
    plist[[3]], plist[[5]],
    plist[[4]], plist[[6]],
    labels = c("GLASSO", "Raw-Inv.", "HGL", "Thr. IPC-HD", "HWGL", "Scr. IPC-HD"),
    heights = c(1,1,1.2),
    ncol = 2, nrow = 3))
  dev.off()
  
  rm(list = grep("output", ls(), value = TRUE))
  rm(list = grep("args", ls(), value = TRUE))
  rm("plist")
}

