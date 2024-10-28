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

plot_version <- "2.2T"

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
subfolder_new        <- paste0("500_AggregatedDataExperiments/")
subfolder_data_new   <- paste0(subfolder_new, "data_all/")
subfolder_plots_new  <- paste0(subfolder_new, "plots_all/")
subfolder_time_new  <- paste0(subfolder_new, "time_all/")

if (!dir.exists(subfolder_new)) {
       dir.create(subfolder_new)
}
if (!dir.exists(subfolder_data_new)) {
       dir.create(subfolder_data_new)
}
if (!dir.exists(subfolder_plots_new)) {
       dir.create(subfolder_plots_new)
}
if (!dir.exists(subfolder_time_new)) {
       dir.create(subfolder_time_new)
}

##################################################################
##################################################################
###################### Generating plots: LOG TIME
##################################################################
##################################################################
## Choosing data to load
## Choosing data to load
method_names <- c(
    "CORRad",             ## Direct inversion methods.
    "GL.CORR.d",          ## GLASSO-methods
    "HGL.CORR.d",         ## HGL-methods.
    "HWGL.CORR.d",        ## HWGL-methdos.
    "COR_ThrP_IPCHD", "COR_Scr_IPCHD")  ## IPC-HD
method_names_clean <- c(
    "Raw Inv.",    ## Direct inversion methods.
    "GLASSO",      ## GLASSO-methods
    "HGL",         ## HGL-methods.
    "HWGL",        ## HWGL-methdos.
    "Thr. IPC-HD", "Scr. IPC-HD")  ## IPC-HD


for(d_shift in c(1, 2, 5, 10)) {
  for(p_val in c(100, 200, 500)) {
    ## Set simulation parameters:
    sim_ind_load    <- which(diagonal_shift == d_shift & p == p_val)
    results_dir     <- subfolder_time_new
    
    ## Load data corresponding to specified parameters:
    for (sim_ind in sim_ind_load) {
      load(paste0(
          subfolder_data_new,
          "output", sim_ind, ".RData"))
    }

    ## Turn into long dataframe to facilitate plotting
    ## from   var1,var2,var3,...,varP ->  var  im
    ##        im1, im2, im3,..., imP      1    im1
    ##        im1, im2, im3,..., imP      2    im2
    ##                ...                   ...
    ## I also eliminate all exactly zero values of im.
    output_merged   <- distinct(bind_rows(mget(ls(pattern = '^output\\d+')))) %>%
      select(!starts_with("var")) %>%
      filter(measure != "A_ND", method %in% method_names) %>%
      mutate(method = str_replace_all(method, setNames(method_names_clean, method_names))) %>%
      mutate(method = str_replace_all(method, setNames("HGL","HGLASSO"))) %>%
      mutate(method = str_replace_all(method, setNames("HWGL","HWGLASSO"))) %>%
      mutate(method = factor(method, levels = method_names_clean)) %>%
      group_by(method, measure, p, ph, T0, n) %>%
      summarise(mean = mean(time), sd = sd(time))
    
    ## Plot 1: ph = 0.4
    file_name <- paste0(
      results_dir, "v", plot_version,
      "_logtime_p", p_val,"_d", d_shift,".pdf")
    pdf(file_name, width = 6.5, height = 4)
    p1 <- output_merged %>%

      
      mutate(
        T0_name = factor(paste0("T ==", T0), levels= c(paste0("T ==", p_val * c(0.5, 0.75, 1)))),
        ph_name = ifelse(ph == 0.4, "p[h] == 0.4", "p[h] == 0.8"),
        FPR = mean) %>%

      ggplot(aes(x = n, y = log(mean, base = 60))) + 
        geom_line(aes(col = method, linetype = method), linewidth = 1) + 
        geom_point(aes(col = method, shape = method)) + 
        geom_hline(yintercept = c(0,1, 1.56), linetype = 2, linewidth = 0.25) +
        ylab(expression(log[60]("seconds"))) +
        xlab("Sample Size n") +
        annotate("text", x = p_val * 0.93, y = 0.15, label = "1 s", size = 2.5) + 
        annotate("text", x = p_val * 0.93, y = 1.15, label = "1 m", size = 2.5) + 
        annotate("text", x = p_val * 0.93, y = 1.71, label = "10 m", size = 2.5) + 
        # annotate("text", x = p_val * 0.93, y = 2.15, label = "1 h") + 
        theme(legend.title = element_blank()) +
        facet_grid(rows = vars(ph_name), cols = vars(T0_name), labeller = label_parsed)
    print(p1)
    dev.off()
  
    rm(list = grep("output", ls(), value = TRUE))
    rm(list = grep("args", ls(), value = TRUE))
    rm("p1")
  }
}




##################################################################
##################################################################
###################### Generating plots: LOG TIME
##################################################################
##################################################################
## Choosing data to load
method_names <- c(
    "GL.CORR.d",          ## GLASSO-methods
    "HGL.CORR.d",         ## HGL-methods.
    "HWGL.CORR.d",        ## HWGL-methdos.
    "CORRad",             ## Direct inversion methods.
    "COR_ThrP_IPCHD", "COR_Scr_IPCHD")  ## IPC-HD

method_names_clean <- c(
    "GLASSO",          ## GLASSO-methods
    "HGL",         ## HGL-methods.
    "HWGL",        ## HWGL-methdos.
    "Raw Inv",     ## Direct inversion methods.
    "Thr. IPC-HD", "Scr. IPC-HD")  ## IPC-HD

for(d_shift in c(1, 2, 5, 10)) {
  for(p_val in c(100, 200, 500)) {
    ## Set simulation parameters:
    sim_ind_load    <- which(diagonal_shift == d_shift & p == p_val)
    results_dir     <- subfolder_time_new
    
    ## Load data corresponding to specified parameters:
    for (sim_ind in sim_ind_load) {
      load(paste0(
          subfolder_data_new,
          "output", sim_ind, ".RData"))
    }

    ## Turn into long dataframe to facilitate plotting
    ## from   var1,var2,var3,...,varP ->  var  im
    ##        im1, im2, im3,..., imP      1    im1
    ##        im1, im2, im3,..., imP      2    im2
    ##                ...                   ...
    ## I also eliminate all exactly zero values of im.
    output_merged   <- distinct(bind_rows(mget(ls(pattern = '^output\\d+')))) %>%
      select(!starts_with("var")) %>%
      filter(measure != "A_ND", method %in% method_names) %>%
      group_by(method, measure, p, ph, T0, n) %>%
      summarise(mean = mean(time), sd = sd(time))
    
    ## Plot 1: ph = 0.4
    file_name <- paste0(
      results_dir, "v", plot_version, 
      "_time_p", p_val,"_d", d_shift,".pdf")

    pdf(file_name, width = 7.5, height = 5)
    p1 <- ggplot(output_merged, aes(x = n, y = mean / 60)) + 
      geom_line(aes(col = method)) + 
      geom_ribbon(
        aes(
          ymin = (mean - sd) / 60, ymax = (mean + sd) / 60, fill = method),
        alpha = 0.1) +
      facet_grid(rows = vars(ph), cols = vars(T0))
    print(p1)
    dev.off()
  
    rm(list = grep("output", ls(), value = TRUE))
    rm(list = grep("args", ls(), value = TRUE))
    rm("p1", "p2")
  }
}




