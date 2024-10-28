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
###################### Generating plots: DEGREE
##################################################################
##################################################################
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
    type            <- "all"
    results_dir     <- paste0(subfolder_new, "plots_", type, "/")
    
    ## Load data corresponding to specified parameters:
    for (sim_ind in sim_ind_load) {
      load(paste0(
          subfolder_new, "data_all/",
          "output", sim_ind, ".RData"))
    }

    ## Turn into long dataframe to facilitate plotting
    ## from   var1,var2,var3,...,varP ->  var  im
    ##        im1, im2, im3,..., imP      1    im1
    ##        im1, im2, im3,..., imP      2    im2
    ##                ...                   ...
    ## I also eliminate all exactly zero values of im.
    output_merged   <- distinct(bind_rows(mget(ls(pattern = '^output\\d+')))) %>%
      filter(method %in% method_names) %>%
      filter(measure != "A_ND") %>%
      mutate(method = str_replace_all(method, setNames(method_names_clean, method_names))) %>%
      mutate(method = str_replace_all(method, setNames("HGL","HGLASSO"))) %>%
      mutate(method = str_replace_all(method, setNames("HWGL","HWGLASSO"))) %>%
      mutate(method = factor(method, levels = method_names_clean))
    var_names <- output_merged %>% select(matches("var")) %>% names()
    
    mat <- t(apply(
      output_merged, MARGIN = 1, 
      function(x) {
        nhubs     <- 5
        p_val     <- length(x) - 9
        trueHubs  <- (1:p_val) %in% (1:nhubs)

        vals      <- as.numeric(x[-(1:9)])
        vals_pos  <- vals
        
        tr_mean   <- mean(vals_pos)
        tr_sd     <- sd(vals_pos)

        hubshat   <- vals > tr_mean + 2 * tr_sd # 2.32 * tr_sd

        tp <- sum(hubshat & trueHubs) / (nhubs)
        fp <- sum(hubshat & !trueHubs) / (p_val - nhubs)
        
        return(c(tp, fp))
      }
    ))
    output_merged$tp <- mat[,1]
    output_merged$fp <- mat[,2]

    output_summarised <- output_merged %>%
      select(!starts_with("var")) %>%
      pivot_longer(
        cols          = c("tp", "fp"),
        names_to      = "eval_par",
        values_to     = "eval") %>%
      
      group_by(method, measure, p, ph, T0, n, eval_par) %>%
      summarise(mean = mean(eval), sd = sd(eval))
    
    ## Plot 1: TPR
    file_name <- paste0(
      results_dir, "v", plot_version,
      "_trimmed_deg_p", p_val,"_d", d_shift,"_TPR.pdf")

    pdf(file_name, width = 6.5, height = 4)
    p1 <- output_summarised %>%
            filter(eval_par == "tp") %>%
            mutate(
              T0_name = factor(paste0("T ==", T0), levels= c(paste0("T ==", p_val * c(0.5, 0.75, 1)))),
              ph_name = ifelse(ph == 0.4, "p[h] == 0.4", "p[h] == 0.8"),
              TPR = mean) %>%
          ggplot(aes(x = n, y = TPR)) + 
            geom_line(aes(col = method, linetype = method), linewidth = 0.75) + 
            geom_point(aes(col = method, shape = method)) + 
            geom_hline(yintercept = c(0,1), linetype = 2) +
            ylab("True Positive Rate (TPR)") +
            xlab("Sample Size n") +
            theme(legend.title = element_blank()) + #, legend.position = "bottom") +
            facet_grid(rows = vars(ph_name), cols = vars(T0_name), labeller = label_parsed)
    print(p1)
    dev.off()

    ## Plot 2: FPR
    file_name <- paste0(
      results_dir, "v", plot_version,
      "_trimmed_deg_p", p_val,"_d", d_shift,"_FPR.pdf")

    pdf(file_name, width = 6.5, height = 4)
    
    p2 <- output_summarised %>%
            filter(eval_par == "fp") %>%
            mutate(
              T0_name = factor(paste0("T ==", T0), levels= c(paste0("T ==", p_val * c(0.5, 0.75, 1)))),
              ph_name = ifelse(ph == 0.4, "p[h] == 0.4", "p[h] == 0.8"),
              FPR = mean) %>%
          
          ggplot(aes(x = n, y = FPR)) + 
            geom_line(aes(col = method, linetype = method), linewidth = 1) + 
            geom_point(aes(col = method, shape = method)) + 
            geom_hline(yintercept = c(0,1), linetype = 2) +
            ylab("True Positive Rate (TPR)") +
            xlab("Sample Size n") +
            theme(legend.title = element_blank()) + #, legend.position = "bottom") +
            facet_grid(rows = vars(ph_name), cols = vars(T0_name), labeller = label_parsed)
    print(p2)
    dev.off()

    rm(list = grep("output", ls(), value = TRUE))
    rm(list = grep("args", ls(), value = TRUE))
    rm("p1", "p2")
  }
}


