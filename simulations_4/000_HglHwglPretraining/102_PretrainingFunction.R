
#####################################################
#####################################################
#####################################################

pretraining_fun <- function(args) {
  .pm <- r.sparse.pdhubmat(
      p = args$p, T0 = args$T0, r = args$r,
      ph = args$ph, pnh = args$pnh, pneff = args$pneff,
      diagonal_shift = args$diagonal_shift,
      shuffle = args$shuffle,
      type = args$type,
      hmin = args$hmin, hmax = args$hmax,
      nhmin = args$nhmin, nhmax = args$nhmax,
      neffmin = args$neffmin, neffmax = args$neffmax)
    
    .sigma <- solve(.pm)
    .rho <- .COVtoCOR(.sigma)
    .ic <- .PMtoIC(.pm)
    .trueHubs <- ((1:args$p) <= args$r)
    
    ##########################
    ### HWGL-CORR
    .start_time <- Sys.time()
    .tp_hwgl_corr <- tp.hwgl(
      pm = .pm, p = args$p, n = args$n, cov = FALSE)

    .end_time <- Sys.time()
    .pretuning_time_HWGL_corr <- difftime(
      time1 = .end_time, time2 = .start_time, units = "s")
    
    ##########################
    ### HWGL-COV
    .start_time <- Sys.time()
    .tp_hwgl_cov <- tp.hwgl(
      pm = .pm, p = args$p, n = args$n, cov = TRUE)
    
    .end_time <- Sys.time()
    .pretuning_time_HWGL_cov <- difftime(
      time1 = .end_time, time2 = .start_time, units = "s")
    
    ##########################
    ### HGL-CORR
    .start_time <- Sys.time()
  .tp_hgl_corr <- hgl.select(
      pm = .pm, p = args$p, n = args$n, cval = 0.5,
      length.out = args$hgl.length.out, cov = FALSE)

  .end_time <- Sys.time()
  .pretuning_time_HGL_corr = difftime(
    time1 = .end_time, time2 = .start_time, units = "s")
    
  ##########################
  ### HGL-COV
  .start_time <- Sys.time()
  .tp_hgl_cov <- hgl.select(
    pm = .pm, p = args$p, n = args$n, cval = 0.5,
    length.out = args$hgl.length.out, cov = TRUE)

  .end_time <- Sys.time()
  .pretuning_time_HGL_cov = difftime(
    time1 = .end_time, time2 = .start_time, units = "s")
    
  ##########################
  ### Save pretraining scheme.
  .output <- list(
    pretuning_par_HWGL_corr = .tp_hwgl_corr,
    pretuning_par_HWGL_cov = .tp_hwgl_cov,
    pretuning_par_HGL_corr = .tp_hgl_corr,
    pretuning_par_HGL_cov = .tp_hgl_cov,
    pretuning_time_HWGL_corr = .pretuning_time_HWGL_corr,
    pretuning_time_HGL_corr = .pretuning_time_HGL_corr,
    pretuning_time_HWGL_cov = .pretuning_time_HWGL_cov,
    pretuning_time_HGL_cov = .pretuning_time_HGL_cov
  )
  
  return(.output)
}

