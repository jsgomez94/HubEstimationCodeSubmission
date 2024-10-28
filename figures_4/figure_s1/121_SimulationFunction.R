
#####################################################
#####################################################
#####################################################
#####################################################
#####################################################
#####################################################


FullSimulation <- function(args) {
  
  #####################################################
  #####################################################
  
  .pm <- r.sparse.pdhubmat(
    p = args$p, T0 = args$T0, r = args$r,
    ph = args$ph, pnh = args$pnh, pneff = args$pneff,
    diagonal_shift = args$diagonal_shift,
    shuffle = args$shuffle, type = args$type,
    hmin = args$hmin, hmax = args$hmax,
    nhmin = args$nhmin, nhmax = args$nhmax,
    neffmin = args$neffmin, neffmax = args$neffmax)
  
  .sigma <- solve(.pm)
  .ic <- .PMtoIC(.pm)
  .trueHubs <- ((1:args$p) <= args$r)
  
  .start_time_sim <- Sys.time()
  
  #################################################
  #################################################
  ## Generate the integral curve plots:
  {
    
    .n_sizes = c(1.1, 1.5, 2, 4)
    .n_len = length(.n_sizes)
    .plot_label = c("(A)", "(F)", "(B)", "(G)", "(C)", "(H)", "(D)", "(I)", "(E)", "(J)")
    count = 1 
    
    print(paste0("206_figS1.pdf"))
    pdf(
      file = paste0("206_figS1.pdf"),
      width = 13, # The width of the plot in inches
      height = 6) # The height of the plot in inches
    par(oma = c(0, 0, 3, 0))
    layout(
      mat = matrix(c(
        2 * (1:(.n_len + 2)) - 1,
        2 * (1:(.n_len + 2))),
        ncol = .n_len + 2, byrow = TRUE),
      widths = c(2.5, rep(2, .n_len + 1)))
    
    ## Adding legends to plots:
    plot.new()
    plot.new()
    legend(
      x = "top",
      legend = c(
        "Hub I.M.C.",
        "Non-hub I.M.C."),
      col = cbPalette[c(1,2)], lty = c(1,1), cex = 1)
    legend(
      x = "bottom",
      legend = c(
        "I.M., True s", 
        "I.M., Overest. s"),
      col = c(cbPalette[4], "red"), pch = 19, cex = 1)
    
    .pm_red = .pm[1:args$p, 1:args$p]
    .cov_red = solve(.pm_red)
    .cor_red = .COVtoCOR(.cov_red)
    .eigen_cov = eigen(.cov_red)
    .eigen_cor = eigen(.cor_red)
    
    plot(
      1 / .eigen_cor$values[args$p:1],
      xlab = "Eigenvalue index",
      ylab = "Eigenvalue",
      pch = 19,
      main = paste(.plot_label[count], "Population Eigenvalues")
    )
    count = count + 1
    
    .inf_measure_curve <- plot.integrals(
      .eigen_cor, args, 
      main = paste(.plot_label[count], "Population I. M. Curves"))
    abline(v = 5, col = cbPalette[4])
    points(
      x = rep(5, args$p),
      y = .inf_measure_curve[, 5],
      pch = 19,
      col = cbPalette[4])
    points(
      x = rep(args$threshold, args$p),
      y = .inf_measure_curve[, args$threshold],
      pch = 19,
      col = "red")
    count = count + 1
    
    for (.i in 1:.n_len) {
      .n = floor(.n_sizes[.i] * args$p)
      .X <- rmvnorm(
        n = .n,
        sigma = .cov_red,
        method = "svd")
      .cov_emp_red = cov(.X)
      .cor_emp_red = cor(.X)
      
      .eigen_cov_emp = eigen(.cov_emp_red)
      .eigen_cor_emp = eigen(.cor_emp_red)
      
      plot(
        1 / .eigen_cor_emp$values[args$p:1],
        xlab = "Eigenvalue index",
        ylab = "Eigenvalue",
        pch = 19,
        main = paste(.plot_label[count], "Eigenvalues: n =", .n))
      count = count + 1
      .inf_measure_curve <- plot.integrals(
        .eigen_cor_emp, args, 
        main = paste(.plot_label[count], "I. M. Curves: n =", .n))
      abline(v = 5, col = cbPalette[4])
      points(
        x = rep(5, args$p),
        y = .inf_measure_curve[, 5],
        pch = 19,
        col = cbPalette[4])
      points(
        x = rep(args$threshold, args$p),
        y = .inf_measure_curve[, args$threshold],
        pch = 19,
        col = "red")
      count = count + 1
    }
    mtext(
      text = paste0("Influence Measure Curve Plots"),
      side = 3, line = 0, outer = TRUE, cex = 2
    )
    
    dev.off()
    
  }
  
}