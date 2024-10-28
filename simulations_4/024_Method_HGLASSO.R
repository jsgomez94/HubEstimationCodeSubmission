

#################################################
#################################################
###################### HGL ######################
#################################################
#################################################


#################################################
#################################################
## BIChgl.reduced2:
##  Compute the optimal tuning parameter for
##  for HGL. This function is used for pre-tuning
##  of the HGL method. The optimal parameters
##  (rho1, rho2, rho3) calculated with this function
##  are used as the input of the function BIChgl.tuned
##  below.
##
##  INPUTS
##    mat         : pxp sample covariance/correlation.
##    rho1length  : length of 1st tuning par. options.
##    rho2length  : length of 2st tuning par. options.
##    rho3length  : length of 3st tuning par. options.
##    cval        : value of c for hglassoBIC from package
##                    HGL.
##    p           : number of variables.
##    n           : sample size.
##    maxit       : maximum number of iterations.
##    
##  OUTPUT:
##    BIC           : numeric vector of BIC values.
##    optimal.index : vector of minimizer indexes.
##    optimal.rho   : vector of minimizer tuning parameters.
##    optimal.model : object of type HGL, with optimal model.
##    total.time    : total training time.
## 
BIChgl.reduced2 <- function(
  mat, rho1length = 10, rho2length = 10, rho3length = 10,
  cval = 0.5, p, n, maxit = 200){
  
  .maxcov <- max(abs(mat - diag(diag(mat))))
  
  ## TUNING PARAMETERS RANGES
  .rho1seq <- .maxcov * 10^seq(from = -3.5, to = -0.1, length.out = rho1length) 
  .rho2seq <- .maxcov * 10^seq(from = -3.5, to = -0.1, length.out = rho2length) / 2
  .rho3seq <- .maxcov * 10^seq(from = -3.5, to = -0.1, length.out = rho2length) / 2

  .dim <- c(rho1length, rho2length, rho3length)
  .BIC.HGL <- array(
    rep(NA, rho1length * rho2length * rho3length), dim = .dim)
  
  .start_time <- Sys.time()

  ## Training for all choices of tuning parameters.
  for (.i in 1:rho1length) {
    for (.j in 1:rho2length) {
      for (.k in 1:rho3length) {

        .rho1 = .rho1seq[.i]
        .rho2 = .rho2seq[.j]
        .rho3 = .rho3seq[.k]
        
        .OUTPUT.HGL <- hglasso(
          S = mat, lambda1 = .rho1,
          lambda2 = .rho2, lambda3 = .rho3,
          maxiter = maxit, trace = FALSE)

        .BIC.HGL[.i, .j, .k] <- hglassoBIC(
          x = .OUTPUT.HGL, S = mat, c = cval)$BIC
      }
    }
  }
  
  ## Selecting optimal tuning par. minimizing BIC.
  .optimal.BIC <- min(.BIC.HGL)
  .opt.index <- which(.BIC.HGL == .optimal.BIC, arr.ind = TRUE)
  
  .opt.rho1 <- .rho1seq[.opt.index[1]]
  .opt.rho2 <- .rho2seq[.opt.index[2]]
  .opt.rho3 <- .rho3seq[.opt.index[3]]
  
  .opt.rho <- c(.opt.rho1, .opt.rho2, .opt.rho3)
  
  print(paste(
    "The optimal tuning is:",
    .opt.rho[1], ",",
    .opt.rho[2], ",",
    .opt.rho[3]))
  
  .opt.HGL <- hglasso(
    S = mat, lambda1 = .opt.rho[1],
    lambda2 = .opt.rho[2], lambda3 = .opt.rho[3],
    maxiter = maxit, trace = FALSE)
  
  .end_time = Sys.time()
  .total_time_HGL <- .end_time - .start_time
  
  
  .OUTPUT <- list(
    BIC = .BIC.HGL,
    optimal.index = .opt.index,
    optimal.rho = .opt.rho,
    optimal.model = .opt.HGL,
    total.time = .total_time_HGL)
  return(.OUTPUT)
}



#################################################
#################################################
## BIChgl.tuned
##  Given a set of pre-trained optimal values
##  (rho1,rho2,rho3), this function retrains the 
##  HGL by searching for the optimal tuning parameters
##  minimizing over neighborhoods of (rho1,rho2,rho3).
##  This allows to fine-tune the choice of tuning
##  parameters on new data, while reducing computational
##  costs of the method.
##
##  INPUTS
##    mat         : pxp sample covariance/correlation.
##    percent     : numeric in [0,1]. How large 
##    rho1  : length of 2st tuning par. options.
##    rho2  : length of 3st tuning par. options.
##    rho3  : length of 3st tuning par. options.
##    rho1length  : length of 1st tuning par. options.
##    rho2length  : length of 2st tuning par. options.
##    rho3length  : length of 3st tuning par. options.
##    cval        : value of c for hglassoBIC from package
##                    HGL.
##    p           : number of variables.
##    n           : sample size.
##    maxit       : maximum number of iterations.
##    
##  OUTPUT:
##    BIC           : numeric vector of BIC values.
##    optimal.index : vector of minimizer indexes.
##    optimal.rho   : vector of minimizer tuning parameters.
##    optimal.model : object of type HGL, with optimal model.
##    total.time    : total training time.
## 
BIChgl.tuned <- function(
  mat, percent = 0.05,
  rho1, rho2, rho3,
  rho1length = 10, rho2length = 10, rho3length = 10,
  cval = 0.5, p, n, maxit = 200) {
  
  .maxcov <- max(abs(mat - diag(diag(mat))))
  
  ## TUNING PARAMETERS RANGES
  .rho1seq <- 1 + percent * (0:(rho1length - 1) - (rho1length - 1) / 2)
  .rho2seq <- 1 + percent * (0:(rho2length - 1) - (rho2length - 1) / 2)
  .rho3seq <- 1 + percent * (0:(rho3length - 1) - (rho3length - 1) / 2)
  
  .dim <- c(rho1length, rho2length, rho3length)
  .BIC.HGL <- array(
    rep(NA, rho1length * rho2length * rho3length), dim = .dim)
  .start_time <- Sys.time()

  ## Training for all choices of tuning parameters.
  for (.i in 1:rho1length) {
    for (.j in 1:rho2length) {
      for (.k in 1:rho3length) {
        .rho1 <- .rho1seq[.i] * rho1
        .rho2 <- .rho2seq[.j] * rho2
        .rho3 <- .rho3seq[.k] * rho3
        
        .OUTPUT.HGL <- hglasso(
          S = mat, lambda1 = .rho1, lambda2 = .rho2, lambda3 = .rho3,
          maxiter = maxit, trace = FALSE)

        .BIC.HGL[.i, .j, .k] = hglassoBIC(
          x = .OUTPUT.HGL, S = mat, c = cval)$BIC

      }
    }
  }
  
  ## Selecting optimal tuning par. minimizing BIC.
  .optimal.BIC <- min(.BIC.HGL)
  .opt.index <- which(.BIC.HGL == .optimal.BIC, arr.ind = TRUE)
  
  .opt.rho1 <- .rho1seq[.opt.index[1]] * rho1
  .opt.rho2 <- .rho2seq[.opt.index[2]] * rho2
  .opt.rho3 <- .rho3seq[.opt.index[3]] * rho3
  
  .opt.rho <- c(.opt.rho1, .opt.rho2, .opt.rho3)
  print(paste(
    "The optimal tuning is:",
    .opt.rho[1], ",",
    .opt.rho[2], ",",
    .opt.rho[3]))
  
  .opt.HGL <- hglasso(
    S = mat, lambda1 = .opt.rho[1], lambda2 = .opt.rho[2], lambda3 = .opt.rho[3],
    maxiter = maxit, trace = FALSE)
  
  .end_time <- Sys.time()
  .total_time_HGL <- .end_time - .start_time
  
  
  .OUTPUT <- list(
    BIC = .BIC.HGL,
    optimal.index = .opt.index,
    optimal.rho = .opt.rho,
    optimal.model = .opt.HGL,
    total.time = .total_time_HGL)
    
  return(.OUTPUT)
}

