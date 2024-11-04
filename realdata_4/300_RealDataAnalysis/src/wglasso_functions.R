if (!require(e1071)) {
  install.packages("e1071")
  library(e1071)
}
if (!require(nortest)) {
  install.packages("nortest")
  library(nortest)
}
if (!require(plot.matrix)) {
  install.packages("plot.matrix")
  library(plot.matrix)
}
if (!require(DescTools)) {
  install.packages("DescTools")
  library(DescTools)
}
if (!require(DescTools)) {
  install.packages("DescTools")
  library(DescTools)
}
if (!require(glasso)) {
  install.packages("glasso")
  library(glasso)
}
if (!require(pracma)) {
  install.packages("pracma")
  library(pracma)
}
if (!require(matrixcalc)) {
  install.packages("matrixcalc")
  library(matrixcalc)
}
if (!require(hglasso)) {
  install.packages("hglasso")
  library(hglasso)
}
if (!require(gganimate)) {
  install.packages("gganimate")
  library(gganimate)
}
if (!require(magick)) {
  install.packages("magick")
  library(magick)
}
if (!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(gtable)) {
  install.packages("gtable")
  library(gtable)
}
if (!require(gridExtra)) {
  install.packages("gridExtra")
  library(gridExtra)
}
if (!require(RColorBrewer)) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
if (!require(igraph)) {
  install.packages("igraph")
  library(igraph)
}




##############################################
############# Part 0: Functions ##############
##############################################


#################################################
#################################################
## .BICemp:
##   Bayesian Information criteria for GGM estimation
##    as in Gao et al. (2012)
##
##  INPUTS:
##      mat       : sample covariance/correlation matrix.
##      inv_est   : estimated precision matrix/inverse correlation.
##      p         : total dimension.
##      n         : sample size.
##      threshold : degree count in BIC only considers 
##                    absolute entries over threshold.
##
##  OUTPUTS: 
##      .bic : value of the BIC.
##
.BICemp <- function(sigmaHat, precMatHat, n, p) {
  .bic <- 0
  .bic <- .bic - n * log(det(precMatHat))
  .bic <- .bic + n * Trace(precMatHat %*% sigmaHat)
  
  .edgenum = 0
  for (.i in 2:p) {
    for (.j in 1:(.i-1)) {
      .edgenum = .edgenum + (abs(precMatHat[.i, .j]) > 10^(-6))
    }
  }
  .bic <- .bic + log(n)*.edgenum
  return(.bic)
}


#################################################
#################################################
## wglasso:
##   Given fixed tuning parameters rho1, rho2, and
##   a set of estimated hubs hubshat, this
##   function calculates the Weighted Graphical LASSO
##   as described in McGillivray et al (2020)
##
##  INPUTS:
##      cov       : sample covariance/correlation matrix.
##      rho1      : tuning parameter for hub entries.
##      rho2      : larger tuning parameter for non-hub entries.
##      hubshat   : hubset estimator.
##      p,n       : data dimensions.
##
##  OUTPUTS: 
##      model       : object of type glasso containing the
##                      resulting estimator.
##      BIC         : value of BIC for estimator.
##      penalty     : tuning parameters c(rho1,rho2) in vector form.
##      total.time  : running time.
##
wglasso <- function(cov, rho1, rho2, hubshat, p, n) {
  .start_time = Sys.time()
  ## We define all the variables we need 
  ## for the estimation:
  .inv_cov = solve(cov)
  ## The cycle chooses all possible ordered
  ## pairs of weights:
  .hweight  = rho1 
  .nhweight = rho2
  ## First, generate the weight matrix:
  .W  <- matrix(rep(.nhweight, p * p), ncol = p)
  for (.hub in hubshat) {
    for (.var in (1:p)) {
      .W[.hub, .var] = .hweight
      .W[.var, .hub] = .hweight
    }
  }
  .W = .W - diag(diag(.W))
  
  .model.WGL = glasso(s = cov, rho = .W, 
                      nobs = n, zero = NULL, 
                      thr = 1.0e-4, maxit = 200,  approx = FALSE,
                      penalize.diagonal = TRUE, start = "cold",
                      w.init = NULL, wi.init = NULL, trace = FALSE)
  .BIC.WGL = .BICemp(sigmaHat = cov, 
                     precMatHat = .model.WGL$wi, 
                     n = n, p = p) 
  .end_time = Sys.time()
  .total_time = .end_time - .start_time
  
  
  .OUTPUT = list(model = .model.WGL, 
                 BIC = .BIC.WGL, 
                 penalty = c(rho1, rho2),
                 total.time = .total_time)
  return(.OUTPUT)
}


#################################################
#################################################
## wglasso:
##    Function that calculates different GM with 
##    varying penalty, and chooses the model with
## lowest BIC.
##
##  INPUTS:
##      cov       : sample covariance/correlation matrix.
##      rho       : vector of tuning parameter values to consider.
##      hubshat   : hubset estimator.
##      p,n       : data dimensions.
##
##  OUTPUTS: 
##      models        : list of glasso objects containing all the
##                      resulting estimators. 
##      BIC           : value of BIC for estimator.
##      optimal.penalty : choice of tuning parameters 
##                      c(rho1,rho2) minimizing the BIC.
##      optimal.model : object glasso with model that minimizes
##                        the BIC. 
##      total.time  : running time.
##
## 
wglassoBIC <- function(cov, rho, hubshat, p, n) {
  ## We define all the variables we need 
  ## for the estimation:
  .inv_cov = solve(cov)
  .rholength = length(rho)
  .BIC.WGL = NULL
  .model.WGL = list()
  .penalty.WGL = NULL
  .count = 0
  ## The cycle chooses all possible ordered
  ## pairs of weights:
  .start_time = Sys.time()
  for (.i in 1:(.rholength - 1)) {
    for (.j in (.i+1):.rholength) {
      
      .hweight  = rho[.i] 
      .nhweight = rho[.j]
      ## First, generate the weight matrix:
      .W  <- matrix(rep(.nhweight, p * p), ncol = p)
      for (.hub in hubshat) {
        for (.var in (1:p)) {
          .W[.hub, .var] = .hweight
          .W[.var, .hub] = .hweight
        }
      }
      .W = .W - diag(diag(.W))
      
      .count = .count + 1
      .model.WGL[[.count]] = glasso(s = cov, rho = .W, 
                                    nobs = n, zero = NULL, 
                                    thr = 1.0e-4, maxit = 200,  approx = FALSE,
                                    penalize.diagonal = TRUE, start = "cold",
                                    w.init = NULL, wi.init = NULL, trace = TRUE)
      .BIC.WGL = c(.BIC.WGL, .BICemp(sigmaHat = cov, 
                                     precMatHat = .model.WGL[[.count]]$wi, 
                                     n = n, p = p) )
      .penalty.WGL = rbind(.penalty.WGL, c(.hweight, .nhweight))
    }
  }
  .end_time = Sys.time()
  .total_time_WGL = .end_time - .start_time
  
  .OUTPUT = list(models = .model.WGL, 
                 BIC = .BIC.WGL, 
                 optimal.penalty = .penalty.WGL[which.min(.BIC.WGL),],
                 optimal.model = .model.WGL[[which.min(.BIC.WGL)]],
                 total.time = .total_time_WGL)
  return(.OUTPUT)
}
