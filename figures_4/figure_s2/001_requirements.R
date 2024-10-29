
wd <- getwd()
req_lib <- paste0(wd, "/req_lib")


if(!require(dplyr, lib = req_lib)){
  .libPaths(req_lib)
  install.packages("dplyr", repos = "https://archive.linux.duke.edu/cran/")
  library(dplyr)
}
if(!require(matrixcalc, lib = req_lib)){
  .libPaths(req_lib)
  install.packages("matrixcalc", repos = "https://archive.linux.duke.edu/cran/")
  library(matrixcalc)
}
if(!require(MASS, lib = req_lib)){
  .libPaths(req_lib)
  install.packages("MASS", repos = "https://archive.linux.duke.edu/cran/")
  library(MASS)
}
if(!require(mvtnorm, lib = req_lib)){
  .libPaths(req_lib)
  install.packages("mvtnorm", repos = "https://archive.linux.duke.edu/cran/")
  library(mvtnorm)
}
if(!require(e1071, lib = req_lib)){
  .libPaths(req_lib)
  install.packages("e1071", repos = "https://archive.linux.duke.edu/cran/")
  library(e1071)
}



  