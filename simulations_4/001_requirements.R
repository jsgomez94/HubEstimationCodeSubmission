
wd <- getwd()
req_lib_dir <- paste0(wd,"/req_lib")
packageVersion("rlang")


###################### Creating folders:
subfolder_new        <- paste0("req_lib/")

if (!dir.exists(subfolder_new)) {
       dir.create(subfolder_new)
}

if(!require(magrittr, lib = req_lib_dir)){
  .libPaths(req_lib_dir)
  install.packages("magrittr", repos = "https://archive.linux.duke.edu/cran/")
  library(magrittr)
}
if(!require(tidyverse, lib = req_lib_dir)){
  .libPaths(req_lib_dir)
  install.packages("tidyverse", repos = "https://archive.linux.duke.edu/cran/")
  library(tidyverse)
}
if(!require(testthat, lib = req_lib_dir)){
  .libPaths(req_lib_dir)
  install.packages("testthat", repos = "https://archive.linux.duke.edu/cran/")
  library(testthat)
}
if(!require(matrixcalc, lib = req_lib_dir)){
  .libPaths(req_lib_dir)
  install.packages("matrixcalc", repos = "https://archive.linux.duke.edu/cran/")
  library(matrixcalc)
}
if(!require(pracma, lib = req_lib_dir)){
  .libPaths(req_lib_dir)
  install.packages("pracma", repos = "https://archive.linux.duke.edu/cran/")
  library(pracma)
}
if(!require(e1071, lib = req_lib_dir)){
  .libPaths(req_lib_dir)
  install.packages("e1071", repos = "https://archive.linux.duke.edu/cran/")
  library(e1071)
}
if(!require(MASS, lib = req_lib_dir)){
  .libPaths(req_lib_dir)
  install.packages("MASS", repos = "https://archive.linux.duke.edu/cran/")
  library(MASS)
}
if(!require(Matrix, lib = req_lib_dir)){
  .libPaths(req_lib_dir)
  install.packages("Matrix", repos = "https://archive.linux.duke.edu/cran/")
  library(Matrix)
}
if(!require(RSpectra, lib = req_lib_dir)){
  .libPaths(req_lib_dir)
  install.packages("RSpectra", repos = "https://archive.linux.duke.edu/cran/")
  library(RSpectra)
}
if(!require(spam, lib = req_lib_dir)){
  .libPaths(req_lib_dir)
  install.packages("spam", repos = "https://archive.linux.duke.edu/cran/")
  library(spam)
}
if(!require(mvtnorm, lib = req_lib_dir)){
  .libPaths(req_lib_dir)
  install.packages("mvtnorm", repos = "https://archive.linux.duke.edu/cran/")
  library(mvtnorm)
}
if(!require(glasso, lib = req_lib_dir)){
  .libPaths(req_lib_dir)
  install.packages("glasso", repos = "https://archive.linux.duke.edu/cran/")
  library(glasso)
}
if(!require(hglasso, lib = req_lib_dir)){
  .libPaths(req_lib_dir)
  install.packages("hglasso", repos = "https://archive.linux.duke.edu/cran/")
  library(hglasso)
}
if(!require(readr, lib = req_lib_dir)){
  .libPaths(req_lib_dir)
  install.packages("readr", repos = "https://archive.linux.duke.edu/cran/")
  library(readr)
}
if(!require(ggh4x, lib = req_lib_dir)){
  .libPaths(req_lib_dir)
  install.packages("ggh4x", repos = "https://archive.linux.duke.edu/cran/")
  library(ggh4x)
}
if(!require(gridExtra, lib = req_lib_dir)){
  .libPaths(req_lib_dir)
  install.packages("gridExtra", repos = "https://archive.linux.duke.edu/cran/")
  library(gridExtra)
}
#if(!require(ggpubr, lib = req_lib_dir)){
#  .libPaths(req_lib_dir)
#  install.packages("ggpubr", repos = "https://archive.linux.duke.edu/cran/")
#  library(ggpubr)
#}

