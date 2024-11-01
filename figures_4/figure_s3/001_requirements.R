

###################### Creating folders:
subfolder_new <- "req_lib/"
wd <- getwd()
req_lib <- paste0(wd, "/req_lib")

if (!dir.exists(subfolder_new)) {
       dir.create(subfolder_new)
}

if (!dir.exists(paste0(wd, "/100_ScreeningSimulations/outputs1"))) {
  dir.create(paste0(wd, "/100_ScreeningSimulations/outputs1"))
}
.libPaths(req_lib)


## Package Installation:

if(!require(tidyverse, lib = req_lib)){
  .libPaths(req_lib)
  install.packages("tidyverse", repos = "https://archive.linux.duke.edu/cran/")
  library(tidyverse)
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
if(!require(pryr, lib = req_lib)){
  .libPaths(req_lib)
  install.packages("pryr", repos = "https://archive.linux.duke.edu/cran/")
  library(pryr)
}
if(!require(pracma)){
  .libPaths(req_lib)
  install.packages("pracma", repos = "https://archive.linux.duke.edu/cran/")
  library(pracma)
}
if(!require(pROC)){
  .libPaths(req_lib)
  install.packages("pROC_1.17.0.1.tar.gz", repos = "https://archive.linux.duke.edu/cran/", type="source")
  library(pROC)
}
if(!require(Ckmeans.1d.dp)){
  .libPaths(req_lib)
  install.packages("Ckmeans.1d.dp_4.3.3.tar.gz", repos = "https://archive.linux.duke.edu/cran/", type="source")
  library(Ckmeans.1d.dp)
}
if(!require(spam)){
  .libPaths(req_lib)
  install.packages("spam", repos = "https://archive.linux.duke.edu/cran/")
  library(spam)
}
if(!require(ggpubr)){
  .libPaths(req_lib)
  install.packages("ggpubr", repos = "https://archive.linux.duke.edu/cran/")
  library(ggpubr)
}
  