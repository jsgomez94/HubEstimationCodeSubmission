
#################################################
#################################################
#################################################
#################################################
## Methods for hub detection:


#################################################
#################################################
## alphahubs: 
##    Function that summarizes the accuracy of
##    strength-hub estimation, given an 
##    estimated PM and the set of true hubs.
##    Hubs are estimated with L2 norm.
##
##  INPUTS
##    Theta     : precision matrix estimator.
##    trueHubs  : set of true hubs.
##    q         : total dimension.
##    nhubs     : number of true hubs.
##
##  OUTPUT
##    TP        : true positive rate of hub recovery.
##    FP        : false positive rate of hub recovery.
##    AUC       : area under the curve summary for the
##                 l2 norms. 
##
alphahubs = function(Theta, 
                     trueHubs, 
                     q,
                     nhubs){
  
  ## Find AUC:
  .alphas = .alphavals(Theta)
  .auc = auc(response = trueHubs, 
             predictor = .alphas,
             direction = "<")
  
  ## Find 2 means cluster accuracy:
  .Twomeans = Ckmeans.1d.dp(x = .alphas, k = 2, method = "linear")
  .nonhubclust = which.min(.Twomeans$centers)
  .hubclust = which.max(.Twomeans$centers)
  
  .hubshat = (.Twomeans$cluster == .hubclust)
  
  ## Knowing the hubs, let us find the TP, TN, FP ratios.
  .TP = sum(.hubshat & trueHubs) / ( nhubs )   
  .FP = sum(.hubshat & !trueHubs) / (q - nhubs)
  
  .OUTPUT = list(TP = .TP,
                 FP = .FP,
                 AUC = .auc)
  return(.OUTPUT)
}

#################################################
#################################################
## .alphavals: 
##    Auxiliary function.
##    Computes the alpha-values of a given matrix.
##
##  INPUTS
##    Theta     : precision matrix estimator.
##
##  OUTPUT
##    .alpha    : L2^2 columnwise norms.
##
.alphavals = function(Theta){
  .alpha = apply(Theta^2, MARGIN = 2, sum)
  return(.alpha)
}




#################################################
#################################################
## degreehubs: 
##    Function that summarizes the accuracy of
##    degree-hub estimation, given an 
##    estimated PM and the set of true hubs.
##    Hubs are estimated with L0 norm.
##
##  INPUTS
##    Theta     : precision matrix estimator.
##    trueHubs  : set of true hubs.
##    q         : total dimension.
##    nhubs     : number of true hubs.
##
##  OUTPUT
##    TP        : true positive rate of hub recovery.
##    FP        : false positive rate of hub recovery.
##    AUC       : area under the curve summary for the
##                 l2 norms. 
##
degreehubs = function(Theta, 
                      trueHubs, 
                      q,
                      nhubs){
  
  ## Find AUC
  .degrees = apply(Theta, MARGIN = 2, 
                   function(x) {sum(abs(x) > 0) })
  .auc = auc(response = trueHubs, 
             predictor = .degrees,
             direction = "<")
  
  ## Find 2 means cluster accuracy:
  .Twomeans = Ckmeans.1d.dp(x = .degrees, k = 2, method = "linear")
  .nonhubclust = which.min(.Twomeans$centers)
  .hubclust = which.max(.Twomeans$centers)
  
  .hubshat = (.Twomeans$cluster == .hubclust)
  
  ## Knowing the hubs, let us find the TP, TN, FP ratios.
  .TP = sum(.hubshat & trueHubs)/  ( nhubs )      
  .FP = sum(.hubshat & !trueHubs)/ (q - nhubs)
  
  .OUTPUT = list(TP = .TP,
                 FP = .FP,
                 AUC = .auc)
  
  return(.OUTPUT)
}



#################################################
#################################################
## ipchdhubs: 
##    Function that summarizes the accuracy of
##    ipc-hd estimation, given an 
##    estimated PM and the set of true hubs.
##
##  INPUTS
##    Theta     : precision matrix estimator.
##    trueHubs  : set of true hubs.
##    q         : total dimension.
##    n         : sample size.
##    r         : number of true hubs.
##    method    : measure used for variable screening.
##                  Can use "maxcor", "l1" or "l2".
##
##  OUTPUT
##    TP        : true positive rate of hub recovery.
##    FP        : false positive rate of hub recovery.
##    AUC       : area under the curve summary for the
##                 l2 norms. 
##
ipchdhubs = function(empcov, 
                     trueHubs, 
                     q, n, r, 
                     method = "maxcor"){
  
  ## Step 1: reduce dimension:
  .red      = .reduce.dim(empcov = empcov, 
                          q = q, n = n, r = r, 
                          method = method)
  .shat     = .shat(empcov = .red$empcov)
  .weights  = .weights(.red = .red, .shat = .shat, q = q)
  .red.weights = .weights[.red$vars]
  .hubshat = .red$hubest
  
  ## Find AUC:
  .auc = auc(response = trueHubs, 
             predictor = .weights,
             direction = "<")
  
  ## Find 2 means cluster accuracy:
  .Twomeans = Ckmeans.1d.dp(x = .red.weights, k = 2, method = "linear")
  .nonhubclust = which.min(.Twomeans$centers)
  .hubclust = which.max(.Twomeans$centers)
  
  ## Lets find 
  .red.hubshat = (.Twomeans$cluster == .hubclust)
  .hubshat[.red$vars] = .red.hubshat
  
  ## Find the hub set:
  # .hubshat = (.weights > 2*.shat/ p)
  
  ## Knowing the hubs, let us find the TP, TN, FP ratios.
  .TP = sum(.hubshat & trueHubs) / (r)   
  .FP = sum(.hubshat & !trueHubs) / (q - r)
  
  
  .OUTPUT = list(TP = .TP,
                 FP = .FP,
                 AUC = .auc)
  return(.OUTPUT)
}

#################################################
#################################################
## .reduce.dim: 
##    Auxiliary function.
##    Performs dimension reduction for the covariance
##    matrix.

##  INPUTS
##    empcov   : covariance matrix estimator.
##    q        : total dimension.
##    n        : sample size.
##    r        : number of true hubs in data.
##    method   : measure used for variable screening.
##                  Can use "maxcor", "l1" or "l2".
##
##  OUTPUT
##    strength  : connectivity strength measurement used 
##                  for screening.
##    hubest    : variables selected by the screening method.
##    empcov    : pxp reduced covariance post variable screening.
##    vars      : variables selected by the screening method.
##    position  : q-sized vector with 0s for variables not selected,
##                  and the position of the variable in the reduced
##                  matrix post screening.
##    p         : number of variables post dimension reduction.
##    ndetected : number of hubs selected by the screening method.
##
.reduce.dim = function (empcov, q, n, r, method = "maxcor") {
  .maxcor = NULL
  if (method == "maxcor") {
    .maxcor = apply(empcov - diag(diag(empcov)), 
                    MARGIN = 1,
                    function(x) max(abs(x)))
  } else if (method == "l2") {
    .maxcor = apply(empcov - diag(diag(empcov)), 
                    MARGIN = 1,
                    function(x) sum(x^2))
  } else if (method == "l1") {
    .maxcor = apply(empcov - diag(diag(empcov)), 
                    MARGIN = 1,
                    function(x) sum(abs(x)))
  }
  
  ## Choose the variables with top alpha-vals
  .p = floor(min(0.75*n, 0.5*q))
  .order = order(.maxcor, decreasing = TRUE)
  
  ## TRUE on selected vars/ 
  ## FALSE on dropped vars.
  .vars = (1:q) %in% .order[1:(.p)]
  .detected = sum((.vars) * ((1:q) < r + 1))
  
  ## New-index-num on selected vars/ 
  ## zero on dropped vars.
  .position = rep(0, q)
  for(.i in 1:q){
    .position[.i] = sum(.vars[1:(.i)])* .vars[.i]
  }
  
  ## Eliminate from hubest the dropped vars.
  .red.hubest = .vars
  
  ## Reduced matrix.
  .red.empcov = empcov[.vars, .vars]
  
  .OUTCOME = list(strength = .maxcor, 
                  hubest = .red.hubest,
                  empcov = .red.empcov,
                  vars = .vars,
                  position = .position,
                  p = .p,
                  ndetected = .detected)
  return(.OUTCOME)
}
#################################################
#################################################
## .eigengaps: 
##    Computes the leading consecutive eigenvalue ratios 
##    up to a cutoff.

##  INPUTS
##    empcov   : covariance matrix estimator.
##    cutoff   : number of eigenvalues to include in calculations
##
##  OUTPUT
##    .gaps    : cuttoff-length vector with 
##                ratios between consecutive eigenvalues
##
.eigengaps = function(empcov, cutoff = NULL){
  .p = dim(empcov)[1]
  if(is.null(cutoff))
    cutoff = floor(.p/2)
  
  .vals = 1 / ( eigen(empcov)$values[(.p):1] )
  .gaps = .vals[1:cutoff]/.vals[2:(cutoff + 1)]
  return(.gaps)
}

#################################################
#################################################
## .shat: 
##    Computes the largest eigenvalue gap within a 
##    given cutoff.
##
##  INPUTS
##    empcov   : covariance matrix estimator.
##    cutoff   : number of eigenvalues to include in calculations
##
##  OUTPUT
##    .shat    : index of the largest ratio between consecutive
##                eigenvalues
##
.shat = function(empcov, cutoff = NULL){
  .p = dim(empcov)[1]
  if(is.null(cutoff))
    cutoff = floor(.p/2)
  
  .gaps = .eigengaps(empcov, cutoff)
  .sort = sort(.gaps, decreasing = TRUE)
  
  .max = .sort[1]
  .max2 = .sort[2]
  
  .shat = NULL
  if(.max > 1.5 * .max2){
    .shat = which.max(.gaps)
  }
  
  return(.shat)
}
#################################################
#################################################
## .weights: 
##    Given the eigenvalue gap found in the previous
##    function, it calculates the influence measures.
##
##  INPUTS
##    .red     : list object. Output of the function reduce.dim().
##    .shat    : index of the largest ratio between consecutive
##                eigenvalues, calculated by function .shat().
##    q        : total dimension.
##
##  OUTPUT
##    .output.weights : Numeric vector of length p. Influence measures 
##                        associated with the reduced covariance matrix 
##                        obtained by reduce.dim().
##
.weights = function (.red, .shat, q) {
  .p = .red$p
  
  .red.weights = NULL
  if (is.null(.shat)) {
    .shat.new = floor(.p / 5)
    .vectors = eigen(.red$empcov)$vectors[, .p:(.p - .shat.new + 1)]  
    .red.weights = apply(.vectors^2, MARGIN = 1, sum)
  } else if (.shat == 1) {
    .red.weights = eigen(.red$empcov)$vectors[, .p]^2
  } else if (.shat > 1) {
    .vectors = eigen(.red$empcov)$vectors[, .p:(.p - .shat + 1)]  
    .red.weights = apply(.vectors^2, MARGIN = 1, sum)
  }
  
  .output.weights = rep(0, q)
  for(.index in 1:q){
    if(.red$position[.index] != 0){
      .newindex = .red$position[.index]
      .output.weights[.index] = .red.weights[.newindex]
    }
  }
  
  return(.output.weights)
}

