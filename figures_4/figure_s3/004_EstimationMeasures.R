#################################################
#################################################
#################################################
##
## In the following document, we introduce the
## functions that take a covariance matrix 
## estimate and turn them into a hub estimation.
## We measure:
##  1) TP rate.
##  2) FP rate.
##  3) AUC
##
#################################################
#################################################





#################################################
#################################################
#################################################
#################################################
## Methods for hub detection:

######################
######################
## Function that summarizes the accuracy of
## strength-hub estimation, given an 
## estimated PM and the set of true hubs.
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
######################
## Computes the alpha-values of a given matrix.
.alphavals = function(Theta){
  .alpha = apply(Theta^2, MARGIN = 2, sum)
  return(.alpha)
}




######################
######################
## Function that summarizes the accuracy of
## degree-hub estimation, given an 
## estimated PM and the set of true hubs.
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




######################
######################
## Function that summarizes the accuracy of
## ipc-hd estimation, given an 
## estimated PM and the set of true hubs.
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
######################
## Reduce dimension by choosing larger max-corr.
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
######################
## Computes the leading consecutive eigenvalue ratios 
## up to a cutoff.
.eigengaps = function(empcov, cutoff = NULL){
  .p = dim(empcov)[1]
  if(is.null(cutoff))
    cutoff = floor(.p/2)
  
  .vals = 1 / ( eigen(empcov)$values[(.p):1] )
  .gaps = .vals[1:cutoff]/.vals[2:(cutoff + 1)]
  return(.gaps)
}
######################
## Computes the largest eigenvalue gap within a 
## given cutoff.
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
######################
## Given the eigenvalue gap found in the previous
## function, it finds the omega-weights.
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

