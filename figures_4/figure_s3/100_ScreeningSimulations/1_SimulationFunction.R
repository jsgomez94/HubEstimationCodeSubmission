#################################################
#################################################
## CreateParameters:
##    Function that, given a particular simulation
##    ID id_task, returns the simulation parameters
##    corresponding to such task ID. 
##
##  INPUTS
##    id_task  : ID of the current simulation task.
##                If id_task==0, is a small debugging example.
##                If id_task%in$1:42, simulations with
##                  different choices of p,q,n, ph, pnh, etc.
##    
##  OUTPUT:
##    id_task : saving the original input id_task into the output.
##    q       : Total dimension of pxp matrix.
##    p      : Size of pxp highly connected submatrix.
##    r       : number of hubs.
##    ph      : Probability of hub connectivity 
##    pnh     : Probability of connection for variables 
##                in the high-connection T0xT0 submatrix.
##    pneff   : Probability of connection for all
##                variables outside of the T0xT0 
##                sumatrix.
##    diagonal_shift : 
##                Size of minimum eigenvalue of the 
##                precision matrix.
##    shuffle : If true, shuffles rows/columns to a random
##                position.
##    type    : whether the distribution signed-uniform
##                or zero-mean normal.
##    hmin    : Used when type = "unif". Minimum absolute 
##                value of hub entries.
##    hmax    : Used when type = "unif". Maximum absolute 
##                value of hub entries. 
##    nhmin   : Used when type = "unif". Minimum absolute 
##                value of entries in T0xT0 submatrix 
##                not related to hub variables.
##    nhmax   : Used when type = "unif". Maximum absolute 
##                value of entries in T0xT0 submatrix 
##                not related to hub variables.
##    neffmin : Used when type = "unif". Minimum absolute 
##                value of entries outside of the 
##                T0xT0 submatrix 
##    neffmin : Used when type = "unif". Minimum absolute 
##                value of entries outside of the 
##                T0xT0 submatrix 
##    T0_prop : ratio T0 / p.
##    n_prop  : ratio n / p.
##    nsim    : number of simulation repetitions used in this node.
##    running_days : time limit (in days) for task in the cluster call.
##    threshold : Deprecated. IPC-HD overestimation of s. 
##    M       : Deprecated. Number of resamples for parameter tuning.
##
CreateParameters = function(id_task){
  .args = list()
  
  if(id_task == 0){
    .args$q = 50; .args$p = 10; .args$r = 2; .args$diagonal_shift = 1
    .args$ph = 1; .args$pnh = 0; .args$n = 500
    
    .args$nsim = 5; .args$M = 100; .args$hgl.length.out = 2
    
    .args$hmin = 4; .args$hmax = 6; .args$nhmin = 4; .args$nhmax = 6
    .args$shuffle = FALSE; .args$type = "unif"
    
    .args$threshold = 2
    
    return(.args)
    
  }
  
  ############################ 
  ## Define the dimension.
  if(id_task < 43){
    .args$p = 100
    .args$r = 5
  } 
  .id_task2 = id_task
  
  ############################ 
  ## Define the size of the diagonal
  if(.id_task2 < 22){
    .args$diagonal_shift = 1
  } else if (.id_task2 < 43) {
    .args$diagonal_shift = 2
  }
  .id_task3 = .id_task2 - 21*(.id_task2 > 21)
  
  
  ############################ 
  ## Define the effective dimension:
  if(.id_task3 < 8){
    .args$n = .args$p * 2 
  } else if (.id_task3 < 15){
    .args$n = .args$p * 4 
  } else if (.id_task3 < 22){
    .args$n = .args$p * 8 
  }
  .id_task4 = .id_task3 - 7*floor((.id_task3 - 1)/7)
  
  ############################ 
  ## Define the sample size:
  .args$q = .args$p * (2^.id_task4)
  
  ############################ 
  ## Define the strength of the hubs:
  .args$ph = 0.8
  .args$pnh = 0.05
  
  ############################ 
  ## Define the rest of parameters:
  
  .args$nsim = 100

  .args$hmin = 4
  .args$hmax = 6
  .args$nhmin = 4
  .args$nhmax = 6
  
  .args$shuffle = FALSE
  .args$type = "unif"
  
  .args$threshold = floor(.args$p/5)
  
  return(.args)
}


#####################################################
#####################################################
##  FullSimulation: 
##    Given simulation parameters, 
##
##
##  INPUTS:
##    args    : output object from CreateParameters() 
##                function.
##
##  OUTPUTS:
##    .output : matrix with AUC, TPR and FPR measurements
##                for the methods included in the simulations.
##
FullSimulation = function(args){
  
  #####################################################
  #####################################################
  ## Create matrix to save outputs:
  .nmethods = 3
  .TPeff.mat = matrix(rep(0, .nmethods * args$nsim), ncol = args$nsim)
  .TPhubs.mat = matrix(rep(0, .nmethods * args$nsim), ncol = args$nsim)
  .auc.mat = matrix(rep(0, .nmethods * args$nsim), ncol = args$nsim)
  
  #####################################################
  #####################################################
  ## Find the tuning parameters for the methods:
  

  #################################################
  #################################################
  ## Cycle:
  for(.sim in 1:args$nsim){
    .count = 1
    .loop_start_time = Sys.time()
    ############################    
    ## Generate data:
    {
      print(paste("Generating pop PM/IC (", 
                  round( 100*(.sim - 1)/args$nsim, 2), 
                  "%" , ")" ))
      .pm = spam_diag(args$q)* args$diagonal_shift
      object_size(.pm)
      
      .red.pm =  r.sparse.pdhubmat(
        p = args$p, T0 = args$p, r = args$r,
        ph = args$ph, pnh = args$pnh, pneff = 0,
        diagonal_shift = args$diagonal_shift,
        shuffle = args$shuffle, type = args$type,
        hmin = args$hmin, hmax = args$hmax,
        nhmin = args$nhmin, nhmax = args$nhmax,
        neffmin = args$nhmin, neffmax = args$nhmax)

      .pm[1:args$p, 1:args$p] = .red.pm
      
      print(paste("PM:",object_size(.pm)))
      
      .sigma = spam_diag(args$q)/ args$diagonal_shift
      .sigma[1:args$p, 1:args$p] = solve(.red.pm)
      print(paste("Sigma:",object_size(.sigma)))
      
      .X = rmvnorm.spam(n = args$n, Sigma = .sigma)
      print(paste("X:",object_size(.X)))
      
      .trueHubs = ( (1:args$q) <= args$r )
      .trueEffective = ( (1:args$q) <= args$p )
    }
    ############################
    ######## Finding PM/IC matrix:
    ########
    {
      print(paste("Finding emp PM/IC (", 
                  round( 100*(.sim - 1)/args$nsim, 2), 
                  "%" , ")" ))
      
      .start_time = Sys.time()
      .rhoHat = cor(.X)
    }
    ############################
    ######## Integral CORR method:
    ########
    {
      print(paste("Corr IPC-HD (",
                  round( 100*(.sim - 1)/args$nsim, 2),
                  "%" , ")" ))
      .output = .reduce.dim(empcov = .rhoHat, 
                            q = args$q, 
                            n = args$n, 
                            r = args$r, 
                            method = "maxcor")
      
      .auc = auc(response = .trueEffective, 
                 predictor = .output$strength,
                 direction = "<")
      
      .TPeffective = sum(.trueEffective & .output$vars) / (args$p)   
      .TPhubs = sum(.trueHubs & .output$vars) / (args$r)   
      
      .end_time = Sys.time()
      .TPeff.mat[.count, .sim] = .TPeffective
      .TPhubs.mat[.count, .sim] = .TPhubs
      .auc.mat[.count, .sim] = .auc
      .count = .count + 1
    }  
    ############################
    ######## Integral CORR method:
    ########
    {
      print(paste("Corr IPC-HD (",
                  round( 100*(.sim - 1)/args$nsim, 2),
                  "%" , ")" ))
      
      .output = .reduce.dim(empcov = .rhoHat, 
                            q = args$q, 
                            n = args$n, 
                            r = args$r, 
                            method = "l2")
      
      .auc = auc(response = .trueEffective, 
                 predictor = .output$strength,
                 direction = "<")
      
      .TPeffective = sum(.trueEffective & .output$vars) / (args$p)   
      .TPhubs = sum(.trueHubs & .output$vars) / (args$r)   
      
      .end_time = Sys.time()
      .TPeff.mat[.count, .sim] = .TPeffective
      .TPhubs.mat[.count, .sim] = .TPhubs
      .auc.mat[.count, .sim] = .auc
      .count = .count + 1
    }  
    ############################
    ######## Integral CORR method:
    ########
    {
      print(paste("Corr IPC-HD (",
                  round( 100*(.sim - 1)/args$nsim, 2),
                  "%" , ")" ))
      .output = .reduce.dim(empcov = .rhoHat, 
                            q = args$q, 
                            n = args$n, 
                            r = args$r, 
                            method = "l1")
      
      .auc = auc(response = .trueEffective, 
                 predictor = .output$strength,
                 direction = "<")
      
      .TPeffective = sum(.trueEffective & .output$vars) / (args$p)   
      .TPhubs = sum(.trueHubs & .output$vars) / (args$r)   
      
      .end_time = Sys.time()
      .TPeff.mat[.count, .sim] = .TPeffective
      .TPhubs.mat[.count, .sim] = .TPhubs
      .auc.mat[.count, .sim] = .auc
      .count = .count + 1

    }  
    
    #################################################
    #################################################
    
    .loop_end_time = Sys.time()
    print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    print(paste("Loop", .sim, "took:"))
    print(difftime(time1 = .loop_end_time ,
                   time2 = .loop_start_time,
                   units = "auto"))
    print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    print("Before deletion:")
    print(gc())
    rm(.pm)
    rm(.sigma)
    rm(.X)
    rm(.rhoHat)
    print("After deletion:")
    print(gc())
    print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  }
  
  #################################################
  ## Clean outputs:
  .output = matrix(0, nrow = .nmethods, ncol = 6)
  colnames(.output) = c("AUC", "AUC.sd", "TPeff","TPeff.sd", "TPhubs","FPhubs.sd")  
  
  rownames(.output) = c("IPCHD_MC", "IPCHD_L2", "IPCHD_L1")
  
  .output[, 1] = apply(.auc.mat, MARGIN = 1, mean)
  .output[, 2] = apply(.auc.mat, MARGIN = 1, sd)
  .output[, 3] = apply(.TPeff.mat, MARGIN = 1, mean)
  .output[, 4] = apply(.TPeff.mat, MARGIN = 1, sd)
  .output[, 5] = apply(.TPhubs.mat, MARGIN = 1, mean)
  .output[, 6] = apply(.TPhubs.mat, MARGIN = 1, sd)
  
  
  return(.output)
  
}
