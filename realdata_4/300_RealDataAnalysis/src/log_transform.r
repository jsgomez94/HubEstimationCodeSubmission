# ################################################################
# ################################################################
## Normalizing:

#################################################
#################################################
## right_log_transform: 
##    Applies min-adjusted log transformation to
##    all variables in data that have high right 
##    skewness.
##
##  INPUTS
##    data      : dataframe.
##    vars      : numeric vector of indeces. 
##                  variables that may be elegible
##                  for log-transformation.
##    right_threshold : if variable has skewness
##                  above this value, it is log-transformed
##
##  OUTPUT
##    .A    : dataframe of same dimension as data,
##              with variables of high positive skewness
##              log-transformed.
##
right_log_transform <- function(data, vars, right_threshold) {

    skew <- apply(X = data[, vars],
               MARGIN = 2,
               skewness # nolint
              )
    log_data <- data

    highskew <- vars[skew > right_threshold]

    ## ELIMINATING RIGHT SKEWNESS WITH
    ## SHIFTED LOG-TRANSFORMATION
    for (.var in highskew) {
        ## Extract the non-missing obs.
        .x <- log_data[, .var]
        .min <- min(.x)

        ## Find the appropiate shifting:
        .shifts <- 10^(seq(-1, 4, length.out = 50))
        .shift_skewness <- rep(NA, 50)
        for (.i in 1:50) {
            .y <- log(.x - .min + .shifts[.i])
            .shift_skewness[.i] <- skewness(.y) # nolint
        }
        .min_i <- which.min(abs(.shift_skewness))
        .shift <- .shifts[.min_i]

        .x <- log(.x - .min + .shift)
        log_data[, .var] <- .x
    }



    return(log_data)
}


#################################################
#################################################
## left_log_transform: 
##    Applies min-adjusted log transformation to
##    all variables in data that have high right 
##    skewness.
##
##  INPUTS
##    data      : dataframe.
##    vars      : numeric vector of indeces. 
##                  variables that may be elegible
##                  for log-transformation.
##    right_threshold : if variable has skewness
##                  below this negative value, it is 
##                  log-transformed
##
##  OUTPUT
##    .A    : dataframe of same dimension as data,
##              with variables of high positive skewness
##              log-transformed.
##
left_log_transform <- function(data, vars, left_threshold) {

    skew <- apply(X = data[, vars],
               MARGIN = 2,
               skewness # nolint
              )
    lowskew <- vars[skew < left_threshold]

    log_data <- data



    ## ELIMINATING LEFT SKEWNESS WITH
    ## SHIFTED LOG-TRANSFORMATION
    for (.var in lowskew) {
        ## Extract the non-missing obs.
        .x <- log_data[, .var]
        .max <- max(.x)

        ## Find the appropiate shifting:
        .shifts <- 10^(seq(-1, 4, length.out = 50))
        .shift_skewness <- rep(NA, 50)
        for (.i in 1:50) {
            .y <- log(-.x + .max + .shifts[.i])
            .shift_skewness[.i] <- skewness(.y) # nolint
        }
        .min_i <- which.min(abs(.shift_skewness))
        .shift <- .shifts[.min_i]

        .x <- -log(-.x + .max + .shift)
        log_data[, .var] <- .x
}



    return(log_data)
}