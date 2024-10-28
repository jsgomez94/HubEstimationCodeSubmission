# ################################################################
# ################################################################
## PLOTTING FUNCTIONS:

cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")

#################################################
#################################################
## plot.mardist: given an nxp dataset where p is
##    large, this function displays the marginal
##    density of a subset of the p variables. The
##    density plots are ordered in increasing order
##    according to a statistic of interest.
##
##  INPUTS
##    data    : nxp matrix of data.
##    n,p     : dimensions of the given data.
##    scores  : p-vector with the scores of each
##                variable. Used to order plots.
##    random  : logical. If FALSE, selects regularly 
##                spaced variables for plotting. 
##                Otherwise, selects randomly.
##    minmax  : If TRUE, the variables with the 
##                lowest/highest scores are plotted. 
##    nprow   : Number of rows in the multiplot.    
##    npcol   : Number of columns in the multiplot.
##    lwd, lcol, pwd, pcol :
##                color/width parameters forlines/points.
##    main    : outer title of multiplot.
##    cex.omain, cex.main :
##                font size of outer and inner titles.
##
##  OUTPUT
##    Plot with nprow x npcol marginal density plots
##      of variables selected in the dataset.
##
plot.mardist <- function(data = NULL,
                        n, p,
                        scores = NULL,
                        random = TRUE,
                        minmax = TRUE,
                        nprow, npcol,
                        lwd = 1, lcol = "black",
                        pwd = 0.5, pcol = "blue",
                        main = NULL, cex.omain = 3, # nolint
                        cex.main = 1) { # nolint
  .nplot <- nprow * npcol

  #### Step 1: Select which variables to plot.
  ####
  ## Order variables by their score.
  .orderscores <- order(scores,
                       decreasing = TRUE)
  .indexes <- rep(NA, .nplot)

  par(mfrow = c(1, 1))
  if (minmax && random) { ## If we choose randomly, including min/max variables.
    .indexes[1]      <- .orderscores[1]
    .indexes[.nplot] <- .orderscores[p]

    ## Choosing which positions to select
    .ordind <- sample(x = 2:(p - 1),
                        size = .nplot - 2,
                        replace = FALSE)
    .ordind <- sort(.ordind,
                   decreasing = FALSE)
    .indexes[2:(.nplot - 1)] <- .orderscores[.ordind]
  } else if (!minmax && random) { ## If we choose variables fully randomly:
    .ordind <- sample(x = 1:p,
                     size = .nplot,
                     replace = FALSE)
    .ordind <- sort(.ordind,
                   decreasing = FALSE)
    .indexes <- .orderscores[.ordind]
    } else if (!random) { ## If we select a deterministic grid of values:
    a <- floor(p / (.nplot - 1))
    r <- p - a * .nplot

    .ordind <- a * (1:(.nplot - 1)) + r
    .ordind <- c(1, .ordind)

    .indexes <- .orderscores[.ordind]
  }

  ## Once we determined the set of variables
  ## to plot, we plot them.
  par(mfrow = c(nprow, npcol),
      oma = c(0, 0, 4, 0))

  for (.var in .indexes){
    .missing <- is.na(data[, .var])
    .nvarcol <- sum(!.missing)
    .varcol <- data[!.missing, .var]
    .pmain <- paste(wrap_sentence(string = colnames(data)[.var],
                                 width = 30), "\n",
                   round(scores[.var], digits = 2))

    .densityplot <- density(.varcol)
    plot(.densityplot,
         main = .pmain,
         cex.main = cex.main,
         col = lcol,
         lwd = lwd)

    abline(v = 0)

    ## Create cloud plot:
    .ymax <- max(.densityplot$y)
    points(x = .varcol,
           y = runif(.nvarcol,
                     min = .ymax / 3,
                     max = 2 * .ymax / 3),
           pch = 19,
           col = pcol,
           cex = pwd)
  }
  # title
  mtext(text = main,
        side = 3,
        outer = TRUE,
        cex = cex.omain)

  ## restore original settings
}


#################################################
#################################################
## wrap_sentence: Auxiliary function used by 
##    plot.mardist function. Takes long titles and
##    breaks them into several lines. 
##
##  INPUTS
##    string  : character object with words separated
##                by underscores "_".
##    width   : Maximum allowed number of characters
##                per line.
##    
##  OUTPUT
##    fullsentence :
##      character object with same text as "string"
##      but separations are now spaces, separated by
##      lines and each line has at most "width" 
##      individual characters.
##
wrap_sentence <- function(string, width) {
  words <- unlist(strsplit(string, "_"))
  fullsentence <- ""
  checklen <- ""
  for (i in 1:length(words)) { # nolint
    checklen <- paste(checklen, words[i])
    if (nchar(checklen) > (width + 1)) {
      fullsentence <- paste0(fullsentence, "\n")
      checklen <- ""
    }
    fullsentence <- paste(fullsentence, words[i])
  }
  fullsentence <- sub("^\\s", "", fullsentence)
  fullsentence <- gsub("\n ", "\n", fullsentence)
  return(fullsentence)
}
  
  
# ################################################################
# ################################################################
## FUNCTIONS FOR DETECTING/VISUALIZING MISSING ENTRIES
  
#################################################
#################################################
## na.count:
##    Counts the number of entries in each row/column
##    of a data matrix that contain NA entries.
##
##  INPUTS
##    data    : data matrix.
##    MARGIN  : If 1, counts NA entries row-wise. 
##    
##  OUTPUT
##    Vector counting number of row/column-wise 
##    NA entries  
##
na.count <- function(data, MARGIN = 2) {  # nolint
  return(apply(
    data, MARGIN = MARGIN,
    function(x) {
      sum(is.na(x))
      }))
}
  
#################################################
#################################################
## na.ignore:
##    Auxiliary function.
##    Given a dataset and an index, it extracts the
##    vector of values of the column or row 
##    associated with the index, removing the NA 
##    values.
##
##  INPUTS
##    data    : data matrix.
##    MARGIN  : If 1, extracts rows.
##    index   : index of row/column to extract.
##    
##  OUTPUT
##    .x      : vector.
## 
na.ignore <- function(data, MARGIN = 2, index = 1) { # nolint
  .x <- NULL
  if (MARGIN == 2) {
    .x <- data[, index]
    .x <- .x[!is.na(.x)]
  } else if (MARGIN == 1) {
    .x <- data[index, ]
    .x <- .x[!is.na(.x)]      
  }
  return(.x)
}
  

#################################################
#################################################
##   plot.isna:
##    Function for plotting where missing values 
##    in a data matrix.
##
##  INPUTS
##    data  : data matrix.
##    main  : title of plot.
##    cex   : size of points in plot.
##    
##  OUTPUT
##    .x      : vector.
##
plot.isna <- function(data, main, cex = 0.1) {
  .d1 <- dim(data)[1]
  .d2 <- dim(data)[2]
    
  plot(
    1, 1,
    col = "white",
    xlim = c(0, .d2 + 1),
    ylim = c(0, .d1 + 1),
    xlab = "Variable index",
    ylab = "Observation index",
    main = main)
  rect(
    xleft = 0,
    ybottom = 0,
    xright = .d2 + 1,
    ytop = .d1 + 1,
    col = "green")
    
  .isna_entry <- which(is.na(data), arr.ind = TRUE)
    
  points(
    x = .isna_entry[, 2],
    y = .isna_entry[, 1],
    col = "red",
    cex = cex)
}

 
################################################################
################################################################
## FUNCTIONS FOR DETECTING/VISUALIZING INFLUENCE WEIGHT CURVES:

#################################################
#################################################
##  plot.integrals:
##    Function that generates oracle influence measure
##    plots, coloring hubs and non-hubs differently.
##
##  INPUTS
##    eig   : object eigen(M) of a covariance/correlation 
##              matrix M.
##    args  : object generated by CreateParameters(), which
##              matches the parameters associated to the 
##              matrix M.
##    main  : title of plot.
##    
##  OUTPUT
##    .inf_meas_curve :
##            Matrix where ith-row corresponds to influence
##            measure curve for i-th variable in the matrix M.
plot.integrals <- function(eig, args, main) {
  .inf_meas_curve <- matrix(rep(0, args$p * args$p), ncol = args$p)
  .inf_meas_curve[, 1] <- eig$vectors[, args$p]^2
  for(.i in 2:args$p){
    .inf_meas_curve[, .i] <- .inf_meas_curve[, .i - 1] + eig$vectors[, args$p - .i + 1]^2
  }
  .weights <- apply(.inf_meas_curve[, 1:args$threshold],
                   MARGIN = 1,
                   sum)
  skew <- skewness(.weights)
  ## Plot of integrals
  plot(c(0, 0), col = "white",
       xlim = c(0, args$p),
       ylim = c(0, 1),
       main = main,
       xlab = "No. of Eigenvectors",
       ylab = "Influence Measure Value")
  for (.j in args$p:1) {
    if (.j < args$r+1) lines(x = 1:args$p, y = .inf_meas_curve[.j, ], col = cbPalette[1])
    if (.j > args$r) lines(x = 1:args$p, y = .inf_meas_curve[.j, ], col = cbPalette[2])
  }
  abline(v = args$threshold, col = "red")

  return(.inf_meas_curve)
}



################################################################
################################################################
## FUNCTIONS FOR PLOTTING MATRICES:

#################################################
#################################################
##  plot.pdm:
##    Function that generates a plot of a square
##    matrix.
##
##  INPUTS
##    pd          : square matrix to plot. 
##    main        : title of the plot.
##    breaks      : number of colors in the gradient
##                    used for plotting.
##    diagonal    : if FALSE, plot only displays 
##                    non-diagonal entries.
##    cex         : size of points in the plot.
##    entryrange  : if NULL, calculated automatically.
##                    Otherwise, bound on the 
##                    absolute values of entries plotted.
##    
##  OUTPUT:
##    Color plot of matrix with gradient color from blue to red
##    indicating sign and magnitude of entries.
##
plot.pdm <- function(pd,
                    main,
                    breaks = 20,
                    diagonal = FALSE,
                    cex = 0.05,
                    entryrange = NULL) {
  .d <- dim(pd)[1]

  if (is.null(entryrange)) {
    entryrange <- 0
    if (diagonal) {
      entryrange <- max(abs(pd))
    } else {
      for (.i in 1:.d) {
        for (.j in (1:.d)[-.i]) {
          entryrange <- max(entryrange, abs(pd[.i, .j]))
        }
      }
    }
  }


  rbPal <- colorRampPalette(c("red", "white", "blue"))
  color <- rbPal(breaks)
  plot(1, 1,
       col = "white",
       xlim = c(1, .d),
       ylim = c(1, .d),
       xlab = "Row Index",
       ylab = "Column Index",
       main = main)

  for (.i in 1:.d) {
    if (diagonal) {
      for (.j in 1:.d) {
        colorindex <- floor(
          breaks * (pd[.i, .j] + entryrange) / (2 * entryrange) + 0.99)
        points(x = c(.j), y = c(.d - .i + 1), col = color[colorindex],
               pch = 19, cex = cex)
      }
    } else {
      for (.j in (1:.d)[-.i]) {
        colorindex <- floor(
          breaks * (pd[.i, .j] + entryrange) / (2 * entryrange) + 0.99)
        points(x = c(.j), y = c(.d - .i + 1), col = color[colorindex],
               pch = 19, cex = cex)
      }
    }
  }
}


#################################################
#################################################
##  multiplot:
##    Generates mutiplots for ggplot2.
##
##  INPUTS
##    plotlist    : list of ggplo2 plots to include.
##    file        : name of file to save plot.
##    cols        : number of columns in multiplots.
##    layout      : if no cols provided, layout for plots.
##    
##  OUTPUT:
##    Printed multiplot with provided ggplot2 plots
##    in the stated layout.
##
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
