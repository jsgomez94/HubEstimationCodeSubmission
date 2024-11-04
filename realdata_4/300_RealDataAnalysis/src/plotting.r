# ################################################################
# ################################################################
## PLOTTING FUNCTIONS:

## Colorblind friendly palette:
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
plot.pdm <- function( # nolint
  data, main, breaks = 21, diagonal = FALSE,
  cex = 0.05, entryrange = NULL) {
  .d <- dim(data)[1]

  if (is.null(entryrange)) {
    entryrange <- 0
    if (diagonal) {
      entryrange <- max(abs(data))
    } else {
      for (.i in 1:.d) {
        for (.j in (1:.d)[-.i]) {
          entryrange <- max(entryrange, abs(data[.i, .j]))
        }
      }
    }
  }

  rb_pal <- colorRampPalette(c("red", "gray95", "blue"))
  color <- rb_pal(breaks)
  plot(1, 1,
       col = "white",
       xlim = c(1, .d),
       ylim = c(1, .d),
       xlab = "Row Index",
       ylab = "Column Index",
       xaxt = "none",
       yaxt = "none",
       main = main)
  axis(2, at = c(1, seq(from = 0, to = .d, length.out = 6)[-1]),
       labels = c(seq(from = .d, to = 0, length.out = 6)[-6], 1))
  axis(1, at = c(1, seq(from = 0, to = .d, length.out = 6)[-1]),
       labels = c(1, seq(from = 0, to = .d, length.out = 6)[-1]))
  
  for (.i in 1:.d) {
    if (diagonal) {
      for (.j in 1:.d) {
        colorindex <- floor( 
          breaks * (data[.i, .j] + entryrange) / (2 * entryrange) + 0.99
        )
        points(x = c(.j), y = c(.d - .i + 1), col = color[colorindex],
               pch = 19, cex = cex)
      }
    } else {
      for (.j in (1:.d)[-.i]) {
        colorindex <- floor( 
          breaks * (data[.i, .j] + entryrange) / (2 * entryrange) + 0.99
        )
        points(x = c(.j), y = c(.d - .i + 1), col = color[colorindex],
               pch = 19, cex = cex)
      }
    }
  }
}




#################################################
#################################################
##  plot.multiclass_density:
##    Function that plots densities when data is
##    a mixture of multiple classes.
##
##  INPUTS
##    x           : vector of data.
##    class       : vector of same length as x, containing
##                    the data class labels.
##    main, xlab, ylab : graphical parameters.
##    
##  OUTPUT:
##    Plot of mixture densities.
##
plot.multiclass_density <- function(x, class, main = "", xlab = "", ylab = ""){
  
  plot( 
    density(x),
    xlim = c(min(x),max(x)),
    main = main,
    col = "black",
    xlab = xlab, ylab = ylab)
  
  
  n <- length(class)
  n_class <- length(unique(class))
  cols <- character(length(class))
  for(i in 1:n_class){
    i_class <- unique(class)[i]
    class_prop = sum(class == i_class) / n
    dens = density(x[class == i_class])
    lines(x = dens$x, y = dens$y * class_prop,
          col = cbPalette[i])
  }
  return(1)
}
