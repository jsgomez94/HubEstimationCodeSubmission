##############################################
##############################################

source("001_requirements.R")
source("002_GeneratingMatrixSparse.R")
source("003_BasicMatrixTransformations.R")

## Colorblind-friendly palette:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")

##############################################
##############################################
## Generating Precision Matrix:
p=100
r = 5
set.seed(20)
pm = r.sparse.pdhubmat(
  p = p, T0 = p-1, ph = 0.9, pnh = 0.025, pneff = 0.025, r = r,
  diagonal_shift = 2, shuffle = FALSE, 
  type = "unif", 
  hmin = 4, hmax = 5,
  nhmin = 4, nhmax = 5,
  neffmin = 4, neffmax = 5)

ic = .PMtoIC(pm)
eig.ic = eigen(ic)


##############################################
##############################################
# PLOT 1: EIGENVALUE RATIOS PLOT

## Calculating consecutive eigenvalue ratios:
data = NULL
half = floor(p/2)
gaps = eig.ic$values[1:half]/eig.ic$values[2:(half + 1)]
data = cbind(1200, gaps)

data2 = matrix(0, nrow = half, ncol = 90)

for(.i in 1:90){
  .n = p + 10*.i
  .X = rmvnorm(n = .n, sigma = solve(pm))
  .RHat = cor(.X)
  
  .icHat = solve(.RHat)
  .eig.icHat = eigen(.icHat)
  
  .gaps = .eig.icHat$values[1:half]/.eig.icHat$values[2:(half + 1)]
  
  .data = cbind(.n, .gaps)
  data = rbind(data, .data)
  
  data2[, .i] = .gaps
  print(.i)
}

## Generating plot:
file_name <- "005_figS2.pdf"
pdf(file_name, width = 9.5, height = 4.5)
par(mfrow = c(1,3),
    oma = c(0,0,4,0),
    mar = c(5.1, 4.5, 4.1, 2.1))
plot(x= p + 10*(1:100), y= 1:100,col = "white",
     bty = "L", xlim = c(100,1150),ylim = c(0.75,3.1),
     main = "Eigenvalue ratios", cex.main = 2,
     xlab = "",
     ylab = "Ratios",
     cex.lab = 1.5)
for(.val in 1:half){
  if(.val < r){
    lines(x = p + 10*(1:90), y = data2[.val,],
          col = cbPalette[1])
  } else if(.val == r){
    lines(x = p + 10*(1:90), y = data2[.val,],
          col = cbPalette[7])
  } else{
    lines(x = p + 10*(1:90), y = data2[.val,],
          col = cbPalette[1])
  }
}
abline(v = 1025)
points(x = rep(1050, half), y = gaps, pch = 19, cex = 1,
       col = rep(c(cbPalette[1],cbPalette[7],cbPalette[1]),
                 c(r-1,1, p-r)))
text(labels = "Pop.", x = 1025, y= 3.05, pos = 4, cex = 1.2)
text(labels = "Emp.", x = 1020, y= 3.05, pos = 2, cex = 1.2)

legend(x = "topleft",
       legend = c("s-th ratio",
                  "Other ratios"),
       col = cbPalette[c(7,1)], lty = 19, cex = 1)


##############################################
##############################################
# PLOT 2: INFLUENCE MEASURES WITH TRUE GAP S = 5

## Influence measures with r = 5 eigenvectors.
pop.weights = apply(eig.ic$vectors[, 1:r]^2,
                    MARGIN = 1,
                    sum) 

eigenweights = matrix(0, ncol = 90, nrow = p)
for(.i in 1:90){
  .n = p + 10*.i
  .X = rmvnorm(n = .n, sigma = solve(pm))
  .RHat = cor(.X)
  
  .icHat = solve(.RHat)
  eig.icHat = eigen(.icHat)
  
  .weights = apply(eig.icHat$vectors[, 1:r]^2,
                   MARGIN = 1,
                   sum)
  eigenweights[, .i] = .weights
  print(.i)
}

## Generating plot:
plot(x= p + 10*(1:100), y= 1:100,col = "white",
     bty = "L", xlim = c(100,1150),ylim = c(0,1),
     main = "Weight", cex.main = 2,
     xlab = "Sample size",
     ylab = "omega-weights",
     cex.lab = 1.5)
for(.val in p:1){
  if(.val < r + 1){
    lines(x = p + 10*(1:90), y = eigenweights[.val,],
          col = cbPalette[7])
  } else{
    lines(x = p + 10*(1:90), y = eigenweights[.val,],
          col = cbPalette[6])
  }
}
abline(v = 1025)
points(x = rep(1050, p), y = pop.weights, pch = 19, cex = 1,
       col = rep(c(cbPalette[7],cbPalette[6]),
                 c(r, p-r)))
text(labels = "Pop.", x = 1025, y= 0.95, pos = 4, cex = 1.2)
text(labels = "Emp.", x = 1020, y= 0.95, pos = 2, cex = 1.2)

legend(x = "topleft",
       legend = c("Hub variables","Non-hub variables"),
       col = cbPalette[7:6], lty = 19, cex = 1)
####################
mtext(text = "IPC-HD method vs. sample size",
      side = 3, outer = TRUE, line = 0, cex = 2)


##############################################
##############################################
# PLOT 3: INFLUENCE MEASURES WITH OVERESTIMATED
# GAP S = 10

## Influence measures with 2r = 10 eigenvectors.
pop.weights = apply(eig.ic$vectors[, 1:(2*r)]^2,
                    MARGIN = 1,
                    sum) 

eigenweights = matrix(0, ncol = 90, nrow = p)
for(.i in 1:90){
  .n = p + 10*.i
  .X = rmvnorm(n = .n, sigma = solve(pm))
  .RHat = cor(.X)
  
  .icHat = solve(.RHat)
  eig.icHat = eigen(.icHat)
  
  .weights = apply(eig.icHat$vectors[, 1:(20)]^2,
                   MARGIN = 1,
                   sum)
  eigenweights[, .i] = .weights
  print(.i)
}

## Generating plot:
plot(x= p + 10*(1:100), y= 1:100,col = "white",
     bty = "L", xlim = c(100,1150),ylim = c(0,1),
     main = "Over-estimated weight", cex.main = 2,
     xlab = "",
     ylab = "omega-weights",
     cex.lab = 1.5)
for(.val in 1:p){
  if(.val < r + 1){
    lines(x = p + 10*(1:90), y = eigenweights[.val,],
          col = cbPalette[7])
  } else{
    lines(x = p + 10*(1:90), y = eigenweights[.val,],
          col = cbPalette[6])
  }
}
abline(v = 1025)
points(x = rep(1050, p), y = pop.weights, pch = 19, cex = 1,
       col = rep(c(cbPalette[7],cbPalette[6]),
                 c(r, p-r)))
text(labels = "Pop.", x = 1025, y= 0.95, pos = 4, cex = 1.2)
text(labels = "Emp.", x = 1020, y= 0.95, pos = 2, cex = 1.2)

legend(x = "topleft",
       legend = c("Hub variables","Non-hub variables"),
       col = cbPalette[c(7,6)], lty = 19, cex = 1)

dev.off()
