
source("002_GeneratingMatrixSparse.R")
source("004_ModulePlotting.R")


#################################################
#################################################
## Figure 1:
##    Sparse precision matrix with a hub.
##    Visualization of eigenvalues/top eivenvector.
p = 100
set.seed(5)
file_name <- "006_fig1.pdf"
pdf(file_name, width = 8, height = 3)
pm = r.sparse.pdhubmat(
      p = p, T = p, r = 1, ph = 0.6, pnh = 0.05, pneff = 0,
      diagonal_shift = 2, shuffle = TRUE, type = "unif",
      hmin = 4, hmax = 5,
      nhmin = 4, nhmax = 5)
hub = which.max(apply(abs(pm), MARGIN = 1, sum))
eig = eigen(pm)

par(mfrow = c(1,3))
plot.pdm(
      pd = pm,main = NULL,
      breaks = 20, diagonal = FALSE,
      cex = 0.2, entryrange = NULL)
mtext(
      text = "Precision matrix",
      outer = FALSE, cex = 1.5, line = 1)
plot(
      eig$values,
     pch = 19, cex = 1,
     xlab = "Index", ylab = "Eigenvalue")
abline(h = 0)

mtext(
      text = "Ordered Eigenvalues",
      outer = FALSE, cex = 1.5, line = 1)
plot(
      eig$vectors[,1], 
     pch = 19, cex = 1,
     ylim = c(-max(abs(eig$vectors[,1])), max(abs(eig$vectors[,1]))),
     xlab = "Variables", ylab = "Eigenvector entry")
abline(h = 0)
abline(v = hub, col = "red", lty = 2)  
mtext(
      text = "1st Eigenvector",
      outer = FALSE, cex = 1.5, line = 1)
dev.off()


#################################################
#################################################
## Figure 2:
##    Dense precision matrix with a hub.
##    Visualization of eigenvalues/top eivenvector.
p = 100
file_name <- "006_fig2.pdf"
pdf(file_name, width = 8, height = 3)
pm = r.sparse.pdhubmat(
      p = p, T = p, r = 1, ph = 0.6, pnh = 0.6, pneff = 0,
      diagonal_shift = 2, shuffle = TRUE, type = "unif",
      hmin = 20, hmax = 25, 
      nhmin = 4, nhmax = 5)
hub = which.max(apply(abs(pm), MARGIN = 1, sum))
eig = eigen(pm)

par(mfrow = c(1,3))
plot.pdm(
      pd = pm, 
      main = NULL, breaks = 20,  diagonal = FALSE,
      cex = 0.2, entryrange = NULL)
mtext(
      text = "Precision matrix",
      outer = FALSE, cex = 1.5, line = 1)
plot(
      eig$values, 
     pch = 19,  cex = 1,
     xlab = "Index", ylab = "Eigenvalue")
abline(h = 0)
mtext(
      text = "Ordered Eigenvalues",
      outer = FALSE, cex = 1.5, line = 1)
plot(
      eig$vectors[,1], 
     pch = 19, cex = 1,
     ylim = c(-max(abs(eig$vectors[,1])), max(abs(eig$vectors[,1]))),
     xlab = "Variables", ylab = "Eigenvector entry")
abline(h = 0)
abline(v = hub, col = "red", lty = 2)  
mtext(
      text = "1st Eigenvector",
      outer = FALSE, cex = 1.5, line = 1)
dev.off()