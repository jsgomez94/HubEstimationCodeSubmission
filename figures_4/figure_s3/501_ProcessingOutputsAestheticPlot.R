cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7")

for(.i in c(1:42)){
    load(paste0("100_ScreeningSimulations/outputs1/output",.i,".RData"))
}


############################################
############################################
############################################
############################################
## Loading simulation data:
wd <- getwd()
req_lib <- paste0(wd, "/req_lib")
.libPaths(req_lib)
library(ggplot2)
library(ggpubr)

varnames = c("q", "n",
             "AUC", "AUC.sd", "AUC.upper", "AUC.lower", 
             "TPeff", "TPeff.sd", "TPeff.upper", "TPeff.lower",
             "TPhubs", "TPhubs.sd", "TPhubs.upper", "TPhubs.lower")
output = matrix(0, ncol = 14, nrow = 18)
colnames(output) = varnames
output = as.data.frame(output)

############################################
############################################
## Processing simulation data:
count = 0
method = 1
for(.i in setdiff(1:21, c(1,8,15))){
  count = count + 1
  .output = get(x = paste0("output", .i))
  .args = get(x = paste0("args", .i))
  
  output$q[count] = .args$q
  output$n[count] = as.character(.args$n)
  output$AUC[count] = .output[method, 1]
  output$AUC.sd[count] = .output[method, 2]
  output$AUC.upper[count] = output$AUC[count] + output$AUC.sd[count]
  output$AUC.lower[count] = output$AUC[count] - output$AUC.sd[count]
  
  output$TPeff[count] = .output[method, 3]
  output$TPeff.sd[count] = .output[method, 4]
  output$TPeff.upper[count] = output$TPeff[count] + output$TPeff.sd[count]
  output$TPeff.lower[count] = output$TPeff[count] - output$TPeff.sd[count]
  
  output$TPhubs[count] = .output[method, 5]
  output$TPhubs.sd[count] = .output[method, 6]
  output$TPhubs.upper[count] = min(output$TPhubs[count] + output$TPhubs.sd[count], 1.005)
  output$TPhubs.lower[count] = output$TPhubs[count] - output$TPhubs.sd[count]
}
#View(output)

############################################
## Generating plots:
AUC.plot <- ggplot(data=output, aes(x=q, y= AUC, colour=n)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(trans='log10') +
  ylim(0.5,  1.05) + 
  labs(title="AUC",
       x = "q size", y = "AUC")
AUC.plot <- AUC.plot + geom_ribbon(aes(ymin=AUC.lower, 
                                       ymax=AUC.upper,
                                       col = n), 
                                   linetype=2, alpha=0.1)

TPeff.plot <- ggplot(data=output, aes(x=q, y= TPeff, colour=n)) + 
  geom_point() + 
  geom_line() +
  scale_x_continuous(trans='log10') +
  ylim(0.5,  1.05)+ 
  labs(title="TPR of effective variables",
       x = "Total dimension", y = "TPR of Effective variables")
TPeff.plot <- TPeff.plot + geom_ribbon(aes(ymin=TPeff.lower, 
                                           ymax=TPeff.upper,
                                           col = n), 
                                       linetype=2, alpha=0.1)

TPhubs.plot <- ggplot(data=output, aes(x=q, y= TPhubs, colour=n)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(trans='log10') +
  ylim(0.5, 1.05) + 
  labs(title="TPR of hubs",
       x ="Total dimension ", y = "TPR of hubs")
TPhubs.plot <- TPhubs.plot + geom_ribbon(aes(ymin=TPhubs.lower, 
                                             ymax=TPhubs.upper,
                                             col = n), 
                                         linetype=2, alpha=0.1)

############################################
## Formatting plots:
screening.plot <- ggarrange(TPeff.plot, TPhubs.plot,
                           ncol = 2, nrow = 1,
                           common.legend = TRUE, legend = "bottom")

screening.plot <- annotate_figure(screening.plot,
                top = text_grob("\n Screening effectiveness vs total dimension \n", color = "black", face = "bold", size = 16))

pdf("006_FigS3.pdf", width = 9, height = 4.5)
print(screening.plot)
dev.off()

