
library(gaston)
prefix <- "/Volumes/GoogleDrive/My Drive/UMASS/simulations.v3.hpc/"
prefix_plot <- "/Volumes/GoogleDrive/My Drive/UMASS/simulations.v3.hpc/plots/"

pvalues_bfi <- read.csv(paste0(prefix, "pvalues_backdoor_frontdoor_iv_1000.csv"))
pvalues_bf <- read.csv(paste0(prefix, "pvalues_backdoor_frontdoor_1000.csv"))
pvalues_bi <- read.csv(paste0(prefix, "pvalues_backdoor_iv_1000.csv"))
pvalues_fi <- read.csv(paste0(prefix, "pvalues_frontdoor_iv_1000.csv"))



bfi_title <- c("QQ plot of p-values (Backdoor: TRUE, Frontdoor: TRUE, IV: TRUE)",
               "QQ plot of p-values (Backdoor: TRUE, Frontdoor: f3FALSE, IV: TRUE)",
               "QQ plot of p-values (Backdoor: TRUE, Frontdoor: TRUE, IV: i3FALSE)",
               "QQ plot of p-values (Backdoor: b1FALSE, Frontdoor: TRUE, IV: TRUE)",
               "QQ plot of p-values (Backdoor: b1FALSE, Frontdoor: TRUE, IV: i1FALSE)",
               "QQ plot of p-values (Backdoor: b1FALSE, Frontdoor: f2FALSE f3FALSE, IV: TRUE)",
               "QQ plot of p-values (Backdoor: TRUE, Frontdoor: f3FALSE, IV: i3FALSE)")

bf_title <- c("QQ plot of p-values (Backdoor: TRUE, Frontdoor: TRUE)",
              "QQ plot of p-values (Backdoor: TRUE, Frontdoor: f1FALSE)",
              "QQ plot of p-values (Backdoor: TRUE, Frontdoor: f3FALSE)",
              "QQ plot of p-values (Backdoor: b1FALSE, Frontdoor: TRUE)")

bi_title <- c("QQ plot of p-values (Backdoor: TRUE, IV: TRUE)",
              "QQ plot of p-values (Backdoor: TRUE, IV: i3FALSE)",
              "QQ plot of p-values (Backdoor: b2FALSE, IV: TRUE)",
              "QQ plot of p-values (Backdoor: b1FALSE, IV: TRUE)")
               
fi_title <- c("QQ plot of p-values (Frontdoor: TRUE, IV: TRUE)",
              "QQ plot of p-values (Frontdoor: TRUE, IV: i1FALSE)",
              "QQ plot of p-values (Frontdoor: TRUE, IV: i2FALSE)",
              "QQ plot of p-values (Frontdoor: TRUE, IV: i3FALSE)",
              "QQ plot of p-values (Frontdoor: f1FALSE, IV: TRUE)",
              "QQ plot of p-values (Frontdoor: f3FALSE, IV: TRUE)")


for (i in 2:ncol(pvalues_bfi)) {
  png(paste0(prefix_plot, colnames(pvalues_bfi)[i], ".png") )
  qqplot.pvalues(pvalues_bfi[,i], pch = ".", cex = 2, thinning = FALSE, 
                 main = bfi_title[i-1], cex.main = 0.9)
  dev.off()
}

for (i in 2:ncol(pvalues_bf)) {
  png(paste0(prefix_plot, colnames(pvalues_bf)[i], ".png") )
  qqplot.pvalues(pvalues_bf[,i], pch = ".", cex = 2, thinning = FALSE, 
                 main = bf_title[i-1], cex.main = 0.9)
  dev.off()
}

for (i in 2:ncol(pvalues_bi)) {
  png(paste0(prefix_plot, colnames(pvalues_bi)[i], ".png") )
  qqplot.pvalues(pvalues_bi[,i], pch = ".", cex = 2, thinning = FALSE, 
                 main = bi_title[i-1], cex.main = 0.9)
  dev.off()
}

for (i in 2:ncol(pvalues_fi)) {
  png(paste0(prefix_plot, colnames(pvalues_fi)[i], ".png") )
  qqplot.pvalues(pvalues_fi[,i], pch = ".", cex = 2, thinning = FALSE, 
                 main = fi_title[i-1], cex.main = 0.9)
  dev.off()
}
