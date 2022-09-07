require(matrixStats)
library(beanplot)
library(vioplot)

setwd("E:/6_clean_version_new/6_clean_version/99_outputs")
#png(file="boxplot.png", res = 300, width=7, height=5, units = 'in')
tiff(filename = "beanplot.tif", width = 16, height = 11, units = "cm", pointsize = 15, compression = "lzw", bg = "white", res = 300)
#tiff(filename = "violot.tif", width = 16, height = 11, units = "cm", pointsize = 15, compression = "lzw", bg = "white", res = 300)

# save model results to file
load("all_preds_extracted_plot100.RData")
load("all_preds_extracted_plot300.RData")
load("all_preds_extracted_plot500.RData")
beanplot(all_preds_100[,1],all_preds_300[,1],all_preds[,1], ylab="Biomass [t/ha]", xlab="Plot size", cex.axis=0.7, cex.lab=0.7)
#vioplot(all_preds_100[,1],all_preds_300[,1],all_preds[,1], ylab="Biomass [t/ha]", xlab="Plot size", cex.axis=0.7, cex.lab=0.7)
axis(1, at=1:3, labels=c("100 m²", "300 m²", "500 m²"), cex.axis=0.7, cex.lab=0.7)
dev.off()


# save model results to file
#load("all_preds_extracted_plot100.RData")
#load("all_preds_extracted_plot300.RData")
#load("all_preds_extracted_plot500.RData")
#boxplot(all_preds_100[,1],all_preds_300[,1],all_preds[,1], ylab="Biomass [t/ha]", xlab="Plot size", cex.axis=0.7, cex.lab=0.7)

#axis(1, at=1:3, labels=c("100 m²", "300 m²", "500 m²"), cex.axis=0.7, cex.lab=0.7)
#dev.off()

#setwd("E:/6_clean_version_new/6_clean_version/99_outputs")
#png(file=" rang boxplot.png", res = 300, width=7, height=5, units = 'in')

# save model results to file
#load("fixed_area_results_100m.RData")
#load("fixed_area_results_300m.RData")
#load("fixed_area_results_500m.RData")

#boxplot(res_fixed_100[,16]-res_fixed_100[,15],res_fixed_300[,16]-res_fixed_300[,15],res_fixed_500[,16]-res_fixed_500[,15], ylab="Biomass [t/ha]", xlab="plot size", cex.axis=1, cex.lab=1)

#axis(1, at=1:3, labels=c("100 m²", "300 m²", "500 m²"), cex.axis=1, cex.lab=1)
#dev.off()
