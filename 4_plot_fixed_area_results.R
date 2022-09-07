require(matrixStats)

setwd("E:/6_clean_version_new/6_clean_version/99_outputs")

# save model results to file
load("fixed_area_results_100m.RData")
load("fixed_area_results_300m.RData")
load("fixed_area_results_500m.RData")


###################
#####   100
###################

# extract results
means_100 <- list()
sds_100 <- list()
dummy2=1

# get the examined sample sizes
vals <- unique(res_fixed_100[,1])

# iterate through the results matrix and extract the means for the 1000 runs
# of each sample size
for (i3 in vals){
  
  # select subset of results depending on sample size
  sub <- res_fixed_100[res_fixed_100[,1]==i3,]
  # calculate mean
  means_100[[dummy2]] <- colMeans(sub)
  sds_100[[dummy2]] <- colSds(sub)
  # increase dummy variable by 1
  dummy2 = dummy2+1
  
}

means_100_fin <- do.call(rbind, means_100)
sds_100_fin <- do.call(rbind, sds_100)

colnames(means_100_fin) <- c("sample_size", "iteration", "fixed area", "plot_size", "spearm_rf", "spearm_val", "rmse_rf", "rmse_val", "Rrmse_rf", "Rrmse_val", "Nrmse_rf", "Nrmse_val", "bias_rf", "bias_val","max", "min","max_val", "min_val")
colnames(sds_100_fin) <- c("sample_size", "iteration", "fixed area", "plot_size", "spearm_rf", "spearm_val", "rmse_rf", "rmse_val", "Rrmse_rf", "Rrmse_val", "Nrmse_rf", "Nrmse_val", "bias_rf", "bias_val","max", "min","max_val", "min_val")



###################
#####   300
###################

# extract results
means_300 <- list()
sds_300 <- list()
dummy2=1

# get the examined sample sizes
vals <- unique(res_fixed_300[,1])

# iterate through the results matrix and extract the means for the 1000 runs
# of each sample size
for (i3 in vals){
  
  # select subset of results depending on sample size
  sub <- res_fixed_300[res_fixed_300[,1]==i3,]
  # calculate mean
  means_300[[dummy2]] <- colMeans(sub)
  sds_300[[dummy2]] <- colSds(sub)
  # increase dummy variable by 1
  dummy2 = dummy2+1
  
}

means_300_fin <- do.call(rbind, means_300)
sds_300_fin <- do.call(rbind, sds_300)

colnames(means_300_fin) <- c("sample_size", "iteration", "fixed area", "plot_size", "spearm_rf", "spearm_val", "rmse_rf", "rmse_val", "Rrmse_rf", "Rrmse_val", "Nrmse_rf", "Nrmse_val", "bias_rf", "bias_val","max", "min","max_val", "min_val")
colnames(sds_300_fin) <- c("sample_size", "iteration", "fixed area", "plot_size", "spearm_rf", "spearm_val", "rmse_rf", "rmse_val", "Rrmse_rf", "Rrmse_val", "Nrmse_rf", "Nrmse_val", "bias_rf", "bias_val","max", "min","max_val", "min_val")



###################
#####   500
###################

# extract results
means_500 <- list()
sds_500 <- list()
dummy2=1

# get the examined sample sizes
vals <- unique(res_fixed_500[,1])

# iterate through the results matrix and extract the means for the 1000 runs
# of each sample size
for (i3 in vals){
  
  # select subset of results depending on sample size
  sub <- res_fixed_500[res_fixed_500[,1]==i3,]
  # calculate mean
  means_500[[dummy2]] <- colMeans(sub)
  sds_500[[dummy2]] <- colSds(sub)
  # increase dummy variable by 1
  dummy2 = dummy2+1
  
}

means_500_fin <- do.call(rbind, means_500)
sds_500_fin <- do.call(rbind, sds_500)

colnames(means_500_fin) <- c("sample_size", "iteration", "fixed area", "plot_size", "spearm_rf", "spearm_val", "rmse_rf", "rmse_val", "Rrmse_rf", "Rrmse_val", "Nrmse_rf", "Nrmse_val", "bias_rf", "bias_val","max", "min","max_val", "min_val")
colnames(sds_500_fin) <- c("sample_size", "iteration", "fixed area", "plot_size", "spearm_rf", "spearm_val", "rmse_rf", "rmse_val", "Rrmse_rf", "Rrmse_val", "Nrmse_rf", "Nrmse_val", "bias_rf", "bias_val","max", "min","max_val", "min_val")



####################################################################################
## plot mean and standard deviation of spearman of random forest regression results
####################################################################################

# tar 1 = "sample_size" 
# tar 2 = "iteration" 
# tar 3 = "spearm_rf" 
# tar 4 = "spearm_svm" 
# tar 5 = "rmse_rf" 
# tar 6 = "rmse_svm" 
# tar 7 = "Rrmse_rf" 
# tar 8 = "Rrmse_svm" 
# tar 9 = "bias_rf" 
# tar 10 = "bias_svm"

names <- c("sample_size", "iteration", "fixed area", "plot_size", "spearm_rf", "spearm_val", "rmse_rf", "rmse_val", "Rrmse_rf", "Rrmse_val", "Nrmse_rf", "Nrmse_val", "bias_rf", "bias_val","max", "min","max_val", "min_val")
setwd("E:/6_clean_version_new/6_clean_version/outputs")
#tiff(filename = "fixed area results_tr.tif", width = 20, height = 35, units = "cm", pointsize = 18, compression = "lzw", bg = "white", res = 300)
tiff(filename = "fixed area results_val.tif", width = 20, height = 35, units = "cm", pointsize = 18, compression = "lzw", bg = "white", res = 300)
#par(cex=2)
par(mfrow=c(3,1), mar=c(4,5,1.8,1))
#png(file="fixed area results_tr.png", res = 300, width=17, height=4, units = 'in')
#png(file="fixed area results_NEW.png", res = 300, width=7, height=16, units = 'in')
#png(file="fixed area results_val.png", res = 300, width=17, height=4, units = 'in')
#par(oma = c(1, 1, 1, 7))
#pdf(file="Fixed area results.pdf", width=19, height=5)
#par(mfrow=c(1,3), mar=c(3, 3, 2.5,0), oma = c(1, 1, 1, 7))
#par(mfrow=c(3,1))
#for (i in 1:3){
  #par(mar=c(5.1, 4.1, 4.1,9.5))
  #plot(runif(3),runif(3),ylab="",xlab="")
#}

tar = 6
yl = c(0,1)
per_met = names[tar]

### 100 plot
###
meanr2_100 <-  means_100_fin[,tar]
sdr2_100 <- sds_100_fin[,tar]
x <- c(seq(6000,18000,1500), rev(seq(6000,18000,1500)))
y1 <- meanr2_100+sdr2_100
y2 <-meanr2_100-sdr2_100
y <- c(y1, rev(y2)) 
# empty plot to set the graph
plot(meanr2_100, col="white", xlim=c(5500,18500), ylim=yl,ann=F, axes=F)
# plot the polygon
polygon(x,y, border=NA, col=rgb(0.0, 0.0, 0.1, 0.2), ylim=c(-20,20))
# plot results
par(new=T)
plot(x[1:9], meanr2_100, xlim=c(5500,18500), ylim=yl, pch=1,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F)


### 300 plot
###
meanr2_300 <-  means_300_fin[,tar]
sdr2_300 <- sds_300_fin[,tar]
#x <- c(seq(6000,18000,1500), rev(seq(6000,18000,1500)))
x <- c(6000,7500,12000,15000,15000,12000,7500,6000)
y1 <- meanr2_300+sdr2_300
y2 <-meanr2_300-sdr2_300
y <- c(y1, rev(y2)) 
par(new=T)
plot(meanr2_300, col="white", xlim=c(5500,18500), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.0, 0.4, 0.1, 0.2), ylim=yl)
par(new=T)
plot(x[1:4], meanr2_300,xlim=c(5500,18500), ylim=yl, pch=2,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F)


### 500 plot
### 

meanr2_500 <-  means_500_fin[,tar]
sdr2_500 <- sds_500_fin[,tar]
#x <- c(seq(6000,18000,1500), rev(seq(6000,18000,1500)))
x <- c(6000,7500,13500,13500,7500,6000)
y1 <- meanr2_500+sdr2_500
y2 <-meanr2_500-sdr2_500
y <- c(y1, rev(y2)) 
par(new=T)
plot(meanr2_500, col="white",xlim=c(5500,18500), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.4, 0.4, 0.1, 0.2), ylim=yl)
par(new=T)
plot(x[1:3], meanr2_500,ylab="Spearman cor.", xlab="Fixed area size", xlim=c(5500,18500), ylim=yl, pch=3,cex.lab=1.6, cex.axis=1.6)

#legend(x = 16000, y = 0.2, c("Plot size 100","Plot size 300", "Plot size 500"), pch=c(1,2,3), c("Var. Plot size 100","Var. Plot size 300", "Var. Plot size 500"), lwd = 10, col=c(rgb(0.0, 0.0, 0.1, 0.2),rgb(0.0, 0.4, 0.1, 0.2), rgb(0.4, 0.4, 0.1, 0.2)))
#legend("topright", c("Var. Plot size 100","Var. Plot size 300", "Var. Plot size 500"), lwd = 10, col=c(rgb(0.0, 0.0, 0.1, 0.2),rgb(0.0, 0.4, 0.1, 0.2), rgb(0.4, 0.4, 0.1, 0.2)))

# ## rmse

#tar = 7
#yl = c(0,80)
#per_met = names[tar]


### 100 plot
###
#meanrmse_100 <-  means_100_fin[,tar]
#sdrmse_100 <- sds_100_fin[,tar]
#x <- c(seq(6000,18000,1500), rev(seq(6000,18000,1500)))
#y1 <- meanrmse_100+sdrmse_100
#y2 <-meanrmse_100-sdrmse_100
#y <- c(y1, rev(y2)) 
#plot(meanrmse_100, col="white", xlim=c(5500,18500), ylim=yl,ann=F, axes=F)
#polygon(x,y, border=NA, col=rgb(0.0, 0.0, 0.1, 0.2), ylim=c(-20,20))
#par(new=T)
#plot(x[1:9], meanrmse_100, xlim=c(5500,18500), ylim=yl, pch=1,cex.lab=1.5, cex.axis=1.5,ann=F, axes=F)


### 300 plot
###
#meanrmse_300 <-  means_300_fin[,tar]
#sdrmse_300 <- sds_300_fin[,tar]
#x <- c(seq(6000,18000,1500), rev(seq(6000,18000,1500)))
#x <- c(6000,7500,12000,15000,15000,12000,7500,6000)
#y1 <- meanrmse_300+sdrmse_300
#y2 <-meanrmse_300-sdrmse_300
#y <- c(y1, rev(y2)) 
#par(new=T)
#plot(meanrmse_300, col="white", xlim=c(5500,18500), ylim=yl,ann=F, axes=F)
#polygon(x,y, border=NA, col=rgb(0.0, 0.4, 0.1, 0.2), ylim=yl)
#par(new=T)
#plot(x[1:4], meanrmse_300,xlim=c(5500,18500), ylim=yl, pch=2,cex.lab=1.5, cex.axis=1.5,ann=F, axes=F)


### 500 plot
### 

#meanrmse_500 <-  means_500_fin[,tar]
#sdrmse_500 <- sds_500_fin[,tar]
#x <- c(seq(6000,18000,1500), rev(seq(6000,18000,1500)))
#x <- c(6000,7500,13500,13500,7500,6000)
#y1 <- meanrmse_500+sdrmse_500
#y2 <-meanrmse_500-sdrmse_500
#y <- c(y1, rev(y2)) 
#par(new=T)
#plot(meanrmse_500, col="white",xlim=c(5500,18500), ylim=yl,ann=F, axes=F)
#polygon(x,y, border=NA, col=rgb(0.4, 0.4, 0.1, 0.2), ylim=yl)
#par(new=T)
#plot(x[1:3], meanrmse_500,ylab="Mean RMSE [t/ha]", xlab="Fixed area size", main="Fixed area results", xlim=c(5500,18500), ylim=yl, pch=3,cex.lab=1.5, cex.axis=1.5)

#legend(x = 16000, y = 60, c("Plot size 100","Plot size 300", "Plot size 500"),pch=c(1,2,3), c("Var. Plot size 100","Var. Plot size 300", "Var. Plot size 500"), lwd = 10, col=c(rgb(0.0, 0.0, 0.1, 0.2),rgb(0.0, 0.4, 0.1, 0.2), rgb(0.4, 0.4, 0.1, 0.2)))
#legend("topright", c("Var. Plot size 100","Var. Plot size 300", "Var. Plot size 500"), lwd = 10, col=c(rgb(0.0, 0.0, 0.1, 0.2),rgb(0.0, 0.4, 0.1, 0.2), rgb(0.4, 0.4, 0.1, 0.2)))

# ## Rrmse

tar = 10
yl = c(0,1)
per_met = names[tar]

### 100 plot
###
meanRrmse_100 <-  means_100_fin[,tar]
sdRrmse_100 <- sds_100_fin[,tar]
x <- c(seq(6000,18000,1500), rev(seq(6000,18000,1500)))
y1 <- meanRrmse_100+sdRrmse_100
y2 <-meanRrmse_100-sdRrmse_100
y <- c(y1, rev(y2)) 
#par(xpd = T, mar = par()$mar +c(0,0,0,8))
#par(mar=c(5.1, 4.1, 4.1,9.5))
plot(meanRrmse_100, col="white", xlim=c(5500,18500), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.0, 0.0, 0.1, 0.2), ylim=c(-20,20))
par(new=T)
plot(x[1:9], meanRrmse_100, xlim=c(5500,18500), ylim=yl, pch=1,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F)


### 300 plot
###
meanRrmse_300 <-  means_300_fin[,tar]
sdRrmse_300 <- sds_300_fin[,tar]
#x <- c(seq(6000,18000,1500), rev(seq(6000,18000,1500)))
x <- c(6000,7500,12000,15000,15000,12000,7500,6000)
y1 <- meanRrmse_300+sdRrmse_300
y2 <-meanRrmse_300-sdRrmse_300
y <- c(y1, rev(y2)) 
par(new=T)
plot(meanRrmse_300, col="white", xlim=c(5500,18500), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.0, 0.4, 0.1, 0.2), ylim=yl)
par(new=T)
plot(x[1:4], meanRrmse_300,xlim=c(5500,18500), ylim=yl, pch=2,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F)


### 500 plot
### 

meanRrmse_500 <-  means_500_fin[,tar]
sdRrmse_500 <- sds_500_fin[,tar]
#x <- c(seq(6000,18000,1500), rev(seq(6000,18000,1500)))
x <- c(6000,7500,13500,13500,7500,6000)
y1 <- meanRrmse_500+sdRrmse_500
y2 <-meanRrmse_500-sdRrmse_500
y <- c(y1, rev(y2)) 
par(new=T)
plot(meanRrmse_500, col="white",xlim=c(5500,18500), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.4, 0.4, 0.1, 0.2), ylim=yl)
par(new=T)
plot(x[1:3], meanRrmse_500,ylab="Mean rRMSE", xlab="Fixed area size", xlim=c(5500,18500), ylim=yl, pch=3,cex.lab=1.6, cex.axis=1.6)

# ## Nrmse

tar = 12
yl = c(0,1)
per_met = names[tar]

### 100 plot
###
meanNrmse_100 <-  means_100_fin[,tar]
sdNrmse_100 <- sds_100_fin[,tar]
x <- c(seq(6000,18000,1500), rev(seq(6000,18000,1500)))
y1 <- meanNrmse_100+sdNrmse_100
y2 <-meanNrmse_100-sdNrmse_100
y <- c(y1, rev(y2)) 
#par(xpd = T, mar = par()$mar +c(0,0,0,8))
#par(mar=c(5.1, 4.1, 4.1,9.5))
plot(meanNrmse_100, col="white", xlim=c(5500,18500), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.0, 0.0, 0.1, 0.2), ylim=c(-20,20))
par(new=T)
plot(x[1:9], meanNrmse_100, xlim=c(5500,18500), ylim=yl, pch=1,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F)


### 300 plot
###
meanNrmse_300 <-  means_300_fin[,tar]
sdNrmse_300 <- sds_300_fin[,tar]
#x <- c(seq(6000,18000,1500), rev(seq(6000,18000,1500)))
x <- c(6000,7500,12000,15000,15000,12000,7500,6000)
y1 <- meanNrmse_300+sdNrmse_300
y2 <-meanNrmse_300-sdNrmse_300
y <- c(y1, rev(y2)) 
par(new=T)
plot(meanNrmse_300, col="white", xlim=c(5500,18500), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.0, 0.4, 0.1, 0.2), ylim=yl)
par(new=T)
plot(x[1:4], meanNrmse_300,xlim=c(5500,18500), ylim=yl, pch=2,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F)


### 500 plot
### 

meanNrmse_500 <-  means_500_fin[,tar]
sdNrmse_500 <- sds_500_fin[,tar]
#x <- c(seq(6000,18000,1500), rev(seq(6000,18000,1500)))
x <- c(6000,7500,13500,13500,7500,6000)
y1 <- meanNrmse_500+sdNrmse_500
y2 <-meanNrmse_500-sdNrmse_500
y <- c(y1, rev(y2)) 
par(new=T)
plot(meanNrmse_500, col="white",xlim=c(5500,18500), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.4, 0.4, 0.1, 0.2), ylim=yl)
par(new=T)
plot(x[1:3], meanNrmse_500,ylab="Mean NRMSE", xlab="Fixed area size", xlim=c(5500,18500), ylim=yl, pch=3,cex.lab=1.6, cex.axis=1.6)
#legend (x = 16000, y = 0.80, legend = c ("Plot size 100","Plot size 300", "Plot size 500"),pch=c(1,2,3),c ("Var. Plot size 100","Var. Plot size 300", "Var. Plot size 500"), lwd = 10, col=c(rgb(0.0, 0.0, 0.1, 0.2),rgb(0.0, 0.4, 0.1, 0.2), rgb(0.4, 0.4, 0.1, 0.2)))
#mtext("Plot size 100","Plot size 300", "Plot size 500",pch=c(1,2,3),side = 4, line = 1,outer = TRUE)
#mtext(19000, 0.85, "Var. Plot size 100","Var. Plot size 300", "Var. Plot size 500"), lwd = 10, col=c(rgb(0.0, 0.0, 0.1, 0.2),rgb(0.0, 0.4, 0.1, 0.2), rgb(0.4, 0.4, 0.1, 0.2)), side = 4, line = 1,outer = TRUE)
#legend(19000, 1  , bty ='n', xpd = TRUE, c("Plot size 100","Plot size 300", "Plot size 500"),pch=c(1,2,3))
#legend(19000, 0.85 , bty ='n', xpd = TRUE, c("Var. Plot size 100","Var. Plot size 300", "Var. Plot size 500"), lwd = 10, col=c(rgb(0.0, 0.0, 0.1, 0.2),rgb(0.0, 0.4, 0.1, 0.2), rgb(0.4, 0.4, 0.1, 0.2)))
#legend(19000, 1  , bty ='n', xpd = TRUE, c("Plot size 100","Plot size 300", "Plot size 500"),pch=c(1,2,3),cex=2.1)
#legend(19000, 0.75 , bty ='n', xpd = TRUE, c("Var. Plot size 100","Var. Plot size 300", "Var. Plot size 500"), lwd = 10, col=c(rgb(0.0, 0.0, 0.1, 0.2),rgb(0.0, 0.4, 0.1, 0.2), rgb(0.4, 0.4, 0.1, 0.2)),cex=2.1)

#par(mar=c(5, 4, 4, 2) + 0.1)
dev.off()

