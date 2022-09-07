require(matrixStats)

setwd("E:/6_clean_version_new/6_clean_version/99_outputs")

# save model results to file
load("no_fixed_model_results_100m.RData")
load("no_fixed_model_results_300m.RData")
load("no_fixed_model_results_500m.RData")


###################
#####   100
###################

# extract results
means_100 <- list()
sds_100 <- list()
dummy2=1

# get the examined sample sizes
vals <- unique(res_no_fixed_area_100[,1])

# iterate through the results matrix and extract the means for the 1000 runs
# of each sample size
for (i3 in vals){
  
  # select subset of results depending on sample size
  sub <- res_no_fixed_area_100[res_no_fixed_area_100[,1]==i3,]
  # calculate mean
  means_100[[dummy2]] <- colMeans(sub)
  sds_100[[dummy2]] <- colSds(sub)
  # increase dummy variable by 1
  dummy2 = dummy2+1
  
}

means_100_fin <- do.call(rbind, means_100)
sds_100_fin <- do.call(rbind, sds_100)

colnames(means_100_fin) <- c("sample_size", "iteration", "sampled area", "plot_size", "spearm_rf", "spearm_val", "rmse_rf", "rmse_val", "Rrmse_rf", "Rrmse_val","Nrmse_rf", "Nrmse_val", "bias_rf", "bias_val","max", "min","max_val", "min_val")
colnames(sds_100_fin) <- c("sample_size", "iteration", "sampled area", "plot_size", "spearm_rf", "spearm_val", "rmse_rf", "rmse_val", "Rrmse_rf", "Rrmse_val", "Nrmse_rf", "Nrmse_val", "bias_rf", "bias_val","max", "min","max_val", "min_val")



###################
#####   300
###################

# extract results
means_300 <- list()
sds_300 <- list()
dummy2=1

# get the examined sample sizes
vals <- unique(res_no_fixed_area_300[,1])

# iterate through the results matrix and extract the means for the 1000 runs
# of each sample size
for (i3 in vals){
  
  # select subset of results depending on sample size
  sub <- res_no_fixed_area_300[res_no_fixed_area_300[,1]==i3,]
  # calculate mean
  means_300[[dummy2]] <- colMeans(sub)
  sds_300[[dummy2]] <- colSds(sub)
  # increase dummy variable by 1
  dummy2 = dummy2+1
  
}

means_300_fin <- do.call(rbind, means_300)
sds_300_fin <- do.call(rbind, sds_300)

colnames(means_300_fin) <- c("sample_size", "iteration", "sampled area", "plot_size", "spearm_rf", "spearm_val", "rmse_rf", "rmse_val", "Rrmse_rf", "Rrmse_val", "Nrmse_rf", "Nrmse_val", "bias_rf", "bias_val","max", "min","max_val", "min_val")
colnames(sds_300_fin) <- c("sample_size", "iteration", "sampled area", "plot_size", "spearm_rf", "spearm_val", "rmse_rf", "rmse_val", "Rrmse_rf", "Rrmse_val", "Nrmse_rf", "Nrmse_val", "bias_rf", "bias_val","max", "min","max_val", "min_val")



###################
#####   500
###################

# extract results
means_500 <- list()
sds_500 <- list()
dummy2=1

# get the examined sample sizes
vals <- unique(res_no_fixed_area_500[,1])

# iterate through the results matrix and extract the means for the 1000 runs
# of each sample size
for (i3 in vals){
  
  # select subset of results depending on sample size
  sub <- res_no_fixed_area_500[res_no_fixed_area_500[,1]==i3,]
  # calculate mean
  means_500[[dummy2]] <- colMeans(sub)
  sds_500[[dummy2]] <- colSds(sub)
  # increase dummy variable by 1
  dummy2 = dummy2+1
  
}

means_500_fin <- do.call(rbind, means_500)
sds_500_fin <- do.call(rbind, sds_500)

colnames(means_500_fin) <- c("sample_size", "iteration", "sampled area", "plot_size", "spearm_rf", "spearm_val", "rmse_rf", "rmse_val", "Rrmse_rf", "Rrmse_val", "Nrmse_rf", "Nrmse_val", "bias_rf", "bias_val","max", "min","max_val", "min_val")
colnames(sds_500_fin) <- c("sample_size", "iteration", "sampled area", "plot_size", "spearm_rf", "spearm_val", "rmse_rf", "rmse_val", "Rrmse_rf", "Rrmse_val", "Nrmse_rf", "Nrmse_val", "bias_rf", "bias_val","max", "min","max_val", "min_val")



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

names <- c("sample_size", "iteration", "sampled area", "plot_size", "spearm_rf", "spearm_val", "rmse_rf", "rmse_val", "Rrmse_rf", "Rrmse_val", "Nrmse_rf", "Nrmse_val", "bias_rf", "bias_val","max", "min","max_val", "min_val")
#png(file="No fixed area results_3.png", res = 300, width=15, height=4, units = 'in')
setwd("E:/6_clean_version_new/6_clean_version/outputs")
#png(file="No fixed area results_tr.png", res = 300, width=17, height=4, units = 'in')
#png(file="No fixed area results_val.png", res = 300, width=17, height=4, units = 'in')
#par(oma = c(0, 0, 0, 8))
#tiff(filename = "No fixed area results_tr.tif", width = 20, height = 35, units = "cm", pointsize = 18, compression = "lzw", bg = "white", res = 300)
tiff(filename = "No fixed area results_val.tif", width = 20, height = 35, units = "cm", pointsize = 18, compression = "lzw", bg = "white", res = 300)
#par(cex=2)
par(mfrow=c(3,1), mar=c(4,5,1.8,5))
#png(file="No fixed area results_NRMSE.png", res = 300, width=17, height=4, units = 'in')
#par(mfrow=c(1,3), mar=c(5.1, 5.5, 4.1,6.5))
#for (i in 1:3){
 # par(mar=c(5.1, 4.1, 4.1,9.5))
  #plot(runif(3),runif(3),ylab="",xlab="")
#}

tar = 6
yl = c(0,1)
per_met = names[tar]

### 100 plot
###
meanr2_100 <-  means_100_fin[,tar]
sdr2_100 <- sds_100_fin[,tar]
x <- c(seq(2,16,1), rev(seq(2,16,1)))
x <- x*12
y1 <- meanr2_100+sdr2_100
y2 <-meanr2_100-sdr2_100
y <- c(y1, rev(y2))
plot(meanr2_100, col="white", xlim=c(24,180), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.0, 0.0, 0.1, 0.2), ylim=c(-20,20))
par(new=T)
plot(x[1:15], meanr2_100, xlim=c(24,180), ylim=yl, pch=1,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F)
n <- c(2400, 3600, 4800, 6000, 7200, 8400, 9600, 10800, 12000, 13200, 14400, 15600, 16800, 18000, 19200)
par(new = T)
plot(x[1:15], n, xlim=c(24,180), ylim=c(2400,96000), pch=1,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F, col= "gray")

### 300 plot
###
meanr2_300 <-  means_300_fin[,tar]
sdr2_300 <- sds_300_fin[,tar]
x <- c(seq(2,16,1), rev(seq(2,16,1)))
x <- x*12
y1 <- meanr2_300+sdr2_300
y2 <-meanr2_300-sdr2_300
y <- c(y1, rev(y2)) 
par(new=T)
plot(meanr2_300, col="white", xlim=c(24,180), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.0, 0.4, 0.1, 0.2), ylim=yl)
par(new=T)
plot(x[1:15], meanr2_300, xlim=c(24,180), ylim=yl, pch=2,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F)
n <- c(7200, 10800, 14400, 18000, 21600, 25200, 28800, 32400, 36000, 39600, 43200, 46800, 50400, 54000, 57600)
par(new = T)
plot(x[1:15], n, xlim=c(24,180), ylim=c(2400,96000), pch=2,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F, col= "gray")

### 500 plot
### 

meanr2_500 <-  means_500_fin[,tar]
sdr2_500 <- sds_500_fin[,tar]
x <- c(seq(2,16,1), rev(seq(2,16,1)))
x <- x*12
y1 <- meanr2_500+sdr2_500
y2 <-meanr2_500-sdr2_500
y <- c(y1, rev(y2)) 
par(new=T)
plot(meanr2_500, col="white",xlim=c(24,180), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.4, 0.4, 0.1, 0.2), ylim=yl)
par(new=T)
plot(x[1:15], meanr2_500, ylab="Spearman cor.", xlab="Sample size", xlim=c(24,180), ylim=yl, pch=3,cex.lab=1.6, cex.axis=1.5)
n <- c(12000, 18000, 24000, 30000, 36000, 42000, 48000, 54000, 60000, 66000, 72000, 78000, 84000, 90000, 96000)
par(new = T)
plot(x[1:15], n, xlim=c(24,180), ylim=c(2400,96000), pch=3,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F, col= "gray")
axis(side = 4, ylab="Spearman cor.", cex.lab=1.6, cex.axis=1.6)
mtext(side = 4, line = 3, "Sampled area [m²]", cex.lab=1.6, cex.axis=1.4)
#legend("bottomright", c("Plot size 100","Plot size 300", "Plot size 500"),pch=c(1,2,3))
#legend("topright", c("Var. Plot size 100","Var. Plot size 300", "Var. Plot size 500"), lwd = 10, col=c(rgb(0.0, 0.0, 0.1, 0.2),rgb(0.0, 0.4, 0.1, 0.2), rgb(0.4, 0.4, 0.1, 0.2)))

#tar = 7
#yl = c(0,80)
#per_met = names[tar]

### 100 plot
###
#meanrmse_100 <-  means_100_fin[,tar]
#sdrmse_100 <- sds_100_fin[,tar]
#x <- c(seq(2,16,1), rev(seq(2,16,1)))
#x <- x*12
#y1 <- meanrmse_100+sdrmse_100
#y2 <-meanrmse_100-sdrmse_100
#y <- c(y1, rev(y2)) 
#plot(meanrmse_100, col="white", xlim=c(24,180), ylim=yl,ann=F, axes=F)
#polygon(x,y, border=NA, col=rgb(0.0, 0.0, 0.1, 0.2), ylim=c(-20,20))
#par(new=T)
#plot(x[1:15], meanrmse_100, xlim=c(24,180), ylim=yl, pch=1,cex.lab=1.5, cex.axis=1.5,ann=F, axes=F)
#n <- c(2400, 3600, 4800, 6000, 7200, 8400, 9600, 10800, 12000, 13200, 14400, 15600, 16800, 18000, 19200)
#par(new = T)
#plot(x[1:15], n, xlim=c(24,180), ylim=c(2400,96000), pch=1,cex.lab=1.5, cex.axis=1.5,ann=F, axes=F, col= "gray")

### 300 plot
###
#meanrmse_300 <-  means_300_fin[,tar]
#sdrmse_300 <- sds_300_fin[,tar]
#x <- c(seq(2,16,1), rev(seq(2,16,1)))
#x <- x*12
#y1 <- meanrmse_300+sdrmse_300
#y2 <-meanrmse_300-sdrmse_300
#y <- c(y1, rev(y2)) 
#par(new=T)
#plot(meanrmse_300, col="white", xlim=c(24,180), ylim=yl,ann=F, axes=F)
#polygon(x,y, border=NA, col=rgb(0.0, 0.4, 0.1, 0.2), ylim=yl)
#par(new=T)
#plot(x[1:15], meanrmse_300, xlim=c(24,180), ylim=yl, pch=2,cex.lab=1.5, cex.axis=1.5,ann=F, axes=F)
#n <- c(7200, 10800, 14400, 18000, 21600, 25200, 28800, 32400, 36000, 39600, 43200, 46800, 50400, 54000, 57600)
#par(new = T)
#plot(x[1:15], n, xlim=c(24,180), ylim=c(2400,96000), pch=2,cex.lab=1.5, cex.axis=1.5,ann=F, axes=F, col= "gray")


### 500 plot
### 

#meanrmse_500 <-  means_500_fin[,tar]
#sdrmse_500 <- sds_500_fin[,tar]
#x <- c(seq(2,16,1), rev(seq(2,16,1)))
#x <- x*12
#y1 <- meanrmse_500+sdrmse_500
#y2 <-meanrmse_500-sdrmse_500
#y <- c(y1, rev(y2)) 
#par(new=T)
#plot(meanrmse_500, col="white",xlim=c(24,180), ylim=yl,ann=F, axes=F)
#polygon(x,y, border=NA, col=rgb(0.4, 0.4, 0.1, 0.2), ylim=yl)
#par(new=T)
#plot(x[1:15], meanrmse_500, ylab="Mean RMSE [t/ha]", xlab="Sample size", main="No fixed area results", xlim=c(24,180), ylim=yl, pch=3,cex.lab=1.5, cex.axis=1.5)
#n <- c(12000, 18000, 24000, 30000, 36000, 42000, 48000, 54000, 60000, 66000, 72000, 78000, 84000, 90000, 96000)
#par(new = T)
#plot(x[1:15], n, xlim=c(24,180), ylim=c(2400,96000), pch=3,cex.lab=1.5, cex.axis=1.5,ann=F, axes=F, col= "gray")
#axis(side = 4, ylab="Mean RMSE [t/ha]", cex.lab=1.5, cex.axis=1.5)
#mtext(side = 4, line = 3, "Sampled area [m²]", cex.lab=1.5, cex.axis=1.5)
#legend("bottomright", c("Plot size 100","Plot size 300", "Plot size 500"),pch=c(1,2,3))
#legend("topright", c("Var. Plot size 100","Var. Plot size 300", "Var. Plot size 500"), lwd = 10, col=c(rgb(0.0, 0.0, 0.1, 0.2),rgb(0.0, 0.4, 0.1, 0.2), rgb(0.4, 0.4, 0.1, 0.2)))

# ## Rrmse
tar = 10
yl = c(0,1)
per_met = names[tar]

### 100 plot
###
meanRrmse_100 <-  means_100_fin[,tar]
sdRrmse_100 <- sds_100_fin[,tar]
x <- c(seq(2,16,1), rev(seq(2,16,1)))
x <- x*12
y1 <- meanRrmse_100+sdRrmse_100
y2 <-meanRrmse_100-sdRrmse_100
y <- c(y1, rev(y2))
#par(oma = c(0, 0, 0, 1))
#par(mar=c(5.1, 4.1, 4.1,9.5))

#par(mar=c(5.1, 4.1, 4.1,1), oma = c(0, 0, 0, 4))

#par(xpd = T, mar = par()$mar +c(0,0,0,8))
#par(xpd = T, par(mar=c(0,0,0,8)) +c(0,0,0,8),side=4,line=5)
#par(xpd = T, par(mar=c(5.1, 4.1, 4.1, 10.1)))
plot(meanRrmse_100, col="white", xlim=c(24,180), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.0, 0.0, 0.1, 0.2), ylim=c(-20,20))
par(new=T)
plot(x[1:15], meanRrmse_100, xlim=c(24,180), ylim=yl, pch=1,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F)
n <- c(2400, 3600, 4800, 6000, 7200, 8400, 9600, 10800, 12000, 13200, 14400, 15600, 16800, 18000, 19200)
par(new = T)
plot(x[1:15], n, xlim=c(24,180), ylim=c(2400,96000), pch=1,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F, col= "gray")

### 300 plot
###
meanRrmse_300 <-  means_300_fin[,tar]
sdRrmse_300 <- sds_300_fin[,tar]
x <- c(seq(2,16,1), rev(seq(2,16,1)))
x <- x*12
y1 <- meanRrmse_300+sdRrmse_300
y2 <-meanRrmse_300-sdRrmse_300
y <- c(y1, rev(y2)) 
par(new=T)
plot(meanRrmse_300, col="white", xlim=c(24,180), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.0, 0.4, 0.1, 0.2), ylim=yl)
par(new=T)
plot(x[1:15], meanRrmse_300, xlim=c(24,180), ylim=yl, pch=2,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F)
n <- c(7200, 10800, 14400, 18000, 21600, 25200, 28800, 32400, 36000, 39600, 43200, 46800, 50400, 54000, 57600)
par(new = T)
plot(x[1:15], n, xlim=c(24,180), ylim=c(2400,96000), pch=2,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F, col= "gray")

### 500 plot
### 

meanRrmse_500 <-  means_500_fin[,tar]
sdRrmse_500 <- sds_500_fin[,tar]
x <- c(seq(2,16,1), rev(seq(2,16,1)))
x <- x*12
y1 <- meanRrmse_500+sdRrmse_500
y2 <-meanRrmse_500-sdRrmse_500
y <- c(y1, rev(y2)) 
par(new=T)
plot(meanRrmse_500, col="white",xlim=c(24,180), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.4, 0.4, 0.1, 0.2), ylim=yl)
par(new=T)
plot(x[1:15], meanRrmse_500, ylab="Mean rRMSE", xlab="Sample size", xlim=c(24,180), ylim=yl, pch=3,cex.lab=1.5, cex.axis=1.5)
n <- c(12000, 18000, 24000, 30000, 36000, 42000, 48000, 54000, 60000, 66000, 72000, 78000, 84000, 90000, 96000)
par(new = T)
plot(x[1:15], n, xlim=c(24,180), ylim=c(2400,96000), pch=3,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F, col= "gray")
axis(side = 4, ylab="Mean rRMSE", cex.lab=1.6, cex.axis=1.6)
mtext(side = 4, line = 3, "Sampled area [m²]", cex.lab=1.6, cex.axis=1.4)

# ## Nrmse
tar = 12
yl = c(0,1)
per_met = names[tar]

### 100 plot
###
meanNrmse_100 <-  means_100_fin[,tar]
sdNrmse_100 <- sds_100_fin[,tar]
x <- c(seq(2,16,1), rev(seq(2,16,1)))
x <- x*12
y1 <- meanNrmse_100+sdNrmse_100
y2 <-meanNrmse_100-sdNrmse_100
y <- c(y1, rev(y2))
#par(oma = c(0, 0, 0, 1))
#par(mar=c(5.1, 4.1, 4.1,9.5))

#par(mar=c(5.1, 4.1, 4.1,1), oma = c(0, 0, 0, 4))

#par(xpd = T, mar = par()$mar +c(0,0,0,8))
#par(xpd = T, par(mar=c(0,0,0,8)) +c(0,0,0,8),side=4,line=5)
#par(xpd = T, par(mar=c(5.1, 4.1, 4.1, 10.1)))
plot(meanNrmse_100, col="white", xlim=c(24,180), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.0, 0.0, 0.1, 0.2), ylim=c(-20,20))
par(new=T)
plot(x[1:15], meanNrmse_100, xlim=c(24,180), ylim=yl, pch=1,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F)
n <- c(2400, 3600, 4800, 6000, 7200, 8400, 9600, 10800, 12000, 13200, 14400, 15600, 16800, 18000, 19200)
par(new = T)
plot(x[1:15], n, xlim=c(24,180), ylim=c(2400,96000), pch=1,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F, col= "gray")

### 300 plot
###
meanNrmse_300 <-  means_300_fin[,tar]
sdNrmse_300 <- sds_300_fin[,tar]
x <- c(seq(2,16,1), rev(seq(2,16,1)))
x <- x*12
y1 <- meanNrmse_300+sdNrmse_300
y2 <-meanNrmse_300-sdNrmse_300
y <- c(y1, rev(y2)) 
par(new=T)
plot(meanNrmse_300, col="white", xlim=c(24,180), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.0, 0.4, 0.1, 0.2), ylim=yl)
par(new=T)
plot(x[1:15], meanNrmse_300, xlim=c(24,180), ylim=yl, pch=2,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F)
n <- c(7200, 10800, 14400, 18000, 21600, 25200, 28800, 32400, 36000, 39600, 43200, 46800, 50400, 54000, 57600)
par(new = T)
plot(x[1:15], n, xlim=c(24,180), ylim=c(2400,96000), pch=2,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F, col= "gray")

### 500 plot
### 

meanNrmse_500 <-  means_500_fin[,tar]
sdNrmse_500 <- sds_500_fin[,tar]
x <- c(seq(2,16,1), rev(seq(2,16,1)))
x <- x*12
y1 <- meanNrmse_500+sdNrmse_500
y2 <-meanNrmse_500-sdNrmse_500
y <- c(y1, rev(y2)) 
par(new=T)
plot(meanNrmse_500, col="white",xlim=c(24,180), ylim=yl,ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.4, 0.4, 0.1, 0.2), ylim=yl)
par(new=T)
plot(x[1:15], meanNrmse_500, ylab="Mean NRMSE", xlab="Sample size", xlim=c(24,180), ylim=yl, pch=3,cex.lab=1.6, cex.axis=1.6)
n <- c(12000, 18000, 24000, 30000, 36000, 42000, 48000, 54000, 60000, 66000, 72000, 78000, 84000, 90000, 96000)
par(new = T)
plot(x[1:15], n, xlim=c(24,180), ylim=c(2400,96000), pch=3,cex.lab=1.6, cex.axis=1.6,ann=F, axes=F, col= "gray")
axis(side = 4, ylab="Mean NRMSE", cex.lab=1.6, cex.axis=1.6)
mtext(side = 4, line = 3, "Sampled area [m²]", cex.lab=1.6, cex.axis=1.4)
#par(xpd=TRUE)
#legend(185, 1  , bty ='n', c("Plot size 100","Plot size 300", "Plot size 500"),pch=c(1,2,3))
#legend(185, 0.85 , bty ='n', c("Var. Plot size 100","Var. Plot size 300", "Var. Plot size 500"), lwd = 10, col=c(rgb(0.0, 0.0, 0.1, 0.2),rgb(0.0, 0.4, 0.1, 0.2), rgb(0.4, 0.4, 0.1, 0.2)))

#legend(185, 1  , bty ='n', xpd = TRUE, c("Plot size 100","Plot size 300", "Plot size 500"),pch=c(1,2,3))
#legend(185, 0.85 , bty ='n', xpd = TRUE, c("Var. Plot size 100","Var. Plot size 300", "Var. Plot size 500"), lwd = 10, col=c(rgb(0.0, 0.0, 0.1, 0.2),rgb(0.0, 0.4, 0.1, 0.2), rgb(0.4, 0.4, 0.1, 0.2)))

#legend("bottomright", c("Plot size 100","Plot size 300", "Plot size 500"),pch=c(1,2,3))
#legend("topright", c("Var. Plot size 100","Var. Plot size 300", "Var. Plot size 500"), lwd = 10, col=c(rgb(0.0, 0.0, 0.1, 0.2),rgb(0.0, 0.4, 0.1, 0.2), rgb(0.4, 0.4, 0.1, 0.2)))
#par(mar=c(5, 4, 4, 2) + 0.1)
dev.off()

