require(randomForest)
require(e1071)
require(raster)
require(rgdal)
require(MLmetrics)
require(matrixStats)
require(hydroGOF)
require(velox)
require(VSURF)
# change folder to folger with input raster datasets
setwd("E:/6_clean_version_new/6_clean_version/2_rasters")

# load raster datasets to velox raster objects
dem <- velox(stack("nDSM.tif"))
loc_min <- velox(stack("local_min_win_pan_cloud_9.tif"))
ndvi <- velox(stack("ndvi_sm.tif"))
main_bands <- velox(stack("mainbands.tif"))
savi <- velox(stack("savi.tif"))
tex_sm <- velox(stack("tex_pca_sm.tif"))

####################################################

classmap <- velox(stack("svm_4classes5_sm.tif"))


# read shapefile containing biomass plots and reference
setwd("E:/6_clean_version_new/6_clean_version/3_shapes")
plots <- readOGR(".", "300")

# check shapefile attribute table
head(plots@data)

# extract values from raster files at location of plots saved in the shapefile
# using velox
dem_ex <- dem$extract(plots, fun=mean)
loc_min_ex <- loc_min$extract(plots, fun=sum)
ndvi_ex <- ndvi$extract(plots, fun=mean)
savi_ex <- savi$extract(plots, fun=mean)
mainbands_ex <- main_bands$extract(plots, fun=mean)
tex_sm_ex <- tex_sm$extract(plots, fun=mean)

#########################################################
# extract cover fraction of soil, vegetation and shadow
#########################################################
# create empty matrix to store results
results <- matrix(NA, nrow=length(plots), ncol=3)
# rename column names
colnames(results) <- c("shadow", "soil", "vegetation") 

# extract raster values of all pixels in the cluster polygons
classes <- classmap$extract(plots)

# count pixels of all considered classes (soil, shadow, coniferous, broadleaved) in each cluster-polygon
# calculate percentages of each class

for(i in 1:length(plots)) {
  
  # get first polygon
  data <- classes[[i]]
  
  # calculate percentages of each class
  results[i,1] <- length(data[data==1])/length(data)
  results[i,2] <- length(data[data==2])/length(data)
  results[i,3] <- length(data[data==3])/length(data)
}

#########################################################

# combine all predictors into a single dataset - species information is also attached
all_preds_300 <- cbind(plots$bio300, plots$code, dem_ex, loc_min_ex, ndvi_ex, savi_ex, mainbands_ex, results, tex_sm_ex)
#all_preds <- cbind(plots$bio300, dem_ex, loc_min_ex, ndvi_ex, results, tcup_ex, tex_pca_ex, tex_sm_ex)

# change column names to make them all interpretable
cnames <- c("biomass", "species", "nDSM", "locmin", "ndvi", "savi", "band1", "band2", "band3", "band4", "shadow", "soil", "vegetation", "tex1", "tex2", "tex3", "tex4", 
            "tex5", "tex6", "tex7", "tex8", "tex9", "tex10")

colnames(all_preds_300) <- cnames
head(all_preds_300)

# save dataframe to a dataset to save time, if sth is changed in the code below
setwd("E:/6_clean_version_new/6_clean_version/99_outputs")
save(all_preds_300, file="all_preds_extracted_plot300.RData")
load("all_preds_extracted_plot300.RData")

# quick test using all predictors (except for species as
rf_test <- randomForest(all_preds_300[,-c((1:2))], all_preds_300[,1], ntree=500, mtry=3)


# run variable selection with VSURF
set.seed(25)
rf_vsurf <- VSURF(all_preds_300[,-c(1,2)], all_preds_300[,1], stepFactor=1, doBest=T)
rf_vsurf$varselect.thres
rf_vsurf$varselect.interp
rf_vsurf$varselect.pred
common_predictors_in_all_plot_size <- all_preds_300[,-c((1:2),(4:7),(10),(15:18),(21:25))]
colnames(all_preds_300[,-c((1:2),(4:7),(10),(15:18),(21:25))])

# check vsurf results and select predictors

# run final RF model with selected predictors

rf_test <- tuneRF(all_preds_300[,-c((1:2),(4:7),(10),(15:18),(21:25))], all_preds_300[,1], stepFactor=1, doBest=T)
colnames(all_preds_300[,(rf_vsurf$varselect.interp+2)])

# check results
rf_test
plot(all_preds_300[,1], rf_test$predicted)
abline(0,1, col="red")



############################
## start simulations here ##
############################


# stratify the dataset according to species
sp1 <- as.data.frame(all_preds_300[all_preds_300[,2]==1,])
sp2 <- as.data.frame(all_preds_300[all_preds_300[,2]==2,])
sp3 <- as.data.frame(all_preds_300[all_preds_300[,2]==3,])
sp4 <- as.data.frame(all_preds_300[all_preds_300[,2]==4,])


# sort each species from low to high biomass values
sp1 <- sp1[order(sp1$biomass),]
sp2 <- sp2[order(sp2$biomass),]
sp3 <- sp3[order(sp3$biomass),]
sp4 <- sp4[order(sp4$biomass),]

# prepare stratification into three parts
# by dividing total number of samples by 3
str1 <- round(length(sp1[,1])/3)
str2 <- round(length(sp2[,1])/3)
str3 <- round(length(sp3[,1])/3)
str4 <- round(length(sp4[,1])/3)

# split the species-specific datasets into three equally sized parts (the strat)

sp1_str1 <- sp1[1:str1,]
sp1_str2 <- sp1[(str1+1):(2*str1),]
sp1_str3 <- sp1[(str1*2+1):length(sp1[,1]),]

sp2_str1 <- sp2[1:str2,]
sp2_str2 <- sp2[(str2+1):(2*str2),]
sp2_str3 <- sp2[(str2*2+1):length(sp2[,1]),]

sp3_str1 <- sp3[1:str3,]
sp3_str2 <- sp3[(str3+1):(2*str3),]
sp3_str3 <- sp3[(str3*2+1):length(sp3[,1]),]

sp4_str1 <- sp4[1:str4,]
sp4_str2 <- sp4[(str4+1):(2*str4),]
sp4_str3 <- sp4[(str4*2+1):length(sp4[,1]),]



# create empty matrix to store results
res_fixed_300 <- matrix(NA, ncol=18, nrow=(length(seq(6000,18000,1500))*1000))

#create dummy variable to iterate through matrix rows
dummy = 1

# set seed to have reproducible results
set.seed(7)

# start loop and iterate through different sample sizes
for (i in seq(6000,18000,1500)){
  i4=300
  # divide the fixed area by 300 (as the plot size is 300 m2)
  # this has to be changed for other plot sizes
  
  ## !!!! add the ceiling()
  i3 <- ceiling((i/300)/12)
  
  
  # start second loop and run each sample size 1000 times
  for (i2 in 1:1000){
    
    # get samples from 300 m plots
    # get i samples from each strata
    rsam <- sample(seq(1,nrow(sp1_str1),1), i3, replace = F)
    sp1_str1_rs <- sp1_str1[rsam,]
    rsam <- sample(seq(1,nrow(sp1_str2),1), i3, replace = F)
    sp1_str2_rs <- sp1_str2[rsam,]
    rsam <- sample(seq(1,nrow(sp1_str3),1), i3, replace = F)
    sp1_str3_rs <- sp1_str3[rsam,]
    
    rsam <- sample(seq(1,nrow(sp2_str1),1), i3, replace = F)
    sp2_str1_rs <- sp2_str1[rsam,]
    rsam <- sample(seq(1,nrow(sp2_str2),1), i3, replace = F)
    sp2_str2_rs <- sp2_str2[rsam,]
    rsam <- sample(seq(1,nrow(sp2_str3),1), i3, replace = F)
    sp2_str3_rs <- sp2_str3[rsam,]
    
    rsam <- sample(seq(1,nrow(sp3_str1),1), i3, replace = F)
    sp3_str1_rs <- sp3_str1[rsam,]
    rsam <- sample(seq(1,nrow(sp3_str2),1), i3, replace = F)
    sp3_str2_rs <- sp3_str2[rsam,]
    rsam <- sample(seq(1,nrow(sp3_str3),1), i3, replace = F)
    sp3_str3_rs <- sp3_str3[rsam,]
    
    rsam <- sample(seq(1,nrow(sp4_str1),1), i3, replace = F)
    sp4_str1_rs <- sp4_str1[rsam,]
    rsam <- sample(seq(1,nrow(sp4_str2),1), i3, replace = F)
    sp4_str2_rs <- sp4_str2[rsam,]
    rsam <- sample(seq(1,nrow(sp4_str3),1), i3, replace = F)
    sp4_str3_rs <- sp4_str3[rsam,]
    
    # join samples from all strata
    all_data <- rbind(sp1_str1_rs, sp1_str2_rs, sp1_str3_rs, sp2_str1_rs, sp2_str2_rs, sp2_str3_rs,
                      sp3_str1_rs, sp3_str2_rs, sp3_str3_rs, sp4_str1_rs, sp4_str2_rs, sp4_str3_rs)
    
    # run random forest model
    # create bootstrap data split (sample i random samples from the i samples with replacement)
    samp <- sample(seq(1, nrow(all_data),1), 0.8*nrow(all_data))
    all_data_tr <- all_data[samp,] 
    all_data_val <- all_data[-samp,]
    
    # run random forest model
    # mtry tuning
    #rf_mod <- randomForest(all_data[,-c((1:2))], all_data[,1], ntree=500, mtry=17)
    rf_mod <- tuneRF(all_data_tr[,-c((1:2),(4:7),(10),(15:18),(21:25))], all_data_tr[,1], stepFactor=1, doBest=T)
    # derive spearman correlation between predicted and observed variables
    spearm <- cor(rf_mod$predicted, all_data_tr[,1], method="spearman")
    rmse <- RMSE(rf_mod$predicted, all_data_tr[,1])
    Rrmse1 <- rmse/mean(all_data_tr[,1])
    Bias1 <- pbias(rf_mod$predicted,all_data_tr[,1])
    min_b <- min(all_data_tr[,1])
    max_b <- max(all_data_tr[,1])
    Nrmse1 <- rmse/(max(all_data_tr[,1])-min(all_data_tr[,1]))
    
    # apply random forest model to the validation set
    pred_val <- predict(rf_mod, all_data_val[,-c((1:2),(4:7),(10),(15:18),(21:25))])
    
    # derive spearman correlation between predicted and observed variables
    spearm2 <- cor(pred_val, all_data_val[,1], method="spearman")
    rmse2 <- RMSE(pred_val,all_data_val[,1])
    Rrmse2 <- rmse2/mean(all_data_val[,1])
    Bias2 <- pbias(pred_val, all_data_val[,1])
    min_b2 <- min(all_data_val[,1])
    max_b2 <- max(all_data_val[,1])
    Nrmse2 <- rmse2/(max(all_data_val[,1])-min(all_data_val[,1]))
    
    # save sample size, iteration id and correlation value to result matrix
    res_fixed_300[dummy,] <- c(i3, i2, i, i4, spearm, spearm2, rmse, rmse2, Rrmse1, Rrmse2, Nrmse1, Nrmse2, Bias1, Bias2,min_b, max_b,min_b2, max_b2)
    # increase dummy variable by 1
    dummy = dummy+1     
    
  }
  print(i)
}

# check results
head(res_fixed_300)

# save model results to file
setwd("E:/6_clean_version_new/6_clean_version/99_outputs")
save(res_fixed_300, file = "fixed_area_results_300m.RData")

# extract results
means <- list()
sds <- list()
dummy2=1

# get the examined sample sizes
vals <- unique(res_fixed_300[,1])

# iterate through the results matrix and extract the means for the 1000 runs
# of each sample size
for (i3 in vals){
  
  # select subset of results depending on sample size
  sub <- res_fixed_300[res_fixed_300[,1]==i3,]
  # calculate mean
  means[[dummy2]] <- colMeans(sub)
  sds[[dummy2]] <- colSds(sub)
  # increase dummy variable by 1
  dummy2 = dummy2+1
  
}

means_fin <- do.call(rbind, means)
sds_fin <- do.call(rbind, sds)

colnames(means_fin) <- c("sample_size", "iteration", "fixed_area", "plot_size", "spearm_rf", "spearm_svm", "rmse_rf", "rmse_svm", "Rrmse_rf", "Rrmse_svm", "bias_rf", "bias_svm" , "min_b", "max_b")
colnames(sds_fin) <- c("sample_size", "iteration", "fixed_area", "plot_size", "spearm_rf", "spearm_svm", "rmse_rf", "rmse_svm", "Rrmse_rf", "Rrmse_svm", "bias_rf", "bias_svm", "min_b", "max_b")

####################################################################################
## plot mean and standard deviation of spearman of random forest regression results
####################################################################################
tar = 3
plot(seq(6000,18000,1500), means_fin[,tar], ylim=c(0, 1), ylab=colnames(means_fin)[tar])
par(new=T)
plot(seq(6000,18000,1500), means_fin[,tar]+sds_fin[,tar], ylim=c(0, 1), col="grey", ann=F)
par(new=T)
plot(seq(6000,18000,1500), means_fin[,tar]-sds_fin[,tar], ylim=c(0, 1), col="grey", ann=F)


# ## r2
meanr2_300 <-  means_fin[,tar]
sdr2_300 <- sds_fin[,tar]
x <- c(1:9,9:1)
y1 <- meanr2_300+sdr2_300
y2 <-meanr2_300-sdr2_300
y <- c(y1, rev(y2)) 
plot(meanr2_300, col="white", xlim=c(0,10), ylim=c(0,1),ann=F, axes=F)
polygon(x,y, border=NA, col=rgb(0.0, 0.4, 0.1, 0.5), ylim=c(-20,20))
par(new=T)
plot(x[1:9], meanr2_300,ylab="mean r_squared", xlab="plot size",xlim=c(0,10), ylim=c(0,1), pch=1,cex.lab=1.5, cex.axis=1.5)



####################################################################################
## plot mean and standard deviation of spearman of support vector regression results
####################################################################################
tar = 4
plot(seq(6000,18000,1500), means_fin[,tar], ylim=c(0.4, 0.9), ylab=colnames(means_fin)[tar])
par(new=T)
plot(seq(6000,18000,1500), means_fin[,tar]+sds_fin[,tar], ylim=c(0.4, 0.9), col="grey", ann=F)
par(new=T)
plot(seq(6000,18000,1500), means_fin[,tar]-sds_fin[,tar], ylim=c(0.4, 0.9), col="grey", ann=F)

####################################################################################
## plot mean and standard deviation of rmse of random forest regression results
####################################################################################
tar = 5
plot(seq(6000,18000,1500), means_fin[,tar], ylim=c(20, 100), ylab=colnames(means_fin)[tar])
par(new=T)
plot(seq(6000,18000,1500), means_fin[,tar]+sds_fin[,tar], ylim=c(20, 100), col="grey", ann=F)
par(new=T)
plot(seq(6000,18000,1500), means_fin[,tar]-sds_fin[,tar], ylim=c(20, 100), col="grey", ann=F)

####################################################################################
## plot mean and standard deviation of rmse of support vector regression results
####################################################################################
tar = 6
plot(seq(6000,18000,1500), means_fin[,tar], ylim=c(20, 100), ylab=colnames(means_fin)[tar])
par(new=T)
plot(seq(6000,18000,1500), means_fin[,tar]+sds_fin[,tar], ylim=c(20, 100), col="grey", ann=F)
par(new=T)
plot(seq(6000,18000,1500), means_fin[,tar]-sds_fin[,tar], ylim=c(20, 100), col="grey", ann=F)
####################################################################################
## plot mean and standard deviation of rrmse of random forest regression results
####################################################################################
tar = 7
plot(seq(6000,18000,1500), means_fin[,tar], ylim=c(0.4, 0.9), ylab=colnames(means_fin)[tar])
par(new=T)
plot(seq(6000,18000,1500), means_fin[,tar]+sds_fin[,tar], ylim=c(0.4, 0.9), col="grey", ann=F)
par(new=T)
plot(seq(6000,18000,1500), means_fin[,tar]-sds_fin[,tar], ylim=c(0.4, 0.9), col="grey", ann=F)


####################################################################################
## plot mean and standard deviation of rrmse of support vector regression results
####################################################################################
tar = 8
plot(seq(6000,18000,1500), means_fin[,tar], ylim=c(0.4, 0.9), ylab=colnames(means_fin)[tar])
par(new=T)
plot(seq(6000,18000,1500), means_fin[,tar]+sds_fin[,tar], ylim=c(0.4, 0.9), col="grey", ann=F)
par(new=T)
plot(seq(6000,18000,1500), means_fin[,tar]-sds_fin[,tar], ylim=c(0.4, 0.9), col="grey", ann=F)

####################################################################################
## plot mean and standard deviation of bias of random forest regression results
####################################################################################
tar = 9
plot(seq(6000,18000,1500), means_fin[,tar], ylim=c(-20, 100), ylab=colnames(means_fin)[tar])
par(new=T)
plot(seq(6000,18000,1500), means_fin[,tar]+sds_fin[,tar], ylim=c(-20, 100), col="grey", ann=F)
par(new=T)
plot(seq(6000,18000,1500), means_fin[,tar]-sds_fin[,tar], ylim=c(-20, 100), col="grey", ann=F)

####################################################################################
## plot mean and standard deviation of bias of support vector regression results
####################################################################################
tar = 10
plot(seq(6000,18000,1500), means_fin[,tar], ylim=c(-20, 100), ylab=colnames(means_fin)[tar])
par(new=T)
plot(seq(6000,18000,1500), means_fin[,tar]+sds_fin[,tar], ylim=c(-20, 100), col="grey", ann=F)
par(new=T)
plot(seq(6000,18000,1500), means_fin[,tar]-sds_fin[,tar], ylim=c(-20, 100), col="grey", ann=F)

