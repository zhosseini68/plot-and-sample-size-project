require(randomForest)
require(e1071)
require(raster)
require(rgdal)
require(MLmetrics)
require(matrixStats)
require(hydroGOF)
require(VSURF)
# load the extracted plot values (this step was already
# conducted in the file 1_fixed area100_ff_compl_ff.R)
setwd("E:/6_clean_version_new/6_clean_version/99_outputs")
load("all_preds_extracted_plot100.RData")
# run random forest model
rf_test <- randomForest(all_preds_100[,-c((1:2),(4:7),(10),(15:18),(21:25))], all_preds_100[,1], ntree=500, mtry=3)


# run variable selection with VSURF
set.seed(25)
#rf_vsurf <- VSURF(all_preds_100[,-c(1,2)], all_preds_100[,1], stepFactor=1, doBest=T)
#rf_vsurf$varselect.thres
#rf_vsurf$varselect.interp
#rf_vsurf$varselect.pred
#colnames(all_preds_100[,(rf_vsurf$varselect.interp+2)])
common_predictors_in_all_plot_size <- all_preds_100[,-c((1:2),(4:7),(10),(15:18),(21:25))]
colnames(all_preds_100[,-c((1:2),(4:7),(10),(15:18),(21:25))])

# stratify the dataset according to species
sp1 <- as.data.frame(all_preds_100[all_preds_100[,2]==1,])
sp2 <- as.data.frame(all_preds_100[all_preds_100[,2]==2,])
sp3 <- as.data.frame(all_preds_100[all_preds_100[,2]==3,])
sp4 <- as.data.frame(all_preds_100[all_preds_100[,2]==4,])


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
res_no_fixed_area_100 <- matrix(NA, ncol=18, nrow=(length(seq(2,16,1))*1000))

#create dummy variable to iterate through matrix rows
dummy = 1
dummy2 = 2

# set seed to have reproducible results
set.seed(7)

# start loop and iterate through different sample sizes
for (i in seq(2,16,1)){
  i4=100
  i5=i*12*100
  # start second loop and run each sample size 100 times
  for (i2 in 1:1000){
    
    # get i samples from each strata
    rsam <- sample(seq(1,nrow(sp1_str1),1), i, replace = F)
    sp1_str1_rs <- sp1_str1[rsam,]
    rsam <- sample(seq(1,nrow(sp1_str2),1), i, replace = F)
    sp1_str2_rs <- sp1_str2[rsam,]
    rsam <- sample(seq(1,nrow(sp1_str3),1), i, replace = F)
    sp1_str3_rs <- sp1_str3[rsam,]
    
    rsam <- sample(seq(1,nrow(sp2_str1),1), i, replace = F)
    sp2_str1_rs <- sp2_str1[rsam,]
    rsam <- sample(seq(1,nrow(sp2_str2),1), i, replace = F)
    sp2_str2_rs <- sp2_str2[rsam,]
    rsam <- sample(seq(1,nrow(sp2_str3),1), i, replace = F)
    sp2_str3_rs <- sp2_str3[rsam,]
    
    rsam <- sample(seq(1,nrow(sp3_str1),1), i, replace = F)
    sp3_str1_rs <- sp3_str1[rsam,]
    rsam <- sample(seq(1,nrow(sp3_str2),1), i, replace = F)
    sp3_str2_rs <- sp3_str2[rsam,]
    rsam <- sample(seq(1,nrow(sp3_str3),1), i, replace = F)
    sp3_str3_rs <- sp3_str3[rsam,]
    
    rsam <- sample(seq(1,nrow(sp4_str1),1), i, replace = F)
    sp4_str1_rs <- sp4_str1[rsam,]
    rsam <- sample(seq(1,nrow(sp4_str2),1), i, replace = F)
    sp4_str2_rs <- sp4_str2[rsam,]
    rsam <- sample(seq(1,nrow(sp4_str3),1), i, replace = F)
    sp4_str3_rs <- sp4_str3[rsam,]
    
    # join samples from all strata
    all_data <- rbind(sp1_str1_rs, sp1_str2_rs, sp1_str3_rs, sp2_str1_rs, sp2_str2_rs, sp2_str3_rs,
                      sp3_str1_rs, sp3_str2_rs, sp3_str3_rs, sp4_str1_rs, sp4_str2_rs, sp4_str3_rs)
    
    
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
    res_no_fixed_area_100[dummy,] <- c(i, i2, i5, i4, spearm, spearm2, rmse, rmse2, Rrmse1, Rrmse2, Nrmse1, Nrmse2, Bias1, Bias2, min_b, max_b, min_b2, max_b2)
    # increase dummy variable by 1
    dummy = dummy+1     
    
  }
  print(i)
}

head(res_no_fixed_area_100)

# save model results to file
setwd("E:/6_clean_version_new/6_clean_version/99_outputs")
save(res_no_fixed_area_100, file = "no_fixed_model_results_100m.RData")

# extract results
means <- list()
sds <- list()
dummy2=1
vals <- unique(res_no_fixed_area_100[,1])

# iterate through the results matrix and extract the means for the 1000 runs
# of each sample size
for (i in vals){
  
  # select subset of results depending on sample size
  sub <- res_no_fixed_area_100[res_no_fixed_area_100[,1]==i,]
  # calculate mean
  means[[dummy2]] <- colMeans(sub)
  sds[[dummy2]] <- colSds(sub)
  # increase dummy variable by 1
  dummy2 = dummy2+1
  
}

means_fin <- do.call(rbind, means)
sds_fin <- do.call(rbind, sds)

colnames(means_fin) <- c("sample_size", "iteration", "sampled area", "plot_size", "spearm_rf", "spearm_svm", "rmse_rf", "rmse_svm","Rrmse_rf", "Rrmse_svm","Bias1_rf","Bias2_svm","min_b","max_b")
colnames(sds_fin) <- c("sample_size", "iteration", "sampled area", "plot_size", "spearm_rf", "spearm_svm", "rmse_rf", "rmse_svm","Rrmse_rf", "Rrmse_svm","Bias1_rf","Bias2_svm","min_b","max_b")

####################################################################################
## plot mean and standard deviation of spearman of random forest regression results
####################################################################################
tar = 3
plot(seq(2,16,1), means_fin[,tar], ylim=c(0.4, 0.9), ylab=colnames(means_fin)[tar])
par(new=T)
plot(seq(2,16,1), means_fin[,tar]+sds_fin[,tar], ylim=c(0.4, 0.9), col="grey", ann=F)
par(new=T)
plot(seq(2,16,1), means_fin[,tar]-sds_fin[,tar], ylim=c(0.4, 0.9), col="grey", ann=F)


####################################################################################
## plot mean and standard deviation of spearman of support vector regression results
####################################################################################
tar = 4
plot(seq(2,16,1), means_fin[,tar], ylim=c(0.4, 0.9), ylab=colnames(means_fin)[tar])
par(new=T)
plot(seq(2,16,1), means_fin[,tar]+sds_fin[,tar], ylim=c(0.4, 0.9), col="grey", ann=F)
par(new=T)
plot(seq(2,16,1), means_fin[,tar]-sds_fin[,tar], ylim=c(0.4, 0.9), col="grey", ann=F)

####################################################################################
## plot mean and standard deviation of rmse of random forest regression results
####################################################################################
tar = 5
plot(seq(2,16,1), means_fin[,tar], ylim=c(20, 100), ylab=colnames(means_fin)[tar])
par(new=T)
plot(seq(2,16,1), means_fin[,tar]+sds_fin[,tar], ylim=c(20, 100), col="grey", ann=F)
par(new=T)
plot(seq(2,16,1), means_fin[,tar]-sds_fin[,tar], ylim=c(20, 100), col="grey", ann=F)

####################################################################################
## plot mean and standard deviation of rmse of support vector regression results
####################################################################################
tar = 6
plot(seq(2,16,1), means_fin[,tar], ylim=c(20, 100), ylab=colnames(means_fin)[tar])
par(new=T)
plot(seq(2,16,1), means_fin[,tar]+sds_fin[,tar], ylim=c(20, 100), col="grey", ann=F)
par(new=T)
plot(seq(2,16,1), means_fin[,tar]-sds_fin[,tar], ylim=c(20, 100), col="grey", ann=F)
####################################################################################
## plot mean and standard deviation of rrmse of random forest regression results
####################################################################################
tar = 7
plot(seq(2,16,1), means_fin[,tar], ylim=c(0.4, 0.9), ylab=colnames(means_fin)[tar])
par(new=T)
plot(seq(2,16,1), means_fin[,tar]+sds_fin[,tar], ylim=c(0.4, 0.9), col="grey", ann=F)
par(new=T)
plot(seq(2,16,1), means_fin[,tar]-sds_fin[,tar], ylim=c(0.4, 0.9), col="grey", ann=F)


####################################################################################
## plot mean and standard deviation of rrmse of support vector regression results
####################################################################################
tar = 8
plot(seq(2,16,1), means_fin[,tar], ylim=c(0.4, 0.9), ylab=colnames(means_fin)[tar])
par(new=T)
plot(seq(2,16,1), means_fin[,tar]+sds_fin[,tar], ylim=c(0.4, 0.9), col="grey", ann=F)
par(new=T)
plot(seq(2,16,1), means_fin[,tar]-sds_fin[,tar], ylim=c(0.4, 0.9), col="grey", ann=F)

####################################################################################
## plot mean and standard deviation of bias of random forest regression results
####################################################################################
tar = 9
plot(seq(2,16,1), means_fin[,tar], ylim=c(-20, 100), ylab=colnames(means_fin)[tar])
par(new=T)
plot(seq(2,16,1), means_fin[,tar]+sds_fin[,tar], ylim=c(-20, 100), col="grey", ann=F)
par(new=T)
plot(seq(2,16,1), means_fin[,tar]-sds_fin[,tar], ylim=c(-20, 100), col="grey", ann=F)

####################################################################################
## plot mean and standard deviation of bias of support vector regression results
####################################################################################
tar = 10
plot(seq(2,16,1), means_fin[,tar], ylim=c(-20, 100), ylab=colnames(means_fin)[tar])
par(new=T)
plot(seq(2,16,1), means_fin[,tar]+sds_fin[,tar], ylim=c(-20, 100), col="grey", ann=F)
par(new=T)
plot(seq(2,16,1), means_fin[,tar]-sds_fin[,tar], ylim=c(-20, 100), col="grey", ann=F)


