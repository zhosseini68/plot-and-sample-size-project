setwd("E:/6_clean_version_new/6_clean_version/99_outputs/")
load("fixed_area_results_100m.RData")
load("fixed_area_results_300m.RData")
load("fixed_area_results_500m.RData")

all_data <- rbind(res_fixed_100, res_fixed_300, res_fixed_500)

colnames(all_data) <- c("sample_size", "iteration", "fixed_area", "plot_size", "spearm_rf", "spearm_val", "rmse_rf", "rmse_val", "Rrmse_rf", "Rrmse_svm", "Nrmse_rf", "Nrmse_val", "bias_rf", "bias_val", "min_biom", "max_biom","min_val", "max_val")



fixed_area <- all_data[,3]


sample_size <- all_data[,1]


plot_size <- all_data[,4]


range_biom <- all_data[,16]-all_data[,15]


setwd("E:/6_clean_version_new/6_clean_version/outputs")

save(all_data, file = "anova_table.RData")


load("anova_table.RData")

#calculate anova and store results to textfile  

anova_trees_spearm_r <- aov(all_data[,6] ~ (fixed_area+sample_size+plot_size+range_biom)^4)

#anova_trees_RMSE <- aov(RMSE ~ (fixed area+sample size+plot size+biomass values range)^4)
#anova_trees_Rrmse <- aov(RMSE/mean ~ (fixed area+sample size+plot size+biomass values range)^4)

sum_anova<-summary(anova_trees_spearm_r)
sum_anova

anova_trees_spearm_r

# print results to file
setwd("E:/6_clean_version_new/6_clean_version/outputs")

sink("ANOVA_r2_N.txt")
print(sum_anova)
sink()

anova_trees_RMSE <- aov(all_data[,8] ~ (fixed_area+sample_size+plot_size+range_biom)^4)


sum_anova<-summary(anova_trees_RMSE)
sum_anova

anova_trees_RMSE

# print results to file
setwd("E:/6_clean_version_new/6_clean_version/outputs")

sink("ANOVA_RMSE_N.txt")
print(sum_anova)
sink()

anova_trees_Rrmse <- aov(all_data[,10] ~ (fixed_area+sample_size+plot_size+range_biom)^4)

sum_anova<-summary(anova_trees_Rrmse)
sum_anova

anova_trees_Rrmse

# print results to file
setwd("E:/6_clean_version_new/6_clean_version/outputs")

sink("ANOVA_Rrmse_N.txt")
print(sum_anova)
sink()

anova_trees_Rrmse <- aov(all_data[,12] ~ (fixed_area+sample_size+plot_size+range_biom)^4)

sum_anova<-summary(anova_trees_Rrmse)
sum_anova

anova_trees_Rrmse

# print results to file
setwd("E:/6_clean_version_new/6_clean_version/outputs")

sink("ANOVA_Nrmse_N.txt")
print(sum_anova)
sink()
