setwd("E:/6_clean_version_new/6_clean_version/99_outputs")

# save model results to file
load("no_fixed_model_results_100m.RData")
load("no_fixed_model_results_300m.RData")
load("no_fixed_model_results_500m.RData")

all_data_no_fixed <- rbind(res_no_fixed_area_100, res_no_fixed_area_300, res_no_fixed_area_500)

colnames(all_data_no_fixed) <- c("sample_size", "iteration", "sampled_area", "plot_size", "spearm_rf", "spearm_val", "rmse_rf", "rmse_val", "Rrmse_rf", "Rrmse_val", "Nrmse_rf", "Nrmse_val", "bias_rf", "bias_val", "min_biom", "max_biom","min_val", "max_val")

sampled_area <- all_data_no_fixed[,3]


sample_size <- all_data_no_fixed[,1]


plot_size <- all_data_no_fixed[,4]


range_biom <- all_data_no_fixed[,16]-all_data_no_fixed[,15]


setwd("E:/6_clean_version_new/6_clean_version/outputs")

save(all_data_no_fixed, file = "anova_table_no_fixed.RData")
load("anova_table_no_fixed.RData")

#calculate anova and store results to textfile  

anova_trees_spearm_r <- aov(all_data_no_fixed[,6] ~ (sampled_area+sample_size+plot_size+range_biom)^4)

#anova_trees_RMSE <- aov(RMSE ~ (fixed area+sample size+plot size+biomass values range)^4)
#anova_trees_Rrmse <- aov(RMSE/mean ~ (fixed area+sample size+plot size+biomass values range)^4)

sum_anova<-summary(anova_trees_spearm_r)
sum_anova

anova_trees_spearm_r

# print results to file
setwd("E:/6_clean_version_new/6_clean_version/outputs")

sink("ANOVA_r2_no_fixed_N.txt")
print(sum_anova)
sink()

anova_trees_RMSE <- aov(all_data_no_fixed[,8] ~ (sampled_area+sample_size+plot_size+range_biom)^4)


sum_anova<-summary(anova_trees_RMSE)
sum_anova

anova_trees_RMSE

# print results to file
setwd("E:/6_clean_version_new/6_clean_version/outputs")

sink("ANOVA_RMSE_no_fixed_N.txt")
print(sum_anova)
sink()

anova_trees_Rrmse <- aov(all_data_no_fixed[,10] ~ (sampled_area+sample_size+plot_size+range_biom)^4)

sum_anova<-summary(anova_trees_Rrmse)
sum_anova

anova_trees_Rrmse

# print results to file
setwd("E:/6_clean_version_new/6_clean_version/outputs")

sink("ANOVA_Rrmse_no_fixed_N.txt")
print(sum_anova)
sink()

anova_trees_Rrmse <- aov(all_data_no_fixed[,12] ~ (sampled_area+sample_size+plot_size+range_biom)^4)

sum_anova<-summary(anova_trees_Rrmse)
sum_anova

anova_trees_Rrmse

# print results to file
setwd("E:/6_clean_version_new/6_clean_version/outputs")

sink("ANOVA_Nrmse_no_fixed_N.txt")
print(sum_anova)
sink()
