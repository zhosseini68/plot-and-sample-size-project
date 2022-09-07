setwd("E:/6_clean_version_new/6_clean_version/outputs")
load("anova_table_no_fixed.RData")
load("anova_table.RData")


all_data_total <- rbind(all_data_no_fixed, all_data)
colnames(all_data_total) <- c("sample_size", "iteration", "fixed_area", "plot_size", "spearm_rf", "spearm_svm", "rmse_rf", "rmse_svm", "Rrmse_rf", "Rrmse_svm", "Nrmse_rf", "Nrmse_val", "bias_rf", "bias_svm", "min_biom", "max_biom","min_val", "max_val")

fixed_area <- all_data_total[,3]

sample_size <- all_data_total[,1]


plot_size <- all_data_total[,4]


range_biom <- all_data_total[,16]-all_data_total[,15]

all_data_total
#calculate anova and store results to textfile  

anova_trees_spearm_r_total <- aov(all_data_total[,6] ~ (fixed_area+sample_size+plot_size+range_biom)^4)



sum_anova<-summary(anova_trees_spearm_r_total)
sum_anova

anova_trees_spearm_r_total

# print results to file
setwd("E:/6_clean_version_new/6_clean_version/outputs")

sink("ANOVA_r2_total_N.txt")
print(sum_anova)
sink()

anova_trees_RMSE_total <- aov(all_data_total[,8] ~ (fixed_area+sample_size+plot_size+range_biom)^4)


sum_anova<-summary(anova_trees_RMSE_total)
sum_anova

anova_trees_RMSE_total

# print results to file
setwd("E:/6_clean_version_new/6_clean_version/outputs")

sink("ANOVA_RMSE_total_N.txt")
print(sum_anova)
sink()

anova_trees_Rrmse_total <- aov(all_data_total[,10] ~ (fixed_area+sample_size+plot_size+range_biom)^4)

sum_anova<-summary(anova_trees_Rrmse_total)
sum_anova

anova_trees_Rrmse_total

# print results to file
setwd("E:/6_clean_version_new/6_clean_version/outputs")

sink("ANOVA_Rrmse_total_N.txt")
print(sum_anova)
sink()

anova_trees_Rrmse_total <- aov(all_data_total[,12] ~ (fixed_area+sample_size+plot_size+range_biom)^4)

sum_anova<-summary(anova_trees_Rrmse_total)
sum_anova

anova_trees_Rrmse_total

# print results to file
setwd("E:/6_clean_version_new/6_clean_version/outputs")

sink("ANOVA_Nrmse_total_N.txt")
print(sum_anova)
sink()
