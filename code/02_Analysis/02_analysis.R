###Load needed functions from "Functions.R" file.
source("code/02_Analysis/00_analysis_functions")
###load population data
# population <- readRDS("output/population.rds")

#------------- Compute Estimates from First Sample Data Frame List ------------#
pop_size <- nrow(population)
# Quality_overall <- list()
#----- Compute True Total Theta -----#
dayOne <- which(colnames(population) == "W1Day1")
Theta <- sum(population[,dayOne:ncol(population)])

#----- Survey Estimate -----#
N_svy <- estimate_totals(data_list = data_list,
                         pop_size = pop_size,
                         day1var = "Day1_svy",
                         day7var = "Day7_svy", 
                         response_var = "response_status_svy")

N_sen <- estimate_totals(data_list = data_list,
                         pop_size = pop_size,
                         day1var = "Day1_sen",
                         day7var = "Day7_sen",
                         response_var = "response_status_sen")

M <- estimate_overlap(data_list = data_list,
                      pop_size = pop_size,
                      day1var = "OverlapDay1",
                      day7var = "OverlapDay7")

Theta_LP <- N_svy * N_sen / M
Theta_LP
#----- LP Estimate -----#
PreparedData <- LinkAndPivotData(data_list)
Huggins_data_Prep <- PrepareHugginsData(PreparedData)
Huggins_data <- calculate_psi(Huggins_data_Prep)



Theta_HUG_GLM <- purrr::map(Huggins_data, ~ Hug_estimator_GLM(.x, 
                                                              pop_size = pop_size))
Theta_HUG_GLM <- unlist(Theta_HUG_GLM)
Theta_HUG_GLM

# XGBoost
Theta_HUG_XGB <- purrr::map(Huggins_data, ~ Hug_estimator_XGB(.x,
                                                              pop_size = pop_size))
Theta_HUG_XGB <- unlist(Theta_HUG_XGB)
Theta_HUG_XGB

# Create an output table
output <- data.frame(rho_svy = head(data_list[[1]]$response_rate_svy,
                                    length(data_list)),
                     fn_svy = head(data_list[[1]]$fn_svy, length(data_list)),
                     fp_svy = head(data_list[[1]]$fp_svy, length(data_list)),
                     rho_sen = head(data_list[[1]]$response_rate_sen,
                                    length(data_list)),
                     fn_sen = head(data_list[[1]]$fn_sen, length(data_list)),
                     fp_sen = head(data_list[[1]]$fp_sen, length(data_list)),
                     Theta = rep(Theta, length(data_list)),
                     ThetaHat_svy = N_svy,
                     Diff_svy = N_svy - Theta,
                     ThetaHat_LP = Theta_LP,
                     Diff_LP = Theta_LP - Theta,
                     ThetaHat_HUG_GLM = Theta_HUG_GLM,
                     Diff_HUG_GLM = Theta_HUG_GLM - Theta,
                     ThetaHat_HUG_XGB = Theta_HUG_XGB,
                     Diff_HUG_XGB = Theta_HUG_XGB - Theta)


quality_svy <- gen_quality_df(output_df = output,
                              estimator_name = "Survey",
                              theta_hat_values = N_svy,
                              theta = Theta)
quality_LP <- gen_quality_df(output_df = output,
                             estimator_name = "LP",
                             theta_hat_values = Theta_LP,
                             theta = Theta)
quality_HUG_GLM <- gen_quality_df(output_df = output,
                                  estimator_name = "HUG_GLM",
                                  theta_hat_values = Theta_HUG_GLM,
                                  theta = Theta)
quality_HUG_XGB <- gen_quality_df(output_df = output,
                                  estimator_name = "HUG_XGB",
                                  theta_hat_values = Theta_HUG_XGB,
                                  theta = Theta)

Quality_overall <- rbind(Quality_overall,quality_svy, quality_LP,
                         quality_HUG_GLM, quality_HUG_XGB)
View(Quality_overall)
# saveRDS(Quality_overall, file = "output/Quality_Table.rds")
# write.csv(Quality_overall, file = "output/Quality_Table.csv")
