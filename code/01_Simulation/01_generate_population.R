###Load needed functions from "Functions.R" file.
source("code/01_Simulation/00_simulation_funtions.R")

###Simulate covariates
covs <- sim_covs(n = 10000, seed = 1337) 

###create list to store each weeks data in:
weeks_list <- list() 

###set seed for reproducibility
seed <- 321
for(i in 1:13) { # iterate from 1 to 13 for each week of a quarter year
  set.seed(seed)
  # simulate one week of target data (i.e. outcome data for 7 days), 
  #based on the covariates from line 5
  tmp_week <- sim_week(covariates = covs, seed = seed, week = i)
  weeks_list[[i]] <- tmp_week # add this week data to the list
  seed <- seed + 1 # update seed (probably not needed?)
}

###Right now, weeks_list is of type list. Need to store and combine the elements
#of the list as a data frame using Reduce()
y_matrix <- Reduce(left_join, weeks_list)
###Link covariate data and response data by ID variables
population <- left_join(covs, y_matrix, by = "ID")

#rm(y_matrix, weeks_list, tmp_week, covs, seed, i)
### Save population data
saveRDS(population, file = "output/population.rds")
write.csv(population, file = "output/population.csv")

### Visualize the relationship between logit of probability and (relevant) predictors
# features <- paste0("X", 1:2)
# #Plot relevant features
# for (x in features) {
#   cat(x, "\n")
#   p <- ggplot(population, aes(x = get(x), y = log(probabilities/(1-probabilities)))) +
#     geom_point() +
#     labs(x = x)
#   print(p)
# }
# 
# # Visualize the interaction between predictor X1 and binary predictor X9 - if included
# ggplot(population, aes(x = X1, y = log(probabilities/(1-probabilities)), color = as.factor(X9))) +
#   geom_point() +
#   labs(x = "X1")
# 
# hist(population$probabilities, main = "Histogram of Probabilities", xlab = "Probability")
# summary(population$probabilities)
