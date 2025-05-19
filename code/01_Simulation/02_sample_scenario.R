###Load needed functions from "Functions.R" file.
source("code/01_Simulation/00_simulation_functions.R")
if(!require(parallel)){install.packages('parallel')}
library(parallel)
###load population data
# population <- readRDS("data/basic_population.rds")

#----------------- Repeat Sampling and Estimation 1000 Times -----------------# 
###Set number of simulations/iteration for this simulation scenario:
num_simulations <- 1

###Initialize a list to store the sample data for each iteration
data_list <- list()

###Extract the population size (needed later for weighting)
pop_size <- nrow(population)

###Start measuring time
start_time <- Sys.time()

###Introduce parallelization
cl <- makeCluster(detectCores() -1)

clusterExport(cl, list("population", "draw_sample", "response_indicator", 
                       "extract_week", "inc_measurementerror_svy", 
                       "inc_measurementerror_sen", "num_simulations"))
###Make sure each core loads the necessary functions
clusterEvalQ(cl, {
  library(purrr)  
  library(tidyverse)  
})

data_list <- parLapply(cl, 1:num_simulations, function(iteration) {
  
  ### Set a new seed for each iteration
  seed <- (1337 + iteration)
  set.seed(seed)
  
  ### We do not want to change the population, since this is the input for each iteration
  pop <- population
  popsize <- nrow(pop) # Extract population size
  
  ###Step 1: Simulate in which week the unit is included in the survey
  pop$sample_week <- sample(rep(1:13, length.out = popsize))
  
  # Reorder the columns, such that the sample_week indicator is before the target data
  first_day <- which(colnames(pop) == "W1Day1")  # Find the index for the first day
  new_column_order <- c(colnames(pop)[1:(first_day - 1)], "sample_week", 
                        colnames(pop)[first_day:(ncol(pop)-1)])
  pop <- pop[, new_column_order]

  ### Step 2: Extract sample weeks: Only keep the target data for the specific week from step 1
  pop <- extract_week(pop)
  
  ### Step 3: Draw sample for the survey data
  pop <- draw_sample(pop = pop, sampsize = 2000)
  
  ### Step 4: Copy sample data as survey data - we introduce measurement error later, 
  # thus sensor and survey data currently both show the true values
  pop <- pop %>%
    mutate(
      Day1_svy = if_else(in_sample == 1, Day1, NA_real_),
      Day2_svy = if_else(in_sample == 1, Day2, NA_real_),
      Day3_svy = if_else(in_sample == 1, Day3, NA_real_),
      Day4_svy = if_else(in_sample == 1, Day4, NA_real_),
      Day5_svy = if_else(in_sample == 1, Day5, NA_real_),
      Day6_svy = if_else(in_sample == 1, Day6, NA_real_),
      Day7_svy = if_else(in_sample == 1, Day7, NA_real_)
    )
  
  ###Rename sensor data
  for (i in 1:7) {
    # within colnames(pop), replace every variable that starts with "Day" and ends with "i",
    # with "Dayi_sen", i running from 1 to 7
    colnames(pop) <- sub(paste0("^Day", i, "$"), # "pattern"
                                paste0("Day", i, "_sen"), # replacement
                                colnames(pop)) # vector to look through
  }
  # define sensor and survey variable names - to be used later
  sen_columns <- paste0("Day", 1:7, "_sen")
  svy_columns <- paste0("Day", 1:7, "_svy")
  
  ### Step 5: Include response indicator (cf. Functions.R lines 156)
  pop <- response_indicator(pop_data = pop, rr_sen = 1, rr_svy = 1)
  
  ### Setp 6: Set Nonrespondent-data to NA
  # for survey data, nonresponse indicator was initialized with NA: 
  # every value that is neither already NA nor 1 gets replaced by NA
  pop[is.na(pop$response_status_svy) | 
        pop$response_status_svy != 1, svy_columns] <- NA
  # for sensor data, 0 indicates nonresponse. Thus, replacement target variable 
  # for nonrespondents with NA
  pop[pop$response_status_sen == 0, sen_columns] <- NA

  
  ### Step 7: Apply Underreporting Survey
  dayOneSvy <- which(colnames(pop) == "Day1_svy") # index of first survey target variable
  dayLastSvy <- which(colnames(pop) == "Day7_svy") # index of last survey target variable
  # Calculate the total number of days this vehicle was on the street, before including measurement error
  pop$Svy_Total_TRUE <- rowSums(pop[, dayOneSvy:dayLastSvy])
  # Apply function which introduces measurement error for survey data
  pop <- inc_measurementerror_svy(samp_data = pop, 
                                          fn_rate = 0, # false-negative rate
                                          fp_rate = 0) # false-positive rate
  # Calculate the total number of days this vehicle was reported as being on the street, 
  # after including measurement error
  pop$Svy_Total_Err <- rowSums(pop[, dayOneSvy:dayLastSvy])

  
  ### Step 8: Apply Underreporting Sensor
  dayOneSen <- which(colnames(pop) == "Day1_sen") # index of first sensor target variable
  dayLastSen <- which(colnames(pop) == "Day7_sen") # index of last sensor target variable
  # Calculate the total number of days this vehicle was on the street, before including measurement error
  pop$Sen_Total_True <- rowSums(pop[, dayOneSen:dayLastSen])
  # Apply function which introduces measurement error for sensor data
  pop <- inc_measurementerror_sen(samp_data = pop,
                                          fn_rate = 0, # false-negative rate
                                          fp_rate = 0) # false-positive rate
  # Calculate the total number of days this vehicle was detected to be on the street, 
  # after including measurement error
  pop$Sen_Total_Err <- rowSums(pop[, dayOneSen:dayLastSen])


  ### Step 9: Create Overlap Columns
  pop$OverlapDay1 <- pop$Day1_svy * pop$Day1_sen
  pop$OverlapDay2 <- pop$Day2_svy * pop$Day2_sen
  pop$OverlapDay3 <- pop$Day3_svy * pop$Day3_sen
  pop$OverlapDay4 <- pop$Day4_svy * pop$Day4_sen
  pop$OverlapDay5 <- pop$Day5_svy * pop$Day5_sen
  pop$OverlapDay6 <- pop$Day6_svy * pop$Day6_sen
  pop$OverlapDay7 <- pop$Day7_svy * pop$Day7_sen
  
  ### Return the modified data for this iteration
  return(pop)
})


stopCluster(cl)

# Measure the end time
end_time <- Sys.time()

execution_time <- end_time - start_time
print(paste("Total time taken for simulations:", execution_time))