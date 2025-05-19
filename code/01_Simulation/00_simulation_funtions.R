#------------------------------ Initial Settings ------------------------------# 
if(!require(tidyverse)){install.packages('tidyverse')}
library(tidyverse)
if(!require(purrr)){install.packages('purrr')}
library(purrr)
############################ Simulate Covariates ##############################

### Function to simulate covariates, to be used later to calculate the 
### probability of each being "on the street" for each unit 
sim_covs <- function(n, seed){
  ###Set seed for reproducability
  set.seed(seed)
  ###Create data similar to Friedman data: Covariates
  # 7 covariates drawn from a uniform distribution
  uniform_covariates <- matrix(runif(n * 7, min = 0, max = 2), nrow = n)
  # 2 covariates drawn from a Bernoulli distribution with p = 0.5
  categorical_covariates <- matrix(rbinom(n * 2, 1, 0.5), nrow = n)
  # Combine all covariates into one matrix
  X <- cbind(uniform_covariates, categorical_covariates)
  # Introduce imperfect multicolinearity feature
  X <- cbind(X, 2 * X[, 1] + rnorm(n, 0, 0.1))
  ###Create IDs based on population size
  ID <- 1:nrow(X)
  ###Create linear predictor 
  logit_p <- 4.5 - 3 *  X[, 1] - 4*X[, 9] + 2*X[, 1]*X[, 9] - X[, 2]^2
  ###Add noise
  logit_p <- logit_p + rnorm(n, 0, 0.1)
  ###Scale to ensure sigmoid works properly
  # logit_p_scaled <- scale(logit_p)
  ###Apply sigmoid function
  # probabilities <- 1 / (1 + exp(-logit_p_scaled))
  probabilities <- 1 / (1 + exp(-logit_p))
  ###Create data frame with ID, covariates, and response probabilities
  covariates <- data.frame(ID, X, probabilities)
  
  return(covariates)
}

############################## Simulate One Week ##############################

### Function to simulate one week of data. Will be called 13 times in Population.R
sim_week <- function(covariates, seed, week) {
  #covariates = data for the covariates
  #week = number of the week to simulate
  set.seed(seed) #for reproducibility
  ###Population size based on covariate data
  N <- nrow(covariates)
  ###Create list-object to store each days data in
  daily_list <- list()
  ###Generate binary response for all 7 days
  # N * 7 because of 7 days for each unit in the population
  # rep(covariates$probabilities, times = 7) means each unit has the same probability for each day
  y_matrix <- rbinom(N * 7, 1, rep(covariates$probabilities, times = 7))
  ###Reshape y_matrix into a N x 7 matrix (rows: individuals, columns: days)
  y_matrix <- matrix(y_matrix, nrow = N, ncol = 7, byrow = FALSE)
  # Set column names to WeekNumberDayNumber (e.g., W1Day1, W1Day2, ..., W1Day7)
  colnames(y_matrix) <- paste0("W", week, "Day", 1:7)
  # Combine responses and IDs into a data frame
  simulated_data <- data.frame(ID = covariates$ID, y_matrix)
  
  return(simulated_data)
}

###################### Draw a Sample From the Population ######################

### Function to include a sample indicator variable (1 being in the sample, otherwise 0)
### Also includes in which week each unit should be included in the sample
draw_sample <- function(pop_data, sampsize) {
  # pop = population data
  # sampsize = sample size
  
  if (!"sample_week" %in% colnames(pop_data)) {
    stop("The 'sample_week' column does not exist in the dataset.")
  }
  ### Here, I want to make sure each week has the same amount of units:
  ### Group by week variable and sample within each week
  unique_weeks <- unique(pop_data$sample_week) # 13 weeks
  sample_per_week <- round(sampsize / length(unique_weeks))  # sample size per week
  
  ### Create a sample indicator variable and initialize to 0 (not in sample)
  pop_data$in_sample <- 0
  
  sampled_pop <- pop_data %>% 
    group_by(sample_week) %>% # to ensure every week has ~ the same amount of units
    mutate(
      # ifselse assigns the value 1 to the variable "in_sample", based on the 
      # condition in it being ture, otherwise the value remains 0
      # row_number() checks whether the row number of each observation is part of a random sample
      # 1:n() generates a vector of integers from 1 to the number of rows in the current group (week)
      # min(...) randomly selects a subset of row numbers just generated, based on the smaller value
      # of sample_per_week (cf. line 79) or the total number of rows in the week (n()).
      in_sample = ifelse(row_number() %in% sample(1:n(), 
                                                  min(sample_per_week, n()), 
                                                  replace = FALSE), 1, in_sample)
    ) %>% 
    ungroup()
  
  ### Reorder columns such that sampweek is before the first day
  first_day <- which(colnames(sampled_pop) == "Day1")  # Find the index for the first day
  #Create new column order - (similar code will be used later as well to reorder variables)
  new_column_order <- c(colnames(sampled_pop)[1:(first_day - 1)], #keep variables up to first day
                        "in_sample", #insert "in_sample" variable next
                        #insert all variables including and following "first day" variable
                        colnames(sampled_pop)[first_day:(ncol(sampled_pop)-1)]) 
  sampled_pop <- sampled_pop[, new_column_order] #insert new column order
  
  return(sampled_pop)
}

################################# Extract Weeks ################################

### Each unit belongs to the sample in one of the 13 weeks (indicated by 
### variable "sample_week" cf. line 48 02_sample_scenario.R). This function extract
### this week from the population. This also reduces the size of the data frame.
extract_week <- function(pop_data) {
  # Identify the index of the first day column
  first_day <- which(colnames(pop_data) == "W1Day1")  # Find the index of the first day column
  
  # Use purrr::map to iterate over the rows
  # works as follows: for each row from 1 to the end of the df (nrow(pop_data))
  # apply a function to each row i - the function follows {
  extracted_data_list <- map(1:nrow(pop_data), function(i) {
    # Get the assigned week for this unit
    assigned_week <- pop_data$sample_week[i]
    
    # Create the column names for the assigned week
    week_columns <- paste0("W", assigned_week, "Day", 1:7)
    
    # Ensure the week data exists
    if (all(week_columns %in% colnames(pop_data))) {
      # Extract the week-specific data and auxiliary data (before the first day)
      # filter row i and all collumns in week_columns (cf. line 128) from pop_data
      week_data <- pop_data[i, week_columns, drop = FALSE]
      aux_data <- pop_data[i, 1:(first_day - 1), drop = FALSE]
      
      # Rename the week columns to "Day1", "Day2", ..., "Day7"
      colnames(week_data) <- paste0("Day", 1:7)
      
      # Combine auxiliary data and week data into a single data frame
      combined_data <- cbind(aux_data, week_data)
      
      return(combined_data)
    } else {
      warning(paste("Missing week data for sample unit", i))
      return(NULL)
    }
  })
  
  # Combine the list of data frames into a single data frame
  final_data <- bind_rows(extracted_data_list)
  
  return(final_data)
}

############################ Nonresponse Indicator ############################

######## NOT USED IN THIS PROJET #########

### This function includes a non-response indicator variable, both for the
### sensor and the survey data, based on the function arguments rr_sen, rr_svy
response_indicator <- function(pop_data, rr_sen, rr_svy) {
  #pop_data = population data
  #rr = response rate
  
  # Identify indices of sampled units for survey (only those that are in the sample)
  # cf. line 93 for creation of in_sample variable
  sampled_svy_indices <- which(pop_data$in_sample == 1)
  
  ###Calculate response size based on response rate and sample size
  response_size_svy <- ceiling(length(sampled_svy_indices) * rr_svy)
  response_size_sen <- ceiling(nrow(pop_data) * rr_sen)  
  
  ###Create indicator variable for respondent status (1 = respondent, 0 = not)
  ###First, initialize the response status as 0 (nonrespondent)
  pop_data$response_status_svy <- NA # NA, because most units are not in the sample, and thus can't be observed
  pop_data$response_status_sen <- 0 # 0, because sensor data is a census and all units are "in the sample"
  
  # Assign respondents for survey data (survey units that are in the sample)
  # randomly sample as many indices as determined by the response size (cf. line 169)
  response_indices_svy <- sample(sampled_svy_indices, response_size_svy, replace = FALSE)
  pop_data$response_status_svy[response_indices_svy] <- 1 # Assign 1 to respondents
  
  # Assign respondents based on sensor response rate
  # Randommy sample as many indices from the census, as determined by the response siye (cf. line 170)
  response_indices_sen <- sample(1:nrow(pop_data), response_size_sen, replace = FALSE)
  pop_data$response_status_sen[response_indices_sen] <- 1 # Assign 1 to respondents
  
  # Add the response rates to the data
  pop_data$response_rate_svy <- rr_svy
  pop_data$response_rate_sen <- rr_sen
  
  ###Find index for first day of first week and last day of last week
  first_day <- which(colnames(pop_data) == "Day1_sen") #Index for first day of first week
  
  ###Reorder colums so that "response_status" is placed before first day's data
  new_column_order <- c(colnames(pop_data)[1:(first_day-1)], "response_status_svy",
                        "response_status_sen", "response_rate_svy", "response_rate_sen",
                        colnames(pop_data)[first_day:(ncol(pop_data)-4)])
  pop_data <- pop_data[, new_column_order]
  
  return(pop_data)
  
}

########################### Introduce Underreporting ###########################

### This function includes both false-negative and false-positive rates for the survey data,
### based on the arguments fn_rate and fp_rate
inc_measurementerror_svy <- function(samp_data, fn_rate, fp_rate) {
  # samp_data = population data
  # fn_rate = false-negative rate as decimal number
  # fp_rate = false-positive rate as decimal number
  
  # Include error message if fn- or fp-rate is not between 0 and 1:
  if (fn_rate < 0 || fn_rate > 1) {
    stop("Error: false-negative rate (fn_rate) must be between 0 and 1.")
  } else if (fp_rate < 0 || fp_rate > 1) {
    stop("Error: false-positive rate (fp_rate) must be between 0 and 1.")
  }
  
  # Identify columns that correspond to the survey variable
  target_columns <- paste0("Day", 1:7, "_svy")
  
  for (i in seq_along(target_columns)) {
    # iterate through each column (i.e. day) in the vector target_columns of samp_data,
    # and apply the function following {
    # drop = FALSE ensures the results is returned as a dataframe, even if a single column is selected
    # , 1, makes sure to apply the function row-wise, not column-wise.
    samp_data[[target_columns[i]]] <- apply(samp_data[, target_columns[i], drop = FALSE], 1, function(x) {
      if (is.na(x)) {
        return(NA)  # Skip NA values and leave them unchanged
      }
      
      # False-negative: If value is 1 and the random number is less than the false-negative rate, set to 0
      if (x == 1 & runif(1) < fn_rate) {
        return(0)
      }
      
      # False-positive: If value is 0 and the random number is less than the false-positive rate, set to 1
      if (x == 0 & runif(1) < fp_rate) {
        return(1)
      }
      
      # If neither FN nor FP condition is met, keep the original value
      return(x)
    })
  }

  # Add the false-negative and false-positive rates as metadata
  samp_data$fn_svy <- fn_rate
  samp_data$fp_svy <- fp_rate
  
  return(samp_data)
}
  


### Basically the same function as above, but now for the sensor data
inc_measurementerror_sen <- function(samp_data, fn_rate, fp_rate) {
  #samp_data = population data
  #fn_rate = false-negative rate as decimal number
  #fp_rate = false-positive rate as decimal number
  
  ### Include error message if fn- or fp-rate is not between 0 and 1:
  if (fn_rate < 0 || fn_rate > 1) {
    stop("Error: false-negative rate (fn_rate) must be between 0 and 1.")
  } else if (fp_rate < 0 || fp_rate > 1) {
    stop("Error: false-positive rate (fp_rate) must be between 0 and 1.")
  }
  
  # Identify columns that correspond to target variable
  target_columns <- paste0("Day", 1:7, "_sen")
  
  
  # Introduce underreporting (false-negative)
  for (i in seq_along(target_columns)) {
    samp_data[[target_columns[i]]] <- apply(samp_data[, target_columns[i], drop = FALSE], 1, function(x) {
      if (is.na(x)) {
        return(NA)  # Skip NA values and leave them unchanged
      }
      
      # False-negative: If value is 1 and the random number is less than the false-negative rate, set to 0
      if (x == 1 & runif(1) < fn_rate) {
        return(0)
      }
      
      # False-positive: If value is 0 and the random number is less than the false-positive rate, set to 1
      if (x == 0 & runif(1) < fp_rate) {
        return(1)
      }
      
      # If neither FN nor FP condition is met, keep the original value
      return(x)
    })
  }
  
  samp_data$fn_sen <- fn_rate
  samp_data$fp_sen <- fp_rate
  
  return(samp_data)
}
