#------------------------------ Initial Settings ------------------------------# 
if(!require(tidyverse)){install.packages('tidyverse')}
library(tidyverse) # for basic data wrangling & manipulation
if(!require(purrr)){install.packages('purrr')}
library(purrr) # to map over lists (of data frames)
if(!require(ggplot2)){install.packages('ggplot2')}
library(ggplot2) # Visualize results later
if(!require(MASS)){install.packages('MASS')}
library(MASS) # StepAIC function
if(!require(xgboost)){install.packages('xgboost')}
library(xgboost) # to run XGBoost model
if(!require(ggtext)){install.packages('ggtext')}
library(ggtext) # include latex expression in ggplot
if(!require(showtext)){install.packages('showtext')}
library(showtext) # include latex expression in ggplot
if(!require(sysfonts)){install.packages('sysfonts')}
library(sysfonts) # include latex expression in ggplot
#---------------------------------- Functions ---------------------------------# 
###Compute the population totals from a list of data frames
estimate_totals <- function(data_list, pop_size, day1var, day7var, response_var) {
  # day1_var & day7_var = first and last target variable (variables to sum over)
  # response_var = indicator variable for being a respondent (1) or not (0)
  est_pop_totals <- purrr::map(data_list, ~ { # map over the list of data frames
    
    ### Calculate number of respondents - sum of response indicator being 1 (indicating response)
    num_respondents <- sum(.x[[response_var]] == 1, na.rm = TRUE)
    
    ### Estimate the population
    .x %>%
      # only use the subset of respondents
      filter(.data[[response_var]] == 1) %>%
      # select all variables between the first and last target variable (i.e. Day 1 to Day 7)
      dplyr::select(!!sym(day1var):!!sym(day7var)) %>%
      # calculate the sum of all target variables 
      # multiply by factor 13 to extrapolate from a weekly to a quarter year estimate 
      sum(na.rm = TRUE) * pop_size / num_respondents * 13
    
  })
  #return a vector of estimated totals
  return(unlist(est_pop_totals))
}


estimate_overlap <- function(data_list, pop_size, day1var, day7var) {
  # data_list = object of type list, including multiple data frames
  # pop_size = size of the population
  # day1_var & day7_var = first and last target variable (variables to sum over)
  # response_var = indicator variable for being a respondent (1) or not (0)
  est_pop_totals <- purrr::map(data_list, ~ { # map over the list of data frames
    
    ### Calculate number of respondents - sum of response indicator being 1 (indicating response)
    num_respondents <- sum(complete.cases(.x[, c(day1var, day7var)]))
    
    ### Estimate the population
    .x %>%
      # select all variables between the first and last target variable (i.e. Day 1 to Day 7)
      dplyr::select(!!sym(day1var):!!sym(day7var)) %>%
      # calculate the sum of all target variables 
      # mulitply by factor 13 to extrapolate from a weekly to a quarter year estimate 
      sum(na.rm = TRUE) * pop_size / num_respondents * 13
    
  })
  #return a vector of estimated totals
  return(unlist(est_pop_totals))
}



LinkAndPivotData <- function(data_list) {
  
  data_long_list <- lapply(data_list, function(data) {
    data <- data %>% 
      filter(response_status_svy == 1 & response_status_sen == 1) %>% # Only need vehicles in both sources
      # indicator for  k-fold cross validation - i (with i = 1:5) indicates in which
      # fold this unit should be included in the test data, otherwise training data
      mutate(fold = rep(1:5, length.out = n()))
    
    #pivot survey data into long format
    long_data_svy <- data %>% 
      # select variables of interest
      dplyr::select(ID, starts_with("X"), probabilities, response_status_svy, 
                    response_status_sen, response_rate_sen, response_rate_svy, fold, 
                    fn_svy, fp_svy, fn_sen, fp_sen, paste0("Day", 1:7, "_svy")) %>% 
      # pivot to long format
      pivot_longer(cols = paste0("Day", 1:7, "_svy"), # variables to pivot into long-format
                   names_to = "Day", #name of the new variable "Day"
                   values_to = "delta_svy")  %>% # variable name of the values of Day 1 to 7
      mutate(Day = gsub("_svy", "", Day))# %>% #Remove "_svy" suffix

    ### repeat for sensor data
    long_data_sen <- data %>% 
      dplyr::select(ID, starts_with("X"), probabilities, response_status_svy, 
                    response_status_sen, response_rate_sen, response_rate_svy, fold, 
                    fn_svy, fp_svy, fn_sen, fp_sen, paste0("Day", 1:7, "_sen")) %>% 
      pivot_longer(cols = paste0("Day", 1:7, "_sen"),
                   names_to = "Day",
                   values_to = "delta_sen")  %>%
      mutate(Day = gsub("_sen", "", Day)) # %>% #Remove "_sen" suffix

    # inner_join ensures only vehicles common to both sources are included
    long_data <- inner_join(long_data_svy, 
                            long_data_sen, 
                            by = c("ID", paste0("X", 1:10), "probabilities", 
                                   "response_status_svy", "response_status_sen",
                                   "response_rate_sen", "response_rate_svy",
                                   "fold", "fn_svy", "fp_svy", "fn_sen", 
                                   "fp_sen", "Day"))
    
    # calculate the number of respondents common to both data sources
    num_com_resp <- n_distinct(long_data$ID)
    
    long_data <- long_data %>% #Remove all-zero rows (didn't catch those vehicles)
      filter(!(delta_svy == 0 & delta_sen == 0)) 
    
    list(long_data = long_data, num_com_resp = num_com_resp)
  })
  
  return(data_long_list)
  
}

PrepareHugginsData <- function(data_list, folds = 5, features = paste0("X", 1:10)) {
  # object of type list including all data frames for this simulation scenario
  # folds = indicating k of k-fold cross-validation
  # features = auxiliary variables included in data set (and to be used in models)
  # Ensure that the input is a list
  if (!is.list(data_list)) {
    stop("Error: The input should be a list of data frames.")
  }
  
  # Apply the pivot_data function to each data frame in the list
  data_long_list <- lapply(data_list, function(data_list) {
    
    data <- data_list$long_data
    num_respondents <- data_list$num_com_resp
    
    
    #---------------- Estimate Conditional Capture Probability ----------------#
    # Initialize a list object to store the results of each fold in
    fold_results <- list()
    for (i in 1:folds) {
      # Split data into training and test data for both datasets
      train_data_svy <- data %>% 
        # choose all vehicles that do not belong to the test data (i.e. fold = i)
        filter(fold != i & delta_sen == 1) # conditional on being captured in other (sensor) source
      test_data_svy <- data %>% 
        filter(fold == i) 
      
      ### Repeat for sensor data
      train_data_sen <- data %>% 
        filter(fold != i & delta_svy == 1) # conditional on being captured in other (surve) source
      test_data_sen <- data %>% 
        filter(fold == i)
      
      ###Apply logistic regression on survey data
      #Apply initial intercept only model
      m0_svy <- glm(delta_svy ~ 1, 
                    family = binomial(link = "logit"),data = train_data_svy)
      mfull_svy <- as.formula(paste("delta_svy ~", paste(features, 
                                                         collapse = "+")))
      #Stepwise model selection based on BIC
      msel_svy <- stepAIC(object = m0_svy, 
                          scope = mfull_svy,
                          direction = "both",
                          trace = 1,
                          steps = 1000,
                          k = log(nrow(train_data_svy))) # use BIC instead of AIC
      
      predictions_svy <- predict(msel_svy, test_data_svy, type = "response")
      predictions_svy_null <- predict(m0_svy, test_data_svy, type = "response")

      
      ###Apply logistic regression on sensor data
      m0_sen <- glm(delta_sen ~ 1, 
                    family = binomial(link = "logit"), data = train_data_sen)
      mfull_sen <- as.formula(paste("delta_sen ~", paste(features, 
                                                         collapse = "+")))
      msel_sen <- stepAIC(object = m0_sen, 
                          scope = mfull_sen, 
                          direction = "both",
                          trace = 1, 
                          steps = 1000,
                          k = log(nrow(train_data_sen)))
      
      predictions_sen <- predict(msel_sen, test_data_sen, type = "response")
      predictions_sen_null <- predict(m0_sen, test_data_sen, type = "response")

      #---------------------------- Apply XGBoost ----------------------------#
      ###Prepare data for XGBoost
      #For survey data
      train_data_svy_clean <- data %>%
        filter(fold != i & delta_sen == 1) # conditional on being captured in other source
      test_data_svy_clean <- data %>%
        filter(fold == i)
      
      train_data_sen_clean <- data %>%
        filter(fold != i & delta_svy == 1)
      test_data_sen_clean <- data %>%
        filter(fold == i)
      
      # Prepare XGBoost matrices after cleaning the target variables
      dtrain_svy <- xgb.DMatrix(data = as.matrix(train_data_svy_clean[, features]),
                                label = train_data_svy_clean$delta_svy)
      dtrain_svy_intercept <- xgb.DMatrix(data = matrix(0, nrow = nrow(train_data_svy_clean), 
                                                        ncol = length(features)),
                                          label = train_data_svy_clean$delta_svy)
      dtest_svy <- xgb.DMatrix(data = as.matrix(test_data_svy_clean[, features]),
                               label = test_data_svy_clean$delta_svy)
      
      dtrain_sen <- xgb.DMatrix(data = as.matrix(train_data_sen_clean[, features]),
                                label = train_data_sen_clean$delta_sen)
      dtrain_sen_intercept <- xgb.DMatrix(data = matrix(0, nrow = nrow(train_data_sen_clean), 
                                                        ncol = length(features)),
                                          label = train_data_sen_clean$delta_sen)
      dtest_sen <- xgb.DMatrix(data = as.matrix(test_data_sen_clean[, features]),
                               label = test_data_sen_clean$delta_sen)
      
      ###Define parameters for the XGBoost model
      params <- list(
        booster = "gbtree",
        objective = "binary:logistic", #Binary classification problem
        eval_metric = "logloss", #Log-loss as evaluation metric
        eta = 0.3, #Learning rate
        max_depth = 1 #Max depth of trees
      )
      
      #Train survey model
      XGBmodel_svy <- xgb.train(
        data = dtrain_svy,
        params = params,
        nrounds = 50 #Number of boosting rounds
      )
      #Train sensor model
      XGBmodel_sen <- xgb.train(
        data = dtrain_sen,
        params = params,
        nround = 50
      )
      
      #Predict on test data
      XGB_predictions_svy <- predict(XGBmodel_svy, dtest_svy)
      XGB_predictions_sen <- predict(XGBmodel_sen, dtest_sen)
      
      #Train survey model
      XGBmodel_svy_null <- xgb.train(
        data = dtrain_svy_intercept,
        params = params,
        nrounds = 100 #Number of boosting rounds
      )
      #Train sensor model
      XGBmodel_sen_null <- xgb.train(
        data = dtrain_sen_intercept,
        params = params,
        nround = 100
      )
      
      XGB_predictions_svy_null <- predict(XGBmodel_svy_null, dtest_svy)
      XGB_predictions_sen_null <- predict(XGBmodel_sen_null, dtest_sen)
      #---------------------- Create Output Data Frames ----------------------# 
      fold_results[[i]] <- list(
        logreg_survey_data = data.frame(
          ID = test_data_svy$ID,
          Day = test_data_svy$Day,
          logreg_predictions_svy = predictions_svy,
          logreg_pred_svy_null = predictions_svy_null,
          model_svy = rep(deparse(msel_svy$formula), times = nrow(test_data_svy))
        ),
        XGB_survey_data = data.frame(
          ID = test_data_svy_clean$ID,
          Day = test_data_svy_clean$Day,
          XGB_predictions_svy = XGB_predictions_svy,
          XGB_predictions_svy_null = XGB_predictions_svy_null
        ),
        logreg_sensor_data = data.frame(
          ID = test_data_sen$ID,
          Day = test_data_sen$Day,
          logreg_predictions_sen = predictions_sen,
          logreg_pred_sen_null = predictions_sen_null,
          model_sen = rep(deparse(msel_sen$formula), times = nrow(test_data_sen))
        ),
        XGB_sensor_data = data.frame(
          ID = test_data_sen_clean$ID,
          Day = test_data_sen_clean$Day,
          XGB_predictions_sen = XGB_predictions_sen,
          XGB_predictions_sen_null = XGB_predictions_sen_null
        ),
        # include the number of respondents for this data set
        num_respondents = num_respondents
      )
      
    }
    
    return(fold_results)
  })
  
  return(data_long_list)
}


### Function which calculates psi and psi inverse (also for GLM null model) in each data frame
calculate_psi <- function(data_list) {
  ### Initialze a list object to store the results in
  final_results <- list()
  
  #Iterate through each simulation scenario
  for (R in 1:length(data_list)) {
    
    logreg_survey_data_all <- data.frame()
    logreg_sensor_data_all <- data.frame()
    XGB_survey_data_all <- data.frame()
    XGB_sensor_data_all <- data.frame()
    
    # Extract number of respondents for entire data set (R), once per simulation scenario
    num_respondents <- data_list[[R]][[1]]$num_respondents
    
    ###Iterate through each fold
    for (k in 1:length(data_list[[R]])) {
      
      logreg_survey_data <- data_list[[R]][[k]]$logreg_survey_data
      logreg_sensor_data <- data_list[[R]][[k]]$logreg_sensor_data
      XGB_survey_data <- data_list[[R]][[k]]$XGB_survey_data
      XGB_sensor_data <- data_list[[R]][[k]]$XGB_sensor_data
      num_respondents <- data_list[[R]][[k]]$num_respondents
      
      # Bind survey/sensor data from each fold
      logreg_survey_data_all <- bind_rows(logreg_survey_data_all, logreg_survey_data)
      logreg_sensor_data_all <- bind_rows(logreg_sensor_data_all, logreg_sensor_data)
      XGB_survey_data_all <- bind_rows(XGB_survey_data_all, XGB_survey_data)
      XGB_sensor_data_all <- bind_rows(XGB_sensor_data_all, XGB_sensor_data)
      
    }
    
    # Merge the comdined survey and sensor folds into one data frame
    logreg_merged_data <- full_join(logreg_survey_data_all, logreg_sensor_data_all,
                                    by = c("ID", "Day")) %>% 
      arrange(ID, Day)
    ### Repeat for XGBoost data
    XBG_merged_data <- full_join(XGB_survey_data_all, XGB_sensor_data_all,
                                 by = c("ID", "Day")) %>%
      arrange(ID, Day)
    
    # Calculate (inverse) capture probability
    logreg_merged_data <- logreg_merged_data %>% 
      mutate(
        # Calculate psi_{i,j}
        psi = 1 - ((1 - logreg_predictions_svy)*(1 - logreg_predictions_sen)),
        
        # Calculate inverse of psi
        psi_inv = 1 / psi,
        
        # Repeat for null model
        psi_null = 1 - ((1 - logreg_pred_svy_null)*(1 - logreg_pred_sen_null)),
        
        psi_inv_null = 1 / psi_null
      )
        
    
    XBG_merged_data <- XBG_merged_data %>%
      mutate(
        # Ensure NA values are replaced with 0s
        predictions_svy = ifelse(is.na(XGB_predictions_svy), 0,
                                 XGB_predictions_svy),
        predictions_sen = ifelse(is.na(XGB_predictions_sen), 0,
                                 XGB_predictions_sen),
        
        # Calculate psi_{i,j}
        psi = 1 - ((1 - predictions_svy)*(1 - predictions_sen)),
        
        # Calculate inverse of psi
        psi_inv = 1 / psi
      )
    
    # Store the processed result for each fold (R)
    final_results[[R]] <- list(
      logreg_data = logreg_merged_data,
      XGB_data = XBG_merged_data,
      num_respondents = num_respondents
    )
  }
  
  return(final_results)
}

# Function to calculate the Huggins estimator
Hug_estimator_GLM <- function(data, pop_size) {
  # 13 to extrapolate from a weekly to a quarterly estimate
  # (pop_size / data$num_respondents) is the weight (population size over number of respondents in both sources)
  # sum(data$logreg_data$psi_inv, na.rm = TRUE) = the sum of each predicted capture probability
  Huggins <- sum(data$logreg_data$psi_inv, na.rm = TRUE) * 13 * (pop_size / data$num_respondents)
  return(Huggins)
}

Hug_estimator_XGB <- function(data, pop_size) {
  # 13 to extrapolate from a weekly to a quarterly estimate
  # (N / length(unique(data$ID))) is the weight (population size over number of respondents in both sources)
  # sum(data$psi_inv, na.rm = TRUE) = the sum of each predicted capture probability
  Huggins <- sum(data$XGB_data$psi_inv, na.rm = TRUE) * 13 * (pop_size / data$num_respondents)
  return(Huggins)
}


### Function to extract an estimator quality data frame from the output
gen_quality_df <- function(output_df, estimator_name, theta_hat_values, theta = Theta) {
  quality_df <- data.frame(
    Estimator = estimator_name,
    Rho_svy = head(output_df$rho_svy, 1),
    fn_svy = head(output_df$fn_svy, 1),
    fp_svy = head(output_df$fp_svy, 1),
    Rho_sen = head(output_df$rho_sen, 1),
    fn_sen = head(output_df$fn_sen, 1),
    fp_sen = head(output_df$fp_sen, 1),
    Theta = theta,
    ThetaHat = mean(theta_hat_values),
    SE = sd(theta_hat_values),
    RB = (mean(theta_hat_values) - theta) / theta,
    CV = sd(theta_hat_values) / theta,
    RRMSE = sqrt(mean((theta_hat_values - theta)^2)) / theta
  )
  
  return(quality_df)
}
