source("code/02_Analysis/00_analysis_functions.R")

dat <- data_list[[1]]

### Prepare and Pivot the data
data <- dat %>% 
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
# filter(!is.na(delta_svy)) # only choose respondents

### repeat for sensor data
long_data_sen <- data %>% 
  dplyr::select(ID, starts_with("X"), probabilities, response_status_svy, 
                response_status_sen, response_rate_sen, response_rate_svy, fold, 
                fn_svy, fp_svy, fn_sen, fp_sen, paste0("Day", 1:7, "_sen")) %>% 
  pivot_longer(cols = paste0("Day", 1:7, "_sen"),
               names_to = "Day",
               values_to = "delta_sen")  %>%
  mutate(Day = gsub("_sen", "", Day)) # %>% #Remove "_sen" suffix
# filter(!is.na(delta_sen))

# inner_join ensures only vehicles common to both sources are included
long_data <- inner_join(long_data_svy, 
                        long_data_sen, 
                        by = c("ID", paste0("X", 1:10), "probabilities", 
                               "response_status_svy", "response_status_sen",
                               "response_rate_sen", "response_rate_svy",
                               "fold", "fn_svy", "fp_svy", "fn_sen", 
                               "fp_sen", "Day")) %>%
  filter(!(delta_svy == 0 & delta_sen == 0))

features <- paste0("X", 1:10)

# hyperparameters
learningrates <- c(0.01, 0.03, 0.1, 0.3)  # learning rates (default 0.3)
maxdepths <- 1:3  # maximal tree depths (default 6)
nrounds <- c(10, 20, 50, 100, 300)  # aantal bomen

###----------------- Hyperparameter tuning for survey data
train_data_svy <- long_data %>% 
  filter(fold != 1, delta_sen == 1)
test_data_svy <- long_data %>% 
  filter(fold == 1)

train_data_svy <- train_data_svy %>% 
  slice_sample(prop = 1) %>%
  mutate(valset = rep(1:V, length.out = n()))

obs <- test_data_svy$delta_svy

cat("", file = "output/survey_log.txt")
t0 <- proc.time()[[3]]

# Initialize an empty list to store the results (data frame for nll values)
results_survey <- data.frame(
  valset = integer(0),
  learningrate = numeric(0),
  maxdepth = integer(0),
  nrounds = integer(0),
  nll = numeric(0)
)

for (v in 1:V) {
  
  # prevalence in training set for class weights and benchmarking of model performance
  alpha_trn <- train_data_svy %>%
    filter(valset != v) %>%
    summarise(alpha_trn = sum(delta_svy) / n()) %>% 
    pull(alpha_trn)
  
  trn <- train_data_svy %>%
    filter(valset != v) %>%
    dplyr::select(all_of(features)) %>%
    as.matrix() %>%
    xgb.DMatrix(data = ., 
                label = train_data_svy %>% filter(valset != v) %>% pull(delta_svy))
  
  classweight <- train_data_svy %>%
    filter(valset != v) %>%
    mutate(classweight = ifelse(delta_svy == 1, 1 / (2 * alpha_trn), 1 / (2 * (1 - alpha_trn)))) %>%
    pull(classweight)
  
  val_x <- train_data_svy %>%
    filter(valset == v) %>%
    dplyr::select(all_of(features)) %>%
    as.matrix() %>%
    xgb.DMatrix(data = .)
  
  val_y <- train_data_svy %>%
    filter(valset == v) %>%
    pull(delta_svy)
  
  for (lr in learningrates) {
    for (md in maxdepths) {
      for (n in nrounds) {
        
        cat(paste(v, lr, md, n, (proc.time()[[3]] - t0) / 60, "\n"), 
            file = "output/survey_log.txt", append = TRUE)
        
        # train model
        mdl <- xgboost(data = trn,
                       eta = lr,
                       max_depth = md,
                       nrounds = n,
                       objective = "binary:logistic",
                       weight = classweight,
                       early_stopping_rounds = 20)
        
        pred <- predict(mdl, val_x)
        pred[pred < 1e-9] <- 1e-9
        pred[pred >= 1 - 1e-9] <- 1 - 1e-9
        
        nll <- -mean(val_y * log(pred) + (1 - val_y) * log(1 - pred))
        
        results_survey <- results_survey %>%
          bind_rows(data.frame(
            valset = v,
            learningrate = lr,
            maxdepth = md,
            nrounds = n,
            nll = nll
          ))
        
      } # n
    } # md
  } # lr
} # v

View(results_survey)
results_survey <- results_survey %>%
  group_by(learningrate, maxdepth, nrounds) %>%
  mutate(avg_nll = mean(nll)) %>%
  ungroup() %>%
  arrange(avg_nll)

save(results_survey, file = "output/hyperparameter_tuning_results_survey.RData")

HPTuningSurvey <- ggplot(results_survey, aes(x = learningrate, y = avg_nll, color = as.factor(maxdepth))) + 
  geom_line() +
  facet_wrap(~ nrounds, scales = "free_y") +
  labs(
    x = "Learning Rate",
    y = "Average NLL",
    color = "maxdepth"
  )

ggsave("output/HPTuningSurvey.svg", plot = HPTuningSurvey, width = 6.65625, height = 3.520833, units = "in", dpi = 96)





###----------------- Hyperparameter tuning for sensor data
train_data_sen <- long_data %>% 
  filter(fold != 1, delta_svy == 1)
test_data_sen <- long_data %>% 
  filter(fold == 1)

train_data_sen <- train_data_sen %>% 
  mutate(valset = rep(1:V, length.out = n()))

obs <- test_data_sen$delta_sen

cat("", file = "output/sensor_log.txt")
t0 <- proc.time()[[3]]

# Initialize an empty list to store the results (data frame for nll values)
results_sensor <- data.frame(
  valset = integer(0),
  learningrate = numeric(0),
  maxdepth = integer(0),
  nrounds = integer(0),
  nll = numeric(0)
)

for (v in 1:V) {
  
  # prevalence in training set for class weights and benchmarking of model performance
  alpha_trn <- train_data_sen %>%
    filter(valset != v) %>%
    summarise(alpha_trn = sum(delta_sen) / n()) %>% 
    pull(alpha_trn)
  
  trn <- train_data_sen %>%
    filter(valset != v) %>%
    dplyr::select(all_of(features)) %>%
    as.matrix() %>%
    xgb.DMatrix(data = ., 
                label = train_data_sen %>% filter(valset != v) %>% pull(delta_sen))
  
  classweight <- train_data_sen %>%
    filter(valset != v) %>%
    mutate(classweight = ifelse(delta_sen == 1, 1 / (2 * alpha_trn), 1 / (2 * (1 - alpha_trn)))) %>%
    pull(classweight)
  
  val_x <- train_data_sen %>%
    filter(valset == v) %>%
    dplyr::select(all_of(features)) %>%
    as.matrix() %>%
    xgb.DMatrix(data = .)
  
  val_y <- train_data_sen %>%
    filter(valset == v) %>%
    pull(delta_sen)
  
  for (lr in learningrates) {
    for (md in maxdepths) {
      for (n in nrounds) {
        
        cat(paste(v, lr, md, n, (proc.time()[[3]] - t0) / 60, "\n"), 
            file = "output/sensor_log.txt", append = TRUE)
        
        # train model
        mdl <- xgboost(data = trn,
                       eta = lr,
                       max_depth = md,
                       nrounds = n,
                       objective = "binary:logistic",
                       weight = classweight,
                       early_stopping_rounds = 20)
        
        pred <- predict(mdl, val_x)
        pred[pred < 1e-9] <- 1e-9
        pred[pred >= 1 - 1e-9] <- 1 - 1e-9
        
        nll <- -mean(val_y * log(pred) + (1 - val_y) * log(1 - pred))
        
        results_sensor <- results_sensor %>%
          bind_rows(data.frame(
            valset = v,
            learningrate = lr,
            maxdepth = md,
            nrounds = n,
            nll = nll
          ))
        
      } # n
    } # md
  } # lr
} # v

View(results_sensor)
table(results_sensor$nll)

results_sensor <- results_sensor %>%
  group_by(learningrate, maxdepth, nrounds) %>%
  mutate(avg_nll = mean(nll)) %>%
  ungroup() %>%
  arrange(avg_nll)
table(results_sensor$avg_nll)
save(results_sensor, file = "output/hyperparameter_tuning_results_sensor.RData")

HPTuningSensor <- ggplot(results_sensor, aes(x = learningrate, y = avg_nll, color = as.factor(maxdepth))) + 
  geom_line() +
  facet_wrap(~ nrounds, scales = "free_y") +
  labs(
    x = "Learning Rate",
    y = "Average NLL",
    color = "maxdepth"
    )

ggsave("output/HPTuningSensor.svg", plot = HPTuningSensor, width = 6.65625, height = 3.520833, units = "in", dpi = 96)

