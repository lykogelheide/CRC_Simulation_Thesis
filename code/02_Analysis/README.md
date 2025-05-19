This folder contains scripts for running the statistical and machine learning estimators on the simulated data and visualizing the results.

### Contents

- `00_analysis_functions.R´  
  Contains self-written functions used for the following three scripts in this folder.
  
- `01_hyperparameter_tuning.R´  
  Performs hyperparmeter tuning for the XGBoost-based Huggins estimator using 10-fold cross-validation on the non-test set of the first iteration of the baseline scenario (i.e. no errors included).
  
- `02_analysis.R´  
  Generates the estimates and computes the thre evaluation metrices for each scenario. Used on each simulation scenario seperately. When run for the first time, the '#' in line X has to be removed, to create the empty data frame the results are stored in.
  
- `03_visualization.R´  
  Generates all evaluation plots (bias, variance, accuracy) and writes them to the "output" folder. To be used on the list object created by "02_analysis.R" after the last simulation scenario (i.e. when all outcome data are included in this data frame)

### Execution Order

1. Run after `code/01_Simulation/´ scripts have finished for the first simulation scenario.  
2. Execute the `01_hyperparameter_tuning.R´ script.  
3. Use the `02_analysis.R´ to run estimators for a single simulation scenario.  
4. Repeat steps 1 and 3 for each of the 48 simulation scenarios once.
5. Finish with `03_visualization.R´ after having analyzed each simulation scenario to produce the plots.
