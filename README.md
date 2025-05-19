# Capture-Recapture Simulation Study: Comparing Machine Learning and Statistical Models

## Introduction

This repository contains the code, data-generating mechanisms, analysis scripts, and outcome for the simulation study conducted as part of the master's thesis titled:
**"Comparing machine learning and statistical models in capture-recapture estimators: a simulation study on bias and precision"**  
Author: Luca Y. Kogelheide  
Supervisors: Dr. Joep Burger (CBS), Dr. Jonas Klingwort (CBS), Prof. Dr. Peter Lugtig (UU)   
Faculty: Methodology and Statistics for the Behavioural, Biomedical and Social Sciences – Utrecht University; in cooperation with Statistics Netherlands (CBS)  
Date: May 11, 2025  
Ethics Approval: FETC (File No. 24-2068)

## Repository Structure

.  
|--- code/ # Folder containing scripts on generating the population, simulation scenario, and anaysis.
    |--- 01_simulation/ # Scripts to simulate population and sample scenarios  
    |--- 02_Analysis/ # Hyperparameter tuning, analysis, and visualization  
|--- output/ # Output of the simulations (plots, desciptives)  
|--- README.md # This file  
|--- requirements.txt # R version and packages used  
|--- LICENSE # MIT License  
|--- simulation_project.Rproj # Rproject  

## Study Design

- **Objective**: This study compares a single source survey estimators with capture-recapture (CRC) estimators, the Lincoln-Petersen estimator with the model-based Huggins
estimators (both CRC estimators), and a logistic regression approach with a XGBoost-algorithm modeling approach within the Huggins estimator, with respect to bias and precision, under 
varying levels of measurement error.
- **Design**: Simulating a synthetic population of 10.000 units over 91 days. First capture occassion represent a survey of 2000 units over a period of one time. The second capture
occassion represents sensor data of each unit in the population (census). There are 48 combinations of survey underreporting (false-negatives), sensor underdetection (false-negatives), 
and sensor overdetection (false-positives), which are assessed across 100 iterations.
- **Evaluation Metrics**: Relative bias (RB); coefficient of variation (CV); relative root mean squared error (RRMSE).

## Data Information

- **Data generation is entirely synthetic**.
- Simulated population includes 10 covariates; only 3 affect capture probabilities.
- Survey and sensor capture events include simulated misreporting (false negatives/positives).
- No personal or identifiable information is included.
- Data are generated on-the-fly via scripts; no raw data files are stored.

## Reproducibility Instructions (Execution Order)

### 1. **Simulate the Population**
source("data/03_simulation_functions.R")  
source("data/01_generate_population.R")  

### 2. **Simulate the first simulation scenario**
First, I simulated the baseline scenario (i.e. no measurement error included). The first iteration will be used for hyperparameter optimization.  
source("code/01_Simulation/02_generate_sample_scenarios.R")  

### 3. **Hyperparameter optimization**
source("code/02_Analysis/02_analysis_functions.R")  
source("code/02_Analysis/01_hyperparameter_selection.R")   # XGBoost tuning  

### 4. **Analysis**
source("02_Analysis/03_analysis.R")  

### 5. **Finish simulation**
Repeat steps 2 and 4 for each of the 48 simulation scenarios  

### 6. **Visualize results**
source("02_Analysis/04_visualization.R")  

### 7. **Inspect output**
Results (plots and summary statistics) will be written to the output/ folder.  

## Ethics, Privacy, and Access

- **Ethical Approval**: Approved by the FETC (Utrecht University), protocol no. 24-2068.  
- **Privacy**: No personal or sensitive data is used; all data are synthetic.  
- **Access**: Repository is publicly available (please read/note MIT license).  

## Contact

For questions or clarification:  
**Luca Y. Kogelheide**  
**Email**: l.y.kogelheide@uu.nl  
**GitHub**: https://github.com/lykogelheide  
