This folder contains all scripts necessary to simulate the population and sample scenarios for the simulation study. All data used in the analysis is generated synthetically using these scripts.

### Contents

- `00_simulation_functions.R´  
  Contains self-written functions used in the other two scripts in this folder.

- `01_generate_population.R´  
  Simulates the base population of 10.000 units and their covariates.

- `02_sample_scenario.R´  
  Creates 100 sampled capture occassions and introduces measurement error based on the included underreporting/underdetection/overdetection parameters. Parameters need to be included manually for each simulation scenario.

### Usage notes

These scripts are designed to be run in the above order, as each script depends on the objects created in the previous one. There is no need to run the `00_simulation_functions.R´, since the other scripts can call this script themselves.  
All variables are generated in memory; no raw datasets are saved.
