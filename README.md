# artis-model

# System Requirements
- Platform: x86_64-apple-darwin17.0 (64-bit)
- Running under: macOS Ventura 13.3.1
- R version 4.2.2
- R packages:
  - readxl 1.4.1
  - janitor 2.1.0
  - countrycode 1.4.0
  - doParallel 1.0.17
  - iterators 1.0.14
  - foreach 1.5.2
  - slam 0.1-50
  - Matrix 1.5-1
  - magrittr 2.0.3
  - data.table 1.14.6
  - forcats 0.5.2
  - stringr 1.5.0
  - dplyr 1.0.10
  - purrr 1.0.1
  - readr 2.1.3
  - tidyr 1.2.1
  - tibble 3.1.8
  - ggplot2 3.4.0
  - tidyverse 1.3.2
  - reticulate 1.26
- Python version 3.10.9
- Python packages:
  - cvxopt     1.3.0
  - daqp       0.5.1
  - ecos       2.0.12
  - numpy      1.24.3
  - osqp       0.6.2.post9
  - pip        22.3.1
  - qdldl      0.1.7
  - qpsolvers  3.4.0
  - quadprog   0.1.11
  - scipy      1.10.1
  - scs        3.2.3
  - setuptools 65.6.3

# Installation Guide

This project uses Python 3.10.9 which can be downloaded [here](https://www.python.org/downloads/release/python-3109/) and RStudio which can be downloaded [here](https://posit.co/download/rstudio-desktop/).

It should take approximately 10 minutes to run this full installation.

## Creating python virtual environment
1. Open the artis-model repository in RStudio.
2. Click on the terminal tab.
3. Type "pwd" in the terminal.
4. Copy the result of the "pwd" terminal command.
5. Type "python3 -m venv [RESULT FROM pwd]/venv" (ie. "python3 -m venv /home/artis-model/venv")
6. Type "source venv/bin/activate" in terminal.
7. Type "pip3 install qpsolvers" in terminal.
6. Type "pip3 install quadprog" in terminal.
7. Type "pip3 install cvxopt" in terminal.
8. Confirm you have successfully installed the packages qpsolvers, quadprog, cvxopt by running "pip list".
9. Type "deactivate" in terminal.
10. Click on the Console tab.

Note that you only need to install the solvers the first time you run this code. Warnings about the latest version of pip may also appear during the installation - these are okay, but errors are not.

# R installation instructions
1. Click "Build" on the build tab on the top right hand side corner of RStudio.
2. Click on the dropdown arrow in the "Install" subtab within the "Build" window.
3. Click the option "Configure Build Tools..."
4. Make sure options mirror the image below and click OK.
![Build configuration options](./images/artis_r_build_config_options.png)
5. Click on the dropdown arrow in the "Install" subtab and select the option "Clean and Install"

# Running the model 

Running the demo for the ARTIS model should take approximately 10 minutes. To run the demo for ARTIS run the `02-artis-pipeline.R` script and then run the `04-build-artis-timeseries.R` script.

# Outputs

The outputs of the demo will appear in the "demo/outputs" directory. Within this folder "demo/outputs/custom_ts" will contain all the final files that if run on the full model inputs would be used to create the results of the ARTIS research paper.

Please find below descriptions of main files:
- "demo/outputs/custom_ts/mid_custom_ts.csv": This is the demo version of the main ARTIS trade records table.
- "demo/outputs/custom_ts/summary_consumption_midpoint.csv": This is the demo version of the main ARTIS seafood consumption records table.

# Methods and Workflow

## High level overview
The following diagrams describes how ARTIS trade records are obtained.

![Disaggregating Trade Records](./images/disaggregating_trade_records.png)
![Aggregating Trade Records back up](./images/building_trade_records_back_up.png)
![Consumption Workflow](./images/consumption_workflow.png)

## Code workflows
The following diagrams describe the how the codebase follows the workflow illustrated above.

![Cleaning data diagram](./images/model_inputs_creation.png)
![Mass balance solutions](./images/country_mass_balance_solution_creation.png)
![Creating ARTIS codeflow](./images/create_artis_codeflow.png)


