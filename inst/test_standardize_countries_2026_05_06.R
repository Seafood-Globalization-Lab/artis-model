# Test this function Now

# Use raw data - compare results aginst ARTIS v1.2 standardized clean input data. 

library(tidyverse)
library(countrycode)
library(artis)
library(devtools)

# Connor's read in paths
# FAO data
fao_raw <- read_csv("QA/dev_standardized_countries/1.2_fao_raw_combos.csv")
sau_raw <- read_csv("QA/dev_standardized_countries/1.2_sau_raw_combos.csv")

# Althea's 
fao_raw <- read_csv("AM_local/dev-standardize-countries/1.2_fao_raw_combos.csv")
sau_raw <- read_csv("AM_local/dev-standardize-countries/1.2_sau_raw_combos.csv")
