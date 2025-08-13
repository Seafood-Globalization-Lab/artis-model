# Model configuration parameters for running ARTIS pipeline on a local machine

# Sourced from scripts 01-clean-model-inputs and 02-artis-pipeline

# R packages required ------------------------------------------------------
library(artis)
library(data.table)
library(magrittr)
library(Matrix)
library(parallel)
library(reticulate)
library(slam)
library(tidyverse)
library(doParallel)
library(aws.s3)
library(arrow)
library(countrycode)
library(glue)

# Set up Start date for finding no solution countries
start_date <- Sys.Date()
artis_version <- "v1.1.0"

# Input data directory paths --------------------------------------------------
datadir_raw <- "/Users/theamarks/Documents/UW-SAFS/ARTIS/data/model_inputs_raw_2.0"
# Directory for inputs to create the ARTIS database
datadir <- "~/Documents/UW-SAFS/ARTIS/data/model_inputs_2.0_FAO"
baci_version <- "202501"
tradedatadir <- paste("baci_raw/baci_", baci_version, sep = "")

# Model output directory paths --------------------------------------------
# Directory where ARTIS database will be generated
outdir <- "~/Documents/UW-SAFS/ARTIS/data/outputs_2.0_FAO"
# Path for sub folder within outputs that will contain all country-level solutions
# to mass balance equation, solved using the python solver "quadprog"
outdir_quadprog <- file.path(outdir, "quadprog_snet")
# Path for sub folder within outputs that will contain all country-level solutions
# to mass balance equation, solved using the python solver "cvxopt"
outdir_cvxopt <- file.path(outdir, "cvxopt_snet")
# Path for collecting ARTIS database files
outdir_snet <- file.path(outdir, "snet")


# Postprocessing output directory paths -----------------------------------
outdir_attribute <- file.path(outdir, "attribute_tables")
outdir_sql <- file.path(outdir, "sql_database")

# 01-clean-model-inputs parameters --------------------------------
# Model Mode for 01-clean-model-inputs - TRUE fo#r SAU; FALSE for FAO
running_sau <- FALSE
## Set TRUE if new SeaLifeBase/FishBase data collection needed for 01-clean-model-inputs:
need_new_fb_slb <- FALSE
# List of possible HS versions: HS96, HS02, HS12, HS17
HS_year <- c("96", "02", "07", "12", "17")
#HS_year <- c("12")
# AM - I think this is leftover code - can set HS year and year for running tests
test <- FALSE
test_year <- c()

# 02-artis-pipeline parameters ------------------------------------
# set years to run - empty if all years [c()]
test_years <- c() 
# set model estimate - "min", "midpoint", "max" - default is "midpoint"
estimate_data_type <- "midpoint"
# Set production data type variable ["SAU"] or ["FAO"] - 02-artis-pipeline
prod_data_type <- "FAO"
dev_mode <- FALSE

# hs_version_run is set in 02-artis-pipeline because of current `artis-hpc` setup

# 02-artis-pipeline python environment -----------------------------------
# Linking python environment set up during installation for use in the pipeline
#python_path <- file.path(getwd(), "venv", "bin", "python3")
#use_python(python_path, required = TRUE)

python_path <- file.path(getwd(), "venv")
use_virtualenv(python_path, required = TRUE)


# 02-artis-pipeline Number of cores ---------------------------------------

# not working yet
# if (run_env == "local") {
#   detected_num_cores <- parallel::detectCores() - 1
# }

# empty AWS values --------------------------------------------------------
# need explicit empty AWS values when running locally - do not change
artis_bucket <- ""
artis_bucket_region <- ""

# Demo Mode -----------------------------------------------------------------
# Not sure if working AM 2025-04-28
# Will determine if the ARTIS pipeline functions are run with the demo variables
# or to run functions to create the ARTIS database for all years and HS versions
# TRUE or FALSE explicitly required for several conditionals within model functions
demo_run <- FALSE

# If running a test environment with specific codes scinames this variable should be true else false
# test <- FALSE
# test_year <- 2018
# test_hs <- "12"
# 
# if(test == TRUE){
#   test_scinames <- read.csv("demo/sciname_shrimps_prawns.csv") %>%
#     select(sciname) %>%
#     distinct() %>%
#     pull(sciname)
#   
#   test_codes <- c("030617", "160529", "160521", "030627", "030616", "030626")
# }else{}


