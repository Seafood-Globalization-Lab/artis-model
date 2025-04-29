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
library(countrycode)

# Getting the start date to identify files generated in this ARTIS run
start_date <- as.character(Sys.Date())

# Directory paths --------------------------------------------------
datadir_raw <- "~/Documents/UW-SAFS/ARTIS/data/model_inputs_raw"
# Directory for inputs to create the ARTIS database
datadir <- "AM_local/model_inputs"
# Directory where ARTIS database will be generated
outdir <- "AM_local/outputs"
# Path for collecting ARTIS database files
outdir_snet <- file.path(outdir, "snet")
outdir_attribute <- file.path(outdir, "attribute_tables")
outdir_sql <- file.path(outdir, "sql_database")
baci_version <- "202201"
tradedatadir <- paste("baci_raw/baci_", baci_version, sep = "")

# ARTIS model parameters 02-artis-pipeline ------------------------------------
# set years to run - empty if all years [c()]
test_years <- c(2006) 

# set model estimate - "min", "midpoint", "max" - default is "midpoint"
estimate_data_type <- "midpoint"

# Set production data type variable ["SAU"] or ["FAO"] - 02-artis-pipeline
prod_data_type <- "SAU"

# hs_version_run is set in 02-artis-pipeline because of current `artis-hpc` setup

# data cleaning options 01-clean-model-inputs --------------------------------
# Model Mode for 01-clean-model-inputs - TRUE for SAU; FALSE for FAO
running_sau <- TRUE

## Set TRUE if new SeaLifeBase/FishBase data collection needed for 01-clean-model-inputs:
need_new_fb_slb <- FALSE


# python environment - 02-artis-pipeline ----------------------------------
# Linking python environment set up during installation for use in the pipeline
#python_path <- file.path(getwd(), "venv", "bin", "python3")
#use_python(python_path, required = TRUE)

python_path <- file.path(getwd(), "venv")
use_virtualenv(python_path, required = TRUE)

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


