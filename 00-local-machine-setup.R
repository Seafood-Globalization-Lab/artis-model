
# Set up for running ARTIS pipeline on a local machine
# R packages needed
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

# Set directory paths
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

# set years to run - 02-artis-pipeline 
# empty if all years [c()]
test_years <- c(2006) 

# Set production data type variable ["SAU"] or ["FAO"] - 02-artis-pipeline
prod_data_type <- "SAU"

# Model Mode for 01-clean-model-inputs - TRUE for SAU; FALSE for FAO
running_sau <- TRUE

## Set TRUE if new SeaLifeBase/FishBase data collection needed for 01-clean-model-inputs:
need_new_fb_slb <- FALSE

# Linking python environment set up during installation for use in the pipeline
#python_path <- file.path(getwd(), "venv", "bin", "python3")
#use_python(python_path, required = TRUE)

python_path <- file.path(getwd(), "venv")
use_virtualenv(python_path, required = TRUE)

# ARTIS folder structure needs to be created differently when running on the
# High Performance Computing sytem
hpc_run <- FALSE

# need explicity empty AWS values when running locally
artis_bucket <- ""
artis_bucket_region <- ""

# Demo Mode -----------------------------------------------------------------
# Not sure if working AM 2025-04-28
# Will determine if the ARTIS pipeline functions are run with the demo variables
# or to run functions to create the ARTIS database for all years and HS versions
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


