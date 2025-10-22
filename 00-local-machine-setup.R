# Model configuration parameters for running ARTIS pipeline on a local machine

# R packages required ------------------------------------------------------
library(glue)
library(reticulate)
library(cli)

# Set up Start date for finding no solution countries
start_date <- Sys.Date()
artis_version <- "2.1.1"
# Set production data type variable ["SAU"] or ["FAO"] - 02-artis-pipeline
prod_data_type <- "SAU" # FIXIT change/combine fully with "running_sau"?
local_data_path <- "/Users/theamarks/Documents/UW-SAFS/ARTIS/data"

# CLI message to declare main model parameters
cli::cli_h1("Configured 🐟 ARTIS {.strong version {artis_version}} 🐙 🦀 🦐 {.strong {prod_data_type}} production data 🦪")
cli::cli_h3("Local data path: {.file {local_data_path}}")

# Main data directory paths --------------------------------------------------
datadir_raw <- file.path(local_data_path, glue::glue("model_inputs_raw_{artis_version}"))
# Directory for inputs to create the ARTIS database
datadir <- file.path(local_data_path, glue::glue("model_inputs_{artis_version}_{prod_data_type}"))
outdir <- file.path(local_data_path, glue::glue("outputs_{artis_version}_{prod_data_type}"))

# FIXIT - declare raw FAO prod file name here instead of in 01-clean-model-inputs.R? Would this work if swithc
# to YAML config?

## Raw BACI ---------------------------------------------------------------
baci_version <- "202501"
tradedatadir <- file.path(datadir_raw, "baci_raw") 

# Output Directories -----------------------------------------------------

# Don't change this bit #############
# Path for sub folder within outputs that will contain all country-level solutions
# to mass balance equation, solved using the python solver "quadprog"
outdir_quadprog <- file.path(outdir, "quadprog_snet")
# Path for sub folder within outputs that will contain all country-level solutions
# to mass balance equation, solved using the python solver "cvxopt"
outdir_cvxopt <- file.path(outdir, "cvxopt_snet")
# Path for collecting ARTIS database files
outdir_snet <- file.path(outdir, "snet")

## Postprocessing output directory paths -----------------------------------
outdir_attribute <- file.path(outdir, "attribute_tables")
outdir_sql <- file.path(outdir, "sql_database")

# 01-clean-model-inputs parameters --------------------------------
# Model Mode for 01-clean-model-inputs - TRUE for SAU; FALSE for FAO
running_sau <- TRUE
## Set TRUE if new SeaLifeBase/FishBase data collection needed for 01-clean-model-inputs:
need_new_fb_slb <- FALSE

# AM - I think this is leftover code - can set HS year and year for running tests
test <- FALSE
test_year <- c()

# 02-artis-pipeline parameters ------------------------------------
# set years to run - empty if all years [c()], [c(2017)] or [c(2017,2020)] for subset of years
test_years <- c() 
# set model estimate - "min", "midpoint", "max" - default is "midpoint"
estimate_data_type <- "midpoint"

# hs_version_run is set in 02-artis-pipeline because of current `artis-hpc` setup

# Development mode --------------------------------------------------------
dev_mode <- TRUE

devdir <- "./AM_local/dev-artis-2.0"

if(dev_mode == TRUE){
  library(devtools)
}

# Create HS version / year assignments -----------------------------------

# Only change df_years when incorporating new HS version
# List of possible HS versions: HS96, HS02, HS07, HS12, HS17

# change this when ingesting new data with new years represented 
max_year <- 2023

# List of possible HS versions: HS96, HS02, HS07, HS12, HS17
# No need to do HS92 when using BACI though as that data starts in 1996
df_years <- data.frame(HS_year = c(rep("96", length(1996:max_year)),
                                   rep("02", length(2002:max_year)),
                                   rep("07", length(2007:max_year)),
                                   rep("12", length(2012:max_year)),
                                   rep("17", length(2017:max_year))),
                                   # add new HS version here
                       analysis_year = c(1996:max_year, 
                                          2002:max_year, 
                                          2007:max_year,
                                          2012:max_year, 
                                          2017:max_year
                                          # add new HS version here
                                        ))

## NOTE: If updating df_years here - you also need to update in ./R/initial_variable_setup.R which 
# creates and makes df_years available for running all functions in 02-artis-pipeline.R

# Create ARTIS directory architecture ------------------------------------

# Creating folder for clean model input data
if (!dir.exists(datadir)) {
  dir.create(datadir)
} else {
  cli::cli_alert_info("Directory {.file model_inputs_{artis_version}_{prod_data_type}/} already exists, contents may be overwritten.")
}

# Create main outputs folder
if (!dir.exists(outdir)) {
  dir.create(outdir)
} else {
  cli::cli_alert_info("Directory {.file model_outputs_{artis_version}/} already exists, contents may be overwritten.")
}

# output suboutput folders. Does not include HS version and year within these folders.
# Creating the sub folder for all country-level solutions generated by the python
# solver quadprog.
if (!dir.exists(outdir_quadprog)) {
  dir.create(outdir_quadprog)
} else {
  cli::cli_alert_info("Directory {.file model_outputs_{artis_version}/quadprog_snet/} already exists, contents may be overwritten.")
}

# Creating the sub folder for all country-level solutions generated by the python
# solver quadprog.
if (!dir.exists(outdir_cvxopt)) {
  dir.create(outdir_cvxopt)
} else {
  cli::cli_alert_info("Directory {.file model_outputs_{artis_version}/cvxopt_snet/} already exists, contents may be overwritten.")
}

# Creating the output folder for all ceate_snet and calculate_consumption outputs - trade and consumption outputs
if(!dir.exists(outdir_snet)) {
  dir.create(outdir_snet)
} else {
  cli::cli_alert_info("Directory {.file model_outputs_{artis_version}/snet/} already exists, contents may be overwritten.")
}

# create attribute directory if doesn't exist
if (!dir.exists(outdir_attribute)) { 
  dir.create(outdir_attribute) 
} else {
  cli::cli_alert_info("Directory {.file model_outputs_{artis_version}/attribute_tables/} already exists, contents may be overwritten.")
}

# create attribute directory if doesn't exist
if (!dir.exists(outdir_sql)) { 
  dir.create(outdir_sql) 
} else {
  cli::cli_alert_info("Directory {.file model_outputs_{artis_version}/sql_database/} already exists, contents may be overwritten.")
}

# 02-artis-pipeline python environment -----------------------------------
# Linking python environment set up during installation for use in the pipeline
#python_path <- file.path(getwd(), "venv", "bin", "python3")
#use_python(python_path, required = TRUE)

python_path <- file.path(getwd(), "venv")
reticulate::use_virtualenv(python_path, required = TRUE)

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


