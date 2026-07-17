# Model configuration parameters for running ARTIS pipeline on a local machine

# R packages required ------------------------------------------------------
library(glue)
library(reticulate)
library(cli)

# Set up Start date for finding no solution countries
start_date <- Sys.Date()
artis_version <- "3.0"
# Set production data type variable ["SAU"] or ["FAO"] - 02-artis-pipeline
prod_data_type <- "FAO" # FIXIT change/combine fully with "running_sau"?
baci_version <- "202601"
fao_prod_version <- "2026.1.0"
fao_pop_version <- "2026_03_09" # last update date from website
# change this when ingesting new data with new years represented 
max_year <- 2024
max_comtrade_h <- 6
#local_data_path <- "/Users/theamarks/Documents/UW-SAFS/ARTIS/data"

# 01-clean-model-inputs parameters --------------------------------
# Model Mode for 01-clean-model-inputs - TRUE for SAU; FALSE for FAO
running_sau <- FALSE
## Set TRUE if new SeaLifeBase/FishBase data collection needed for 01-clean-model-inputs:
need_new_fb_slb <- FALSE
# AM - I think this is leftover code - can set HS year and year for running tests
test <- FALSE
test_year <- c()
# 02-artis-pipeline parameters ------------------------------------
# set years to run - empty if all years [c()], [c(2017)] or [c(2017,2020)] for subset of years
test_years <- c(1996) 
# set model estimate - "min", "midpoint", "max" - default is "midpoint"
estimate_data_type <- "midpoint"
# hs_version_run is set in 02-artis-pipeline because of current `artis-hpc` setup

# Development mode --------------------------------------------------------
dev_mode <- FALSE
devdir <- "./AM_local/dev-artis-3.0"

if(dev_mode == TRUE){
  library(devtools)
}

# UW gephardtlab.fish server paths ---------------------------------------

# FIXIT: Time to switch this over to a config.yml file to hold the config combos -
# Like local vs remote server files and run local vs AWS. 

# Set where files are stored - local or UW NAS server
# UW NAS server - gephardtlab.fish.washinton.edu local mapped path
#path_env <- file.path("/Volumes/gephartlab") 
path_env <- file.path("~/Documents/UW-SAFS/ARTIS/data/ARTIS_local_development")

path_data_storage <- file.path(path_env, "data-storage")
path_artis_dev <- file.path(path_env, "artis-development", glue::glue("ARTIS_{artis_version}_{prod_data_type}"))
# Raw data paths
path_baci_raw <- file.path(path_data_storage, "cepii-baci", glue::glue("baci_{baci_version}"))
path_fao_prod_raw <- file.path(path_data_storage, "fao-global-production")
path_sau_prod_raw <- file.path(path_data_storage, "sau-production")
path_fao_pop_raw <- file.path(path_data_storage, "fao-annual-population", glue::glue("Population_E_All_Data_{fao_pop_version}"))
path_fb_slb_raw <- file.path(path_data_storage, "fishbase-sealifebase")

path_hs_codes_raw <- file.path(path_data_storage, "all-hs-codes")
path_EUMOFA <- file.path(path_data_storage, "EUMOFA")
path_cnv_fct <- file.path(path_data_storage, "seafood-conversion-factors")

# Main data directory paths --------------------------------------------------
#local_data_path <- glue::glue("/Users/theamarks/Documents/UW-SAFS/ARTIS/data")
#datadir_raw <- file.path(local_data_path, glue::glue("model_inputs_raw_{artis_version}"))
# Directory for inputs to create the ARTIS database
datadir <- file.path(path_artis_dev, glue::glue("model_inputs_{artis_version}_{prod_data_type}")) # FIXIT - Change to dir_inputs (lots of downstream changes)
outdir <- file.path(path_artis_dev, glue::glue("outputs_{artis_version}_{prod_data_type}")) # FIXIT - Change to dir_output (lots of downstream changes)

# FIXIT - declare raw FAO prod file name here instead of in 01-clean-model-inputs.R? Would this work if swithc
# to YAML config?

## Raw BACI ---------------------------------------------------------------

#tradedatadir <- file.path(datadir_raw, "baci_raw") 

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
# FIXIT - phase this out to new KNB structure
outdir_attribute <- file.path(outdir, "attribute_tables")
outdir_sql <- file.path(outdir, "sql_database")

# Create HS version / year assignments -----------------------------------

# Only change df_years when incorporating new HS version
# List of possible HS versions: HS96, HS02, HS07, HS12, HS17

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

# CLI message to declare main model parameters -----------------------------
cli::cli_verbatim("
    ___    ____  ______ _______
   /   |  / __ \\/_  __//  _/ ___/
  / /| | / /_/ / / /   / / \\__ \\ 
 / ___ |/ _, _/ / /  _/ / ___/ / 
/_/  |_/_/ |_| /_/  /___//____/  
")
cli::cli_h1(" 🦐 🐟 🦪 Configured ARTIS {.strong v{artis_version}} 🐙 🦀 🐠")
cli:: cli_li("Production data: {.field {prod_data_type}}")
cli:: cli_li("Years covered: {.field {test_years}}")
cli:: cli_li("Estimate type: {.field {estimate_data_type}}")
#cli:: cli_li("Local data path: {.file {local_data_path}}")


# Create & check ARTIS directory architecture ------------------------------------

# UW remote server connection
if(!dir.exists(path_env)) {
  cli::cli_abort("Not connected to UW server {.file {path_env}}. Map server to local machine before proceeding - following these instructions {.url https://uwconnect.uw.edu/it?id=kb_article_view&sysparm_article=KB0034311}")
}

dirs_to_create <- c(
  datadir,
  outdir,
  outdir_quadprog,
  outdir_cvxopt,
  outdir_snet,
  outdir_attribute,
  outdir_sql
)

dirs_existing <- c()

for (d in dirs_to_create) {
  if (!dir.exists(d)) {
    dir.create(d)
  } else {
    dirs_existing <- c(dirs_existing, d)
  }
}

if (length(dirs_existing) > 0) {
  cli::cli_alert_info(
    "Directories already exist - Contents may be overwritten in {.file {path_artis_dev}}: \n{.file {basename(dirs_existing)}}"
  )
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


