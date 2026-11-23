# Model configuration parameters for running ARTIS pipeline on a local machine

# Set configuration parameters -------------------------------------------

# Set up Start date for finding no solution countries
start_date <- Sys.Date()
artis_version <- "3.0"
# Set production data type variable ["SAU"] or ["FAO"] - 02-artis-pipeline
prod_data_type <- "FAO" # FIXIT change/combine fully with "running_sau"?
baci_version <- "202601"
fao_prod_version <- "2026.1.0"
# change this when ingesting new data with new years represented 
max_year <- 2024
min_year <- 1996 # not applied yet
hs_versions <- c("96", "02", "07", "12", "17", "22")
# maybe could assign names to hs_versions that equal the editions?
hs_editions <- c("1996", "2002", "2007", "2012", "2017", "2022")
hs_chapters <- c(3, 5, 15, 16, 23)
# Possible future parameter 
lockfile_update <- FALSE

# Regex to filter to ARTIS-relevant HS codes (chapter 03, 05/0511, 16/1604/1605, 23/2301)
artis_hs_regex <- "^03$|^03[0-9]{2}$|^16$|^160[45]$|^23$|^03[0-9]{4}$|^160[45][0-9]{2}$|^2301$|^230120$|^051191$|^05$|^0511$"

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
test_years <- c() 
# set model estimate - "min", "midpoint", "max" - default is "midpoint"
estimate_data_type <- "midpoint"
# hs_version_run is set in 02-artis-pipeline because of current `artis-hpc` setup

# Development mode --------------------------------------------------------
dev_mode <- FALSE
devdir <- "./AM_local/dev-artis-3.0"

# UW gephardtlab.fish server paths ---------------------------------------

# Set where files are stored - local or UW NAS server
# UW NAS server - gephartlab.fish.washington.edu local mapped path
# CLI: smbutil view //amarks1@gephartlab.fish.washington.edu
#path_env <- file.path("/Volumes/gephartlab") 
path_env <- file.path("~/Documents/UW-SAFS/ARTIS/data/ARTIS_local_development")


# Raw data paths ---------------------------------------------------------
path_data_storage <- file.path(path_env, "data-storage")
path_artis_dev <- file.path(path_env, "artis-development", glue::glue("ARTIS_{artis_version}_{prod_data_type}"))
# Raw data paths
path_baci_raw <- file.path(path_data_storage, "cepii-baci", glue::glue("baci_{baci_version}"))
path_fao_prod_raw <- file.path(path_data_storage, "fao-global-production")
path_sau_prod_raw <- file.path(path_data_storage, "sau-production")
path_fao_pop_raw <- file.path(path_data_storage, "fao-annual-population")
path_fb_slb_raw <- file.path(path_data_storage, "fishbase-sealifebase")

path_hs_codes_raw <- file.path(path_data_storage, "all-hs-codes")
path_EUMOFA <- file.path(path_data_storage, "EUMOFA")
path_cnv_fct <- file.path(path_data_storage, "seafood-conversion-factors")
path_wco_hs <- file.path(path_data_storage, "wco-hs-nomenclature")

# Main data directory paths --------------------------------------------------
# Directories for data inputs and outputs
# FIXIT - Change to dir_inputs (lots of downstream changes)
datadir <- file.path(path_artis_dev, glue::glue("model_inputs_{artis_version}_{prod_data_type}")) 
# FIXIT - Change to dir_output (lots of downstream changes)
outdir <- file.path(path_artis_dev, glue::glue("outputs_{artis_version}_{prod_data_type}")) 

# Output subdirectories -----------------------------------------------------

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



# CLI message to declare main model parameters -----------------------------
cli::cli_verbatim("
    ___    ____  ______ _______
   /   |  / __ \\/_  __//  _/ ___/
  / /| | / /_/ / / /   / / \\__ \\ 
 / ___ |/ _, _/ / /  _/ / ___/ / 
/_/  |_/_/ |_| /_/  /___//____/  
")
cli::cli_h1(" 🦐 🐟 🦪 ARTIS {.strong v{artis_version}} Configured 🐙 🦀 🐠")
cli::cli_li("Production data: {.field {prod_data_type}}")
if(is.null(test_years)){
  cli::cli_li("Years covered: {.field 1996 - {max_year}}")
} else{
  cli::cli_li("Years covered: {.field {test_years}}")
}
cli::cli_li("Estimate type: {.field {estimate_data_type}}")
cli::cli_li("Develop mode enabled: {.field {dev_mode}}")
#cli:: cli_li("Local data path: {.file {local_data_path}}")

# 02-artis-pipeline python environment -----------------------------------
# Linking python environment set up during installation for use in the pipeline
#python_path <- file.path(getwd(), "venv", "bin", "python3")
#use_python(python_path, required = TRUE)

# FIXIT 2026-11-18 AM ran out of room on my computer
# python_path <- file.path(getwd(), "venv")
# reticulate::use_virtualenv(python_path, required = TRUE)

# empty AWS values --------------------------------------------------------
# need explicit empty AWS values when running locally - do not change
artis_bucket <- ""
artis_bucket_region <- ""



