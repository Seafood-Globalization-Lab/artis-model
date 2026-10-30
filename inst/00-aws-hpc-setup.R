# Model configuration parameters for running ARTIS pipeline on AWS infrastructure

# sourced from 02-artis-pipeline.R

# R packages required ------------------------------------------------------

# make Posit package manager primary install repo (uses faster binaries)
# @ snapshot date - hold all package versions to the "latest" version at this date
# Think about updating this date, but a few months delayed from "today" to avoid release bugs
pak::repo_add(CRAN = "RSPM@2026-06-21")

# install packages
pak::pak(c(
  "glue",
  "reticulate",
  "cli",
  "aws.s3",
))

library(reticulate)
library(aws.s3)
library(glue) 
library(cli)

# configure environment ---------------------------------------------------
# Create and download ARTIS R package
setwd('/usr/src/ARTIS')

readRenviron(".Renviron")

# ONLY UNCOMMENT IF YOU ARE RUNNING DOCKER ON A NEW MAC CHIP
reticulate::use_virtualenv("/usr/src/ARTIS/venv", required = TRUE)

# AWS variables -----------------------------------------------------------
artis_bucket <- "s3://artis-s3-bucket/"
artis_bucket_region <- "us-east-1"

# ARTIS model parameters ----------------------------------------------

# set specific years to run in number vector, leave empty to run all years
test_years <- c(1996:2019)

# set model estimate - "min", "midpoint", "max" - default is "midpoint"
estimate_data_type <- "midpoint"
# Set production data type variable ["SAU"] or ["FAO"] - 02-artis-pipeline
prod_data_type <- "SAU"

# Set up Start date for finding no solution countries
start_date <- Sys.Date()

dev_mode <- FALSE

# directories -------------------------------------------------------------
datadir <- "model_inputs"
outdir <- "outputs"
# Path for sub folder within outputs that will contain all country-level solutions
# to mass balance equation, solved using the python solver "quadprog"
outdir_quadprog <- file.path(outdir, "quadprog_snet")
# Path for sub folder within outputs that will contain all country-level solutions
# to mass balance equation, solved using the python solver "cvxopt"
outdir_cvxopt <- file.path(outdir, "cvxopt_snet")
# Path for collecting ARTIS database files
outdir_snet <- file.path(outdir, "snet")



