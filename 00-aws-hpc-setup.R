# Model configuration parameters for running ARTIS pipeline on AWS infrastructure

# sourced from 02-artis-pipeline

# R packages required ------------------------------------------------------
library(data.table)
library(magrittr)
library(Matrix)
library(parallel)
library(reticulate)
library(slam)
library(tidyverse)
library(doParallel)
library(aws.s3)
library(glue)
# loading in ARTIS R package
library(artis)

# configure environment ---------------------------------------------------
# Create and download ARTIS R package
setwd('/usr/src/ARTIS')

readRenviron(".Renviron")

# ONLY UNCOMMENT IF YOU ARE RUNNING DOCKER ON A NEW MAC CHIP
use_virtualenv("/usr/src/ARTIS/venv", required = TRUE)

# AWS variables -----------------------------------------------------------
artis_bucket <- "s3://artis-s3-bucket/"
artis_bucket_region <- "us-east-1"

# ARTIS model parameters ----------------------------------------------

# set specific years to run in number vector, leave empty to run all years
test_years <- c()

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



