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
# loading in ARTIS R package
library(artis)

# configure environment ---------------------------------------------------
# Create and download ARTIS R package
setwd('/usr/src/ARTIS')

readRenviron(".Renviron")

# ONLY UNCOMMENT IF YOU ARE RUNNING DOCKER ON A NEW MAC CHIP
use_virtualenv("/usr/src/ARTIS/venv", required = TRUE)

# AWS variables -----------------------------------------------------------
# Download files to create ARTIS R package from AWS S3
artis_bucket <- "s3://artis-s3-bucket/"
artis_bucket_region <- "us-east-1"

# ARTIS model parameters ----------------------------------------------
# HS version to run in this pipeline script. 
# artis-hpc/create-pipeline-versions.sh script automatically changes this line 
# based on $HS_VERSIONS values set to submit multiple jobs to AWS Batch - 
# Do not manually change for AWS.
hs_version_run <- "07"

# set specific years to run in number vector, leave empty to run all years
test_years <- c()

# set model estimate - "min", "midpoint", "max" - default is "midpoint"
estimate_data_type <- "midpoint"

# directories -------------------------------------------------------------
datadir <- "model_inputs"
outdir <- "outputs"
outdir_snet <- file.path(outdir, "snet")




