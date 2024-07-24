
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

# Getting the start date to identify files generated in this ARTIS run
start_date <- as.character(Sys.Date())

# Directory for inputs to create the ARTIS database
datadir <- "model_inputs"
# Directory where ARTIS database will be generated
outdir <- "outputs"

# HS version for trade records and timespan for ARTIS
# ie HS12 will generate all trade records from 2012 - 2020
hs_version_run <- "12"

# Linking python environment set up during installation for use in the pipeline
#python_path <- file.path(getwd(), "venv", "bin", "python3")
#use_python(python_path, required = TRUE)

python_path <- file.path(getwd(), "venv")
use_virtualenv(python_path, required = TRUE)

# Will determine if the ARTIS pipeline functions are run with the demo variables
# or to run functions to create the ARTIS database for all years and HS versions
demo_run <- FALSE

# ARTIS folder structure needs to be created differently when running on the
# High Performance Computing sytem
hpc_run <- FALSE

