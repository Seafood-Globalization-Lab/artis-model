
# libraries
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

# Create and download ARTIS R package
setwd('/usr/src/ARTIS')

readRenviron(".Renviron")

# ONLY UNCOMMENT IF YOU ARE RUNNING DOCKER ON A NEW MAC CHIP
use_virtualenv("/usr/src/ARTIS/venv", required = TRUE)

# Download files to create ARTIS R package from AWS S3
artis_bucket <- "s3://artis-s3-bucket/"
artis_bucket_region <- "us-east-1"

datadir <- "model_inputs"
outdir <- "outputs"

