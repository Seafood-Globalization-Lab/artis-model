#-------------------------------------------------------------------------------
# Setup based on the kind of analysis and machine you are working with

# Start with a clean working environment
# rm(list=ls())
#-------------------------------------------------------------------------------
run_env <- "aws"

if (run_env == "aws") {
  # High Performance Computing on AWS Setup
  source("00-aws-hpc-setup.R")
} else if (run_env == "demo") {
  
  # Demo Setup
  # Uncomment the line below if you are running the ARTIS demo
  # Note: you do not need to run the local machine setup if you are running the demo
  source("00-demo-setup.R")
} else {
  # Local Machine Setup
  # Uncomment the line below if creating the ARTIS database and outputs on a
  # local machine
  source("00-local-machine-setup.R")
}

# Set up Start date for finding no solution countries
start_date <- Sys.Date()

# Path for sub folder within outputs that will contain all country-level solutions
# to mass balance equation, solved using the python solver "quadprog"
outdir_quadprog <- file.path(outdir, "quadprog_snet")

# Path for sub folder within outputs that will contain all country-level solutions
# to mass balance equation, solved using the python solver "cvxopt"
outdir_cvxopt <- file.path(outdir, "cvxopt_snet")

#-------------------------------------------------------------------------------
# This section gathers all ARTIS output data separated across multiple HS version
# and years into one file by output type (trade/consumption) and estimate type (max/midpoint/min)

final_outdir <- file.path(outdir, "artis_outputs")

# Ensure the 'outputs' directory exists on the aws "local" machine
if (!dir.exists("outputs")) { dir.create("outputs") }
# Ensure the 'artis_outputs' directory exists on the aws "local" machine
if (!dir.exists(final_outdir)) { dir.create(final_outdir) }

build_artis_data(outdir_snet, final_outdir,
                 run_env = run_env,
                 s3_bucket_name = artis_bucket,
                 s3_region = artis_bucket_region)

if (run_env == "aws") {
  # clean up worker before exiting
  unlink(datadir, recursive = TRUE)
  unlink(outdir, recursive = TRUE)
}
