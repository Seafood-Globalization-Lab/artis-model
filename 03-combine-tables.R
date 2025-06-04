#-------------------------------------------------------------------------------
# Setup based on the kind of analysis and machine you are working with

# Start with a clean working environment
rm(list=ls())
#-------------------------------------------------------------------------------
run_env <- "local"

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

# FIXIT fully integrate as dependency - right now only used in post-processing
library(duckdb)

# Gather ARTIS outputs -------------------------------------------------------
# This section gathers all ARTIS output data separated across multiple HS version
# and years into one file by output type (trade/consumption) and estimate type (max/midpoint/min)

final_outdir <- file.path(outdir, "outputs_combined")

# Ensure the 'outputs' directory exists on the aws "local" machine
if (!dir.exists("outputs")) { dir.create("outputs") }
# Ensure the 'artis_outputs' directory exists on the aws "local" machine
if (!dir.exists(final_outdir)) { dir.create(final_outdir) }

# find and combine all partitioned snet / artis files into a single .parquet file
combine_partitioned_data(
  search_dir = outdir_snet,
  outdir = final_outdir,
  data_type = "artis",
  estimate_data_type = "midpoint",
  artis_version = artis_version,
  file_type = "qs",
  date = start_date,
  search_pattern = "S-net_raw_midpoint",
  custom_timeseries = FALSE
)

# find and combine partitioned consumption files into single .parquet file
combine_partitioned_data(
  search_dir = outdir_snet,
  outdir = final_outdir,
  data_type = "consumption",
  estimate_data_type = "midpoint",
  artis_version = artis_version,
  file_type = "qs",
  date = start_date,
  search_pattern = "consumption_midpoint",
  custom_timeseries = FALSE
)

