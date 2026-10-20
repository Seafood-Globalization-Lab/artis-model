
# Local machine setup
source("00-local-machine-setup.R")

# Demo setup
# Model inputs and outputs directories
datadir <- "demo/model_inputs"
outdir <- "outputs"

# Demo run will only generate an ARTIS database for 2018 using HS12 trade records
test_years <- c(2018)
hs_version_run <- "12"

demo_run <- TRUE
