
# Zorro Setup
# Setup and directories for Zorro (High Performance Computer used for full analysis)
packdir <- "/project/ARTIS/Package"
datadir <- "/project/ARTIS/ARTIS/model_inputs"
outdir <- "/project/ARTIS/ARTIS/snet"
setwd(packdir)

# If running on Zorro need to specify path to R packages
# These paths will change based on which HPC is in use
library(artis, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/4.2/")
library(data.table, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/4.2/")
library(magrittr, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/4.2/")
library(Matrix, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/4.2/")
library(parallel) # "parallel" package is now part of base and does not need to be installed, BUT still needs to be activated by running library(parallel)
library(reticulate, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/4.2/")
library(slam, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/4.2/")
library(tidyverse, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/4.2/")
library(doParallel, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/4.2/")
library(countrycode, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/4.2/")

hs_version_run <- "17"

# ARTIS folder structure needs to be created differently when running on the
# High Performance Computing system
hpc_run <- TRUE
demo_run <- FALSE
