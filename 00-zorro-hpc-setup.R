
# Zorro Setup
# Setup and directories for Zorro (High Performance Computer used for full analysis)
packdir <- "/project/ARTIS/Package"
datadir <- "/project/ARTIS/ARTIS/model_inputs_20221129"
outdir <- "/project/ARTIS/ARTIS/snet_20221129"
setwd(packdir)

# If running on Zorro need to specify path to R packages
library(artis, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/3.6/")
library(data.table, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/3.6/")
library(magrittr, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/3.6/")
library(Matrix, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/3.6/")
library(parallel) # "parallel" package is now part of base and does not need to be installed, BUT still needs to be activated by running library(parallel)
library(reticulate, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/3.6/")
library(slam, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/3.6/")
library(tidyverse, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/3.6/")

hs_version_run <- "17"
