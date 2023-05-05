
#-------------------------------------------------------------------------------
# Zorro Setup
# Setup and directories for Zorro
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

#-------------------------------------------------------------------------------
# Testing Setup

# library(artis)
# library(data.table)
# library(magrittr)
# library(Matrix)
# library(parallel) # "parallel" package is now part of base and does not need to be installed, BUT still needs to be activated by running library(parallel)
# library(reticulate)
# library(slam)
# library(tidyverse)
# 
# datadir <- "qa/model_inputs_20221129"
# outdir <- "qa/snet_20221129"
#-------------------------------------------------------------------------------
# Geting country solutions

# quadprog country solutions
outdir_quadprog <- file.path(outdir, "quadprog_snet")

# Creating out folder if necessary
if (!dir.exists(outdir_quadprog)) {
  dir.create(outdir_quadprog)
} else {
  warning("OUTDIR already exists!")
}

get_country_solutions(
  datadir,
  outdir_quadprog,
  hs_version = hs_version_run,
  prod_type = "FAO"
)

# getting all missing countries
no_solve_countries <- get_no_solve_countries(
  snet_dir = outdir_quadprog,
  artis_run_date_no_dash = "2022-11-18"
)

quadprog_no_solve_fp <- file.path(outdir_quadprog, "no_solve_countries.csv")

print(file.path(snet_dir, "no_solve_countries.csv"))
write.csv(no_solve_countries, quadprog_no_solve_fp, row.names = FALSE)


# # quadprog country solutions
outdir_cvxopt <- file.path(outdir, "cvxopt_snet")

# Creating out folder if necessary
if (!dir.exists(outdir_cvxopt)) {
  dir.create(outdir_cvxopt)
} else {
  warning("OUTDIR already exists!")
}

# NO need to run solve countries with cvxopt
if (nrow(no_solve_countries) > 0) {
  get_country_solutions(
    datadir,
    outdir_cvxopt,
    hs_version = hs_version_run,
    solver_type = "cvxopt",
    no_solve_countries = no_solve_countries,
    prod_type = "FAO"
  )
}


#-------------------------------------------------------------------------------
# Creating snet

outdir_snet <- file.path(outdir, "snet")

# Creating out folder if necessary
if (!dir.exists(outdir_snet)) {
  dir.create(outdir_snet)
} else {
  warning("OUTDIR already exists!")
}

get_snet(
  outdir_quadprog,
  outdir_cvxopt,
  datadir,
  outdir_snet,
  hs_version = hs_version_run,
  prod_type = "FAO"
)


