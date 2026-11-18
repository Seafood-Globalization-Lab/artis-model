# Pipeline step to setup required directories 

# Setup -----------------------------------------------------------

# Can source a yaml config file
# config <- config::get(file)

# make Posit package manager primary install repo (uses faster binaries)
# @ snapshot date - hold all package versions to the "latest" version at this date
# Think about updating this date, but a few months delayed from "today" to avoid release bugs
pak::repo_add(CRAN = "PPM@2026-06-21")

# FIXIT: Future feature to update based on config file parameter
if(lockfile_update){
  pak::lockfile_create(
    pkg = ".",
    lockfile = "artis_pkg.lock",
    dependencies = TRUE
  )
}

# Install all artis pkg dependencies exactly as pinned in the lockfile and building/install
pak::lockfile_install(lockfile = "artis_pkg.lock")

library(artis)
library(magrittr)

# Run Local Machine Configuration (directory paths, parameters)
source(system.file("00-local-machine-setup.R", package = "artis"))

# define data collection functions
source(system.file("setup_artis_functions.R", package = "artis"))


# Check connection -------------------------------------------------------

check_artis_env(path_env = path_env)

# Create directory architecture ------------------------------------------

setup_artis_dirs(c(
  datadir,
  outdir,
  outdir_quadprog,
  outdir_cvxopt,
  outdir_snet,
  outdir_attribute,
  outdir_sql
))


# Create hs version x year dataframe -------------------------------------

df_years <- build_df_years(
  hs_versions, 
  hs_editions, 
  max_year)
