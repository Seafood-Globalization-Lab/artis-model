# Pipeline step to setup required directories 

# Setup -----------------------------------------------------------

# make Posit package manager primary install repo (uses faster binaries)
# @ snapshot date - hold all package versions to the "latest" version at this date
# Think about updating this date, but a few months delayed from "today" to avoid release bugs
pak::repo_add(CRAN = "PPM@2026-06-21")

# install artis from the package root
pak::local_install(".")

# Run Local Machine Configuration (directory paths, parameters)
source(system.file("00-local-machine-setup.R", package = "artis"))

# define data collection functions
source(system.file("collect_raw_data_functions.R", package = "artis"))


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

