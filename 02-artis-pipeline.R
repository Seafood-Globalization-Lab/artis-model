
#-------------------------------------------------------------------------------
# Setup based on the kind of analysis and machine you are working with

# Start with a clean working environment
rm(list=ls())

# High Performance Computing (Zorro) Setup
# Uncomment the line below if creating the full ARTIS database and outputs on
# the Zorro High Performance computing system
# source("00-zorro-hpc-setup.R")

# Local Machine Setup
# Uncomment the line below if creating the ARTIS database and outputs on a
# local machine
# source("00-local-machine-setup.R")

# Demo Setup
# Uncomment the line below if you are running the ARTIS demo
# Note: you do not need to run the local machine setup if you are running the demo
source("00-demo-setup.R")

#-------------------------------------------------------------------------------
# This section generates the solutions for the mass balance problem for all
# countries across all years and HS versions

if (!hpc_run) {
  # Creating general outputs folder if necessary, will delete contents of folder
  # if it already exists
  if(dir.exists(outdir)) {
    warning("General output folder already exists, all contents are being deleted to create an empty folder.")
    unlink(outdir, recursive = TRUE)
  }
  dir.create(outdir)
}

# Path for sub folder within outputs that will contain all country-level solutions
# to mass balance equation, solved using the python solver "quadprog"
outdir_quadprog <- file.path(outdir, "quadprog_snet")

if (!hpc_run) {
  # Creating the sub folder for all country-level solutions generated by the python
  # solver quadprog. Will delete folder if it already exists
  if (dir.exists(outdir_quadprog)) {
    warning("quadprog output folder already exists, all contents are being deleted to create an empty folder.")
    unlink(outdir_quadprog, recursive = TRUE)
  }
  dir.create(outdir_quadprog)
}

# Formats and prepares all inputs for the country-level mass balance problems,
# and passes them into the python quadprog solver. All solutions are saved in the
# quadprog solver output folder.
if (!demo_run) {
  get_country_solutions(
    datadir,
    outdir_quadprog,
    hs_version = hs_version_run,
    prod_type = "FAO"
  )
} else {
  get_country_solutions(
    datadir,
    outdir_quadprog,
    hs_version = hs_version_run,
    prod_type = "FAO",
    test_year = test_years,
    num_cores = 3
  )
}


# Depending on the HS version and year, some mass balance problems were not
# solved by the quadprog solver. This function goes through all years for a
# specific HS version and finds all the countries where no solution was found by
# the quadprog solver by year and HS version
no_solve_countries <- get_no_solve_countries(
  snet_dir = outdir_quadprog,
  artis_run_date_no_dash = start_date
)

# Lists any combination country, year, HS version combination, where no solution
# to the mass balance problem was solved.
quadprog_no_solve_fp <- file.path(outdir_quadprog, "no_solve_countries.csv")
write.csv(no_solve_countries, quadprog_no_solve_fp, row.names = FALSE)

# Note: that if any country mass balance problems are not solved by the quadprog
# solver they will be passed onto the cvxopt solver to find its solution

# Path for sub folder within outputs that will contain all country-level solutions
# to mass balance equation, solved using the python solver "cvxopt"
outdir_cvxopt <- file.path(outdir, "cvxopt_snet")

if (!hpc_run) {
  # Creating the sub folder for all country-level solutions generated by the python
  # solver quadprog. Will delete folder if it already exists
  if (dir.exists(outdir_cvxopt)) {
    warning("cvxopt output folder already exists, all contents are being deleted to create an empty folder.")
    unlink(outdir_cvxopt, recursive = TRUE)
  }
  dir.create(outdir_cvxopt)
}

# cvxopt solver will only find solutions for the country mass balance problems,
# that were not solved by the quadprog solver
if (nrow(no_solve_countries) > 0) {
  
  # Formats and prepares all inputs for the country-level mass balance problems,
  # and passes them into the python cvxopt solver. All solutions are saved in the
  # cvxopt solver output folder.
  if (!demo_run) {
    get_country_solutions(
      datadir,
      outdir_cvxopt,
      hs_version = hs_version_run,
      solver_type = "cvxopt",
      no_solve_countries = no_solve_countries,
      prod_type = "FAO"
    )
  } else {
    get_country_solutions(
      datadir,
      outdir_cvxopt,
      hs_version = hs_version_run,
      solver_type = "cvxopt",
      no_solve_countries = no_solve_countries,
      prod_type = "FAO",
      test_year = test_years,
      num_cores = 3
    )
  }
}

#-------------------------------------------------------------------------------
# This section takes the solutions for each mass balance problem and runs the
# calculations to create the final ARTIS database

# Path for collecting ARTIS database files
outdir_snet <- file.path(outdir, "snet")

if(!hpc_run) {
  # Creating the output folder for all ARTIS database files, will delete folder
  # if it already exists
  if(dir.exists(outdir_snet)) {
    warning("ARTIS snet output folder already exists, all contents are being deleted to create an empty folder.")
    unlink(outdir_snet, recursive = TRUE)
  }
  dir.create(outdir_snet)
}

# Takes all solutions of country mass balance problems and calculates ARTIS database
# records, along with corresponding consumption records
if (!demo_run) {
  get_snet(
    outdir_quadprog,
    outdir_cvxopt,
    datadir,
    outdir_snet,
    num_cores = 10,
    hs_version = hs_version_run,
    prod_type = "FAO"
  )
} else {
  get_snet(
    outdir_quadprog,
    outdir_cvxopt,
    datadir,
    outdir_snet,
    num_cores = 3,
    hs_version = hs_version_run,
    prod_type = "FAO",
    test_years = test_years
  )
}