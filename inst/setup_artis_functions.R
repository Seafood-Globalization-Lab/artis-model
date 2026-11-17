# Functions for ARTIS pipeline directory setup


# Check environment connection -------------------------------------------

## Verifies that the root data storage environment path is accessible
## (e.g. a locally-mapped NAS or remote server volume).
## Aborts with a connection instruction if the path is not found.

check_artis_env <- function(path_env) {

  if (!dir.exists(path_env)) {
    cli::cli_abort(
      c(
        "Environment path not accessible: {.file {path_env}}",
        "i" = "Map the server to your local machine before proceeding: \\
               {.url https://uwconnect.uw.edu/it?id=kb_article_view&sysparm_article=KB0034311}"
      )
    )
  }

  cli::cli_alert_success("Environment path accessible: {.file {path_env}}")
  invisible(path_env)
}


# Create ARTIS directory architecture ------------------------------------

## Creates each directory in `dirs` if it does not already exist.
## Reports which directories were created and which already existed.
## Uses recursive = TRUE so intermediate parent directories are created
## if they are also missing.

setup_artis_dirs <- function(dirs) {

  dirs_created  <- character(0)
  dirs_existing <- character(0)

  for (d in dirs) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE)
      dirs_created <- c(dirs_created, d)
    } else {
      dirs_existing <- c(dirs_existing, d)
    }
  }

  # Report created directories ----------------------------------------------

  if (length(dirs_created) > 0) {
    cli::cli_alert_success(
      "Created {length(dirs_created)} director{?y/ies}: {.file {basename(dirs_created)}}"
    )
  }

  # Report pre-existing directories -----------------------------------------

  if (length(dirs_existing) > 0) {
    cli::cli_alert_info(
      "{length(dirs_existing)} director{?y/ies} already exist — \\
      contents may be overwritten: {.file {basename(dirs_existing)}}"
    )
  }

  invisible(dirs)
}

# Create HS version / year assignments -----------------------------------

df_years <- data.frame(
  HS_year = ,
  analysis_year = 
  )

# List of possible HS versions: HS96, HS02, HS07, HS12, HS17
# No need to do HS92 when using BACI though as that data starts in 1996
df_years <- data.frame(HS_year = c(rep("96", length(1996:max_year)),
                                   rep("02", length(2002:max_year)),
                                   rep("07", length(2007:max_year)),
                                   rep("12", length(2012:max_year)),
                                   rep("17", length(2017:max_year))),
                                   # add new HS version here
                       analysis_year = c(1996:max_year, 
                                          2002:max_year, 
                                          2007:max_year,
                                          2012:max_year, 
                                          2017:max_year
                                          # add new HS version here
                                        ))

## NOTE: If updating df_years here - you also need to update in ./R/initial_variable_setup.R which 
# creates and makes df_years available for running all functions in 02-artis-pipeline.R