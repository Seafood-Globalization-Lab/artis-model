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

build_df_years <- function(hs_versions, hs_editions, max_year) {
  names(hs_versions) <- hs_editions

  # build each hs version x analysis year data frame block
  purrr::imap(hs_versions, function(hs_version, edition_year) {
    # HS_year column with the single hs_version by rows equal to the number of years associated
    # analysis_year column with the vector of each associated year value
    data.frame(
      HS_year = rep(hs_version, length(as.numeric(edition_year):max_year)),
      analysis_year = as.numeric(edition_year):max_year
    )
  }) %>%
    # append all chunks together into single data frame
    dplyr::bind_rows()
}
