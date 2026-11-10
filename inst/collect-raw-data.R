# Pipeline step to collect raw data ingested by the ARTIS model

# Setup -----------------------------------------------------------

# make Posit package manager primary install repo (uses faster binaries)
# @ snapshot date - hold all package versions to the "latest" version at this date
# Think about updating this date, but a few months delayed from "today" to avoid release bugs
pak::repo_add(CRAN = "PPM@2026-06-21")

# install artis from the package root
pak::local_install(".")

# Run Local Machine Configuration (directory paths, parameters)
source(system.file("00-local-machine-setup.R", package = "artis"))


# Collect FAO Production -------------------------------------------------

## Webpage - https://www.fao.org/fishery/collection/global_production/en?lang=en
## Download link - https://www.fao.org/fishery/static/Data/GlobalProduction_2026.1.0.zip

collect_fao_prod_data <- function(outdir, version, overwrite = FALSE) {

  # Construct paths and URL -------------------------------------------------

  filename <- glue::glue("GlobalProduction_{version}.zip")
  dest_file <- file.path(outdir, filename)
  url <- glue::glue("https://www.fao.org/fishery/static/Data/{filename}")

  # Guard: outdir must exist ------------------------------------------------

  if (!dir.exists(outdir)) {
    cli::cli_abort(
      "Output directory does not exist: {.file {outdir}}. Create it before calling {.fn collect_fao_prod_data}."
    )
  }

  # Guard: skip if file already exists --------------------------------------

  if (file.exists(dest_file)) {
    if (!overwrite) {
      cli::cli_alert_info(
        "FAO production zip already exists — skipping download. \\
        Set {.arg overwrite = TRUE} to re-download. {.file {dest_file}}"
      )
      return(invisible(dest_file))
    }
    cli::cli_alert_warning(
      "Overwriting existing FAO production zip: {.file {dest_file}}"
    )
  }

  # Download ----------------------------------------------------------------

  cli::cli_alert_info("Downloading FAO Global Production {.val {version}} from {.url {url}}")

  tryCatch(
    utils::download.file(url, destfile = dest_file, mode = "wb", quiet = FALSE),
    error = function(e) {
      cli::cli_abort(
        c(
          "Download failed for FAO Global Production {.val {version}}.",
          "URL: {.url {url}}",
          "Error: {conditionMessage(e)}"
        )
      )
    }
  )

  # Verify download ---------------------------------------------------------

  if (!file.exists(dest_file) || file.size(dest_file) == 0) {
    cli::cli_abort(
      "Download completed but output file is missing or empty: {.file {dest_file}}"
    )
  }

  size_mb <- round(file.size(dest_file) / 1e6, 1)
  cli::cli_alert_success(
    "Downloaded FAO Global Production {.val {version}} ({size_mb} MB) to {.file {dest_file}}"
  )

  invisible(dest_file)
}

## Execute collection
path_fao_prod_zip <- collect_fao_prod_data(
  outdir = path_fao_prod_raw,
  version = fao_prod_version, 
  overwrite = FALSE
)

# Collect BACI Trade -----------------------------------------------------

## Webpage - https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37


# Collect FAO Population -------------------------------------------------

## Webpage - https://www.fao.org/faostat/en/#data/OA
## Download link "all data" - https://bulks-faostat.fao.org/production/Population_E_All_Data.zip
## Download link "all data normalized" - https://bulks-faostat.fao.org/production/Population_E_All_Data_(Normalized).zip

collect_fao_pop_data <- function(outdir,
                                 download_date = format(Sys.Date(), "%Y_%m_%d"),
                                 overwrite = FALSE) {

  # Construct paths and URL -------------------------------------------------
  # The FAOSTAT download URL is static (no version in URL). Versioning is
  # encoded directly in the filename using the download date.

  filename  <- glue::glue("Population_E_All_Data_{download_date}.zip")
  dest_file <- file.path(outdir, filename)
  url       <- "https://bulks-faostat.fao.org/production/Population_E_All_Data.zip"

  # Guard: outdir must exist ------------------------------------------------

  if (!dir.exists(outdir)) {
    cli::cli_abort(
      "Output directory does not exist: {.file {outdir}}. Create it before calling {.fn collect_fao_pop_data}."
    )
  }

  # Guard: skip if file already exists --------------------------------------

  if (file.exists(dest_file)) {
    if (!overwrite) {
      cli::cli_alert_info(
        "FAO population zip already exists for {.val {download_date}} — skipping download. \\
        Set {.arg overwrite = TRUE} to re-download. {.file {dest_file}}"
      )
      return(invisible(dest_file))
    }
    cli::cli_alert_warning(
      "Overwriting existing FAO population zip: {.file {dest_file}}"
    )
  }

  # Download ----------------------------------------------------------------

  cli::cli_alert_info("Downloading FAO Annual Population ({.val {download_date}}) from {.url {url}}")

  tryCatch(
    utils::download.file(url, destfile = dest_file, mode = "wb", quiet = FALSE),
    error = function(e) {
      cli::cli_abort(
        c(
          "Download failed for FAO Annual Population.",
          "URL: {.url {url}}",
          "Error: {conditionMessage(e)}"
        )
      )
    }
  )

  # Verify download ---------------------------------------------------------

  if (!file.exists(dest_file) || file.size(dest_file) == 0) {
    cli::cli_abort(
      "Download completed but output file is missing or empty: {.file {dest_file}}"
    )
  }

  size_mb <- round(file.size(dest_file) / 1e6, 1)
  cli::cli_alert_success(
    "Downloaded FAO Annual Population ({.val {download_date}}) ({size_mb} MB) to {.file {dest_file}}"
  )

  invisible(dest_file)
}

path_fao_pop_zip <- collect_fao_pop_data(
  outdir    = path_fao_pop_raw,
  overwrite = FALSE
)

# Collect FishBase & Sealife Base ----------------------------------------

artis::collect_fb_slb_data(parent_outdir = path_fb_slb_raw)

# Collect EUMOFA processing ----------------------------------------------

## website - https://fishery-aquaculture-market-observatory.ec.europa.eu/en/data/bulk-download
## Download link "yearly_processing" - 

# Collect WCO HS Nomenclature --------------------------------------------

