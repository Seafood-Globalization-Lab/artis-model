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

## WCO HS Nomenclature instrument and tools page:
##   https://www.wcoomd.org/en/topics/nomenclature/instrument-and-tools.aspx
## HS 2022 (current) edition page:
##   https://www.wcoomd.org/en/topics/nomenclature/instrument-and-tools/hs-nomenclature-2022-edition/hs-nomenclature-2022-edition.aspx
## Previous editions page:
##   https://www.wcoomd.org/en/topics/nomenclature/instrument-and-tools/hs_nomenclature_previous_editions.aspx
## NOTE: HS 1996 edition is not available on the WCO website.
##
## URL structure — chapter filenames encode the HS section number as a 2-digit prefix:
##   Section I  (Ch. 1–5)  → "01"  e.g. Chapter 3  → 0103
##   Section III (Ch. 15)  → "03"  e.g. Chapter 15 → 0315
##   Section IV (Ch. 16–24)→ "04"  e.g. Chapter 16 → 0416, Chapter 23 → 0423
##
## Edition-specific URL sub-paths and filename patterns:
##   2022: hs-nomenclature-2022/2022/{stem}_2022e.pdf
##   2017: hs-nomenclature-2017/2017/{stem}_2017e.pdf
##   2012: hs-nomenclature-2012/hs-2012/{stem}_2012e.pdf
##   2007: hs-nomenclature-older-edition/2007/hs-2007/{stem}_2007e.pdf
##   2002: hs-nomenclature-older-edition/2002/hs-2002/{stem}e.pdf  (no year suffix)

## hs_editions  # editions to collect — 1996 will be skipped (not available)
## hs_chapters  # HS chapter numbers to collect (e.g. c(3, 15, 16, 23))
## path_wco_hs  # data dir for downloaded PDFs

collect_wco_hs_chapters <- function(outdir, hs_editions, hs_chapters, overwrite = FALSE) {

  # Edition URL sub-path and filename suffix behavior -----------------------
  # year_suffix: TRUE  → filename is "{stem}_{edition}e.pdf"
  #              FALSE → filename is "{stem}e.pdf" (2002 drops the year)

  edition_specs <- list(
    "2022" = list(path = "hs-nomenclature-2022/2022",                  year_suffix = TRUE),
    "2017" = list(path = "hs-nomenclature-2017/2017",                  year_suffix = TRUE),
    "2012" = list(path = "hs-nomenclature-2012/hs-2012",               year_suffix = TRUE),
    "2007" = list(path = "hs-nomenclature-older-edition/2007/hs-2007", year_suffix = TRUE),
    "2002" = list(path = "hs-nomenclature-older-edition/2002/hs-2002", year_suffix = FALSE)
  )

  # HS section prefix per chapter number ------------------------------------
  # Covers only the chapters relevant to ARTIS (03, 15, 16, 23).
  # The prefix reflects the HS section the chapter belongs to.

  chapter_prefix <- c("3" = "01", "15" = "03", "16" = "04", "23" = "04")

  wco_base_url <- "https://www.wcoomd.org/-/media/wco/public/global/pdf/topics/nomenclature/instruments-and-tools"

  # Guard: outdir must exist ------------------------------------------------

  if (!dir.exists(outdir)) {
    cli::cli_abort(
      "Output directory does not exist: {.file {outdir}}. Create it before calling {.fn collect_wco_hs_chapters}."
    )
  }

  # Guard: check all requested chapters have a known prefix -----------------

  unknown_chapters <- hs_chapters[!as.character(hs_chapters) %in% names(chapter_prefix)]
  if (length(unknown_chapters) > 0) {
    cli::cli_abort(
      c(
        "Unknown HS chapter{?s}: {.val {unknown_chapters}}",
        "i" = "Supported chapters: {.val {as.integer(names(chapter_prefix))}}"
      )
    )
  }

  # Loop: edition (outer) → chapter (inner) ---------------------------------

  downloaded <- character(0)

  for (edition in hs_editions) {

    # Skip editions not available on the WCO website
    if (!edition %in% names(edition_specs)) {
      cli::cli_alert_warning(
        "HS {.val {edition}} is not available on the WCO website — skipping all chapters."
      )
      next
    }

    spec <- edition_specs[[edition]]

    for (chapter in hs_chapters) {

      prefix      <- chapter_prefix[[as.character(chapter)]]
      stem        <- glue::glue("{prefix}{sprintf('%02d', chapter)}")
      pdf_name    <- if (spec$year_suffix) glue::glue("{stem}_{edition}e.pdf") else glue::glue("{stem}e.pdf")
      url         <- glue::glue("{wco_base_url}/{spec$path}/{pdf_name}?la=en")
      filename    <- glue::glue("wco_hs_{edition}_chapter_{sprintf('%02d', chapter)}.pdf")
      dest_file   <- file.path(outdir, filename)

      # Guard: skip if file already exists
      if (file.exists(dest_file)) {
        if (!overwrite) {
          cli::cli_alert_info(
            "HS {.val {edition}} chapter {.val {chapter}} PDF already exists — skipping download. \\
            Set {.arg overwrite = TRUE} to re-download. {.file {dest_file}}"
          )
          next
        }
        cli::cli_alert_warning(
          "Overwriting existing HS {.val {edition}} chapter {.val {chapter}} PDF: {.file {dest_file}}"
        )
      }

      # Download
      cli::cli_alert_info(
        "Downloading WCO HS {.val {edition}} chapter {.val {chapter}} from {.url {url}}"
      )

      tryCatch(
        utils::download.file(url, destfile = dest_file, mode = "wb", quiet = FALSE),
        error = function(e) {
          cli::cli_abort(
            c(
              "Download failed for WCO HS {.val {edition}} chapter {.val {chapter}}.",
              "URL: {.url {url}}",
              "Error: {conditionMessage(e)}"
            )
          )
        }
      )

      # Verify download
      if (!file.exists(dest_file) || file.size(dest_file) == 0) {
        cli::cli_abort(
          "Download completed but output file is missing or empty: {.file {dest_file}}"
        )
      }

      size_kb <- round(file.size(dest_file) / 1e3, 1)
      cli::cli_alert_success(
        "Downloaded WCO HS {.val {edition}} chapter {.val {chapter}} ({size_kb} KB) to {.file {dest_file}}"
      )

      downloaded <- c(downloaded, dest_file)
    }
  }

  invisible(downloaded)
}

## Execute collection
collect_wco_hs_chapters(
  outdir      = path_wco_hs,
  hs_editions = hs_editions,
  hs_chapters = hs_chapters,
  overwrite   = FALSE
)
