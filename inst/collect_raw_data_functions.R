# Function for raw data collection


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

# Collect BACI Trade -----------------------------------------------------

## Webpage - https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37
## Download link structure - https://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS{hs_ver}_V{version}.zip
## Version is auto-detected by scraping the webpage (e.g. "202601")
## hs_versions: one or more of "92", "96", "02", "07", "12", "17", "22"

collect_baci_data <- function(outdir,
                              hs_versions = c("96", "02", "07", "12", "17", "22"),
                              overwrite = FALSE) {

  baci_page_url <- "https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37"
  baci_base_url <- "https://www.cepii.fr/DATA_DOWNLOAD/baci/data"
  # Guard: outdir must exist ------------------------------------------------

  if (!dir.exists(outdir)) {
    cli::cli_abort(
      "Output directory does not previously exist: {.file {outdir}}. Create it before calling {.fn collect_baci_data}."
    )
  }

  # Detect latest version from CEPII webpage --------------------------------

  cli::cli_alert_info("Fetching BACI download page to detect latest version: {.url {baci_page_url}}")

  page_text <- tryCatch(
    paste(readLines(baci_page_url, warn = FALSE), collapse = "\n"),
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to fetch BACI download page.",
          "URL: {.url {baci_page_url}}",
          "Error: {conditionMessage(e)}"
        )
      )
    }
  )

  version_matches <- regmatches(page_text, gregexpr("BACI_HS\\d+_V(\\d+)\\.zip", page_text))[[1]]
  if (length(version_matches) == 0) {
    cli::cli_abort(
      c(
        "Could not detect BACI version from download page — the page structure may have changed.",
        "i" = "Check manually: {.url {baci_page_url}}"
      )
    )
  }
  baci_version <- sub(".*_V(\\d+)\\.zip", "\\1", version_matches[[1]])

  cli::cli_alert_info("Detected latest BACI version: {.val {baci_version}}")

  # Pre-flight: report which files or unzipped folders already exist ---------

  expected_zips <- file.path(outdir, glue::glue("BACI_HS{hs_versions}_V{baci_version}.zip"))
  expected_dirs <- tools::file_path_sans_ext(expected_zips)
  already_exists <- expected_zips[file.exists(expected_zips) | dir.exists(expected_dirs)]

  if (length(already_exists) > 0) {
    if (!overwrite) {
      cli::cli_alert_info(
        "{length(already_exists)} of {length(hs_versions)} BACI version{?s} already exist (as zip or unzipped folder) and will be skipped: \\
        {.file {basename(tools::file_path_sans_ext(already_exists))}}"
      )
    } else {
      cli::cli_alert_warning(
        "{length(already_exists)} of {length(hs_versions)} existing BACI zip{?s} will be overwritten: \\
        {.file {basename(already_exists)}}"
      )
    }
  }

  # Loop: hs_versions -------------------------------------------------------

  downloaded <- character(0)

  for (hs_ver in hs_versions) {

    filename  <- glue::glue("BACI_HS{hs_ver}_V{baci_version}.zip")
    url       <- glue::glue("{baci_base_url}/{filename}")
    dest_file <- file.path(outdir, filename)

    # Guard: skip if zip or unzipped folder already exists
    dest_dir <- tools::file_path_sans_ext(dest_file)
    if (file.exists(dest_file) || dir.exists(dest_dir)) {
      if (!overwrite) {
        existing <- if (dir.exists(dest_dir)) dest_dir else dest_file
        cli::cli_alert_info(
          "BACI HS{hs_ver} already exists — skipping download. \\
          Set {.arg overwrite = TRUE} to re-download. {.file {existing}}"
        )
        next
      }
      cli::cli_alert_warning(
        "Overwriting existing BACI HS{hs_ver} zip: {.file {dest_file}}"
      )
    }

    # Download
    cli::cli_alert_info(
      "Downloading BACI HS{hs_ver} version {.val {baci_version}} from {.url {url}}"
    )

    tryCatch(
      utils::download.file(url, destfile = dest_file, mode = "wb", quiet = FALSE),
      error = function(e) {
        cli::cli_abort(
          c(
            "Download failed for BACI HS{hs_ver} version {.val {baci_version}}.",
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

    size_mb <- round(file.size(dest_file) / 1e6, 1)
    cli::cli_alert_success(
      "Downloaded BACI HS{hs_ver} version {.val {baci_version}} ({size_mb} MB) to {.file {dest_file}}"
    )

    downloaded <- c(downloaded, dest_file)
  }

  invisible(downloaded)
}

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

# Collect FishBase & Sealife Base ----------------------------------------

## already in the ARTIS namespace

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
