# extract_fao_hs6_descriptions() — Reference Implementation
# artis package | R/extract_fao_hs6_descriptions.R
#
# Source: developed and validated against FAO & WCO HS Codes for Fisheries and
# Aquaculture Products - Harmonized System Nomenclature 2022 (DOI: 10.4060/cc6347en)
# PDF: https://www.fao.org/3/cc6347en/cc6347en.pdf (310 pages, HS2022 edition)
#
# Compatible with multiple HS editions (tested: HS2017, HS2022). Unique HS6
# code and row counts vary by edition.


# Internal helpers -------------------------------------------------------

## Detect the character position where the right column begins on a two-column page.
## Finds lines that contain two HS code patterns and returns the median
## start position of the second code. Returns NA if no two-code lines are found
## (e.g. single-column page or section header page).
.detect_col_split_fao <- function(page_text) {
  lines <- stringr::str_split(page_text, "\n")[[1]]
  positions <- integer(0)
  for (line in lines) {
    m <- gregexpr("\\d{4}\\.\\d{2}\\s{2,}", line)[[1]]
    if (length(m) == 2 && m[1] != -1) {
      positions <- c(positions, m[2])
    }
  }
  if (length(positions) > 0) as.integer(median(positions)) else NA_integer_
}

## Determine whether a continuation line begins a new description under the
## same HS code, rather than continuing the previous description.
##
## A new description is detected when:
##   1. The line starts with an uppercase letter (not "(" or lowercase)
##   2. The accumulated previous description does NOT end with a grammatical
##      connector (comma, open paren, or connecting words)
##
## This heuristic handles cases like 0308.29, 0308.30, and 0308.90 where two
## distinct product descriptions appear consecutively under one HS code.
.is_new_desc_fao <- function(line, prev_desc) {
  if (is.null(prev_desc) || nchar(stringr::str_trim(line)) < 5) return(FALSE)
  starts_upper <- stringr::str_detect(line, "^[A-Z]")
  ends_connector <- stringr::str_detect(
    prev_desc,
    paste0(
      ",\\s*$|\\(\\s*$",
      "|\\band\\s*$|\\bor\\s*$|\\bof\\s*$|\\bfor\\s*$|\\bto\\s*$",
      "|\\bfrom\\s*$|\\bexcluding\\s*$|\\bincluding\\s*$",
      "|\\bwhether\\s*$|\\bnot\\s*$|\\bthe\\s*$"
    )
  )
  starts_upper && !ends_connector
}

## Parse one column's text stream (a character vector of trimmed lines) into
## a data frame of HS6 entries. Each HS code line starts a new entry; wrapped
## continuation lines are appended. When .is_new_desc_fao() detects the start
## of a second description under the same code, the current description is saved
## and a new row is opened under the same hs6.
.parse_stream_fao <- function(stream, hs_version) {
  hs_pat <- "^(\\d{4}\\.\\d{2})\\s{2,}(.+)$"
  entries <- list()
  cur_code <- NULL
  cur_desc <- NULL

  save_entry <- function(code, desc) {
    entries[[length(entries) + 1]] <<- data.frame(
      hs_version       = hs_version,
      hs6              = gsub("\\.", "", code),
      description_orig = stringr::str_squish(desc),
      stringsAsFactors = FALSE
    )
  }

  for (line in stream[nchar(stream) > 0]) {
    trimmed <- stringr::str_trim(line)
    if (nchar(trimmed) == 0) next

    if (stringr::str_detect(trimmed, hs_pat)) {
      # New HS code entry: save previous and start fresh
      if (!is.null(cur_code)) save_entry(cur_code, cur_desc)
      m <- stringr::str_match(trimmed, hs_pat)
      cur_code <- m[1, 2]
      cur_desc <- stringr::str_trim(m[1, 3])

    } else if (!is.null(cur_code)) {
      if (.is_new_desc_fao(trimmed, cur_desc)) {
        # Multiple descriptions under the same HS code:
        # save current description and begin a new row for the same code
        save_entry(cur_code, cur_desc)
        cur_desc <- trimmed
      } else {
        # Continuation of the current description
        cur_desc <- paste(cur_desc, trimmed)
      }
    }
  }
  # Flush final entry
  if (!is.null(cur_code)) save_entry(cur_code, cur_desc)

  dplyr::bind_rows(entries)
}

## Split Section II pages into independent left and right column streams,
## parse each stream, combine results, deduplicate, and sort by hs6.
.parse_section2_fao <- function(page_texts, hs_version) {
  # Lines to skip: range headers (e.g. "0301.11 - 0302.41"),
  # section title heading, and bare page numbers
  skip_pat <- paste0(
    "\\d{4}\\.\\d{2}\\s*-\\s*\\d{4}\\.\\d{2}",
    "|Full description of fish",
    "|^\\s*\\d{1,3}\\s*$"
  )

  all_left  <- character(0)
  all_right <- character(0)

  for (page_text in page_texts) {
    split_pos <- .detect_col_split_fao(page_text)
    lines <- stringr::str_split(page_text, "\n")[[1]]

    for (line in lines) {
      if (stringr::str_detect(line, skip_pat) || stringr::str_trim(line) == "") next

      if (!is.na(split_pos) && nchar(line) >= split_pos) {
        all_left  <- c(all_left,  stringr::str_trim(substr(line, 1L, split_pos - 1L)))
        all_right <- c(all_right, stringr::str_trim(substr(line, split_pos, nchar(line))))
      } else {
        all_left  <- c(all_left,  stringr::str_trim(line))
        all_right <- c(all_right, "")
      }
    }
  }

  left_df  <- .parse_stream_fao(all_left[nchar(all_left) > 0],   hs_version)
  right_df <- .parse_stream_fao(all_right[nchar(all_right) > 0], hs_version)

  dplyr::bind_rows(left_df, right_df) %>%
    dplyr::filter(!is.na(hs6), hs6 != "", !is.na(description_orig)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(hs6)
}


# Main function ----------------------------------------------------------

#' Extract HS6 product descriptions from the FAO fisheries HS codes handbook
#'
#' Reads the FAO & WCO *HS Codes for Fisheries and Aquaculture Products*
#' handbook PDF and extracts the "Full description of fisheries and aquaculture
#' products" table (Section II) into a tidy data frame suitable for downstream
#' taxa and product matching in the ARTIS pipeline.
#'
#' @details
#' Called during HS code setup to ingest the FAO fisheries HS6 reference table.
#' The output feeds into [clean_hs()], which applies spelling corrections,
#' standardizes genus syntax, and adds taxonomic classification columns.
#'
#' ## PDF structure and parsing
#'
#' The handbook is a two-column, typeset PDF (Adobe InDesign origin). Each page
#' of Section II is parsed as follows:
#'
#' * The column boundary is detected dynamically per page by locating lines that
#'   contain two HS code patterns and computing the median start position of the
#'   second code.
#' * Each page is split into left and right column text streams at that boundary,
#'   and both streams are parsed independently to prevent cross-column text
#'   contamination.
#' * Within each stream, new HS code entries are detected by the pattern
#'   `XXXX.XX  <description>`. Wrapped continuation lines are appended to the
#'   current description.
#' * Section II boundaries are detected using the range header pattern
#'   `XXXX.XX - XXXX.XX` present on every Section II page.
#'
#' ## Multiple descriptions per HS code
#'
#' Some HS codes list more than one product description consecutively under the
#' same code (e.g., the same species under different treatments). Each
#' description is returned as its own row with the `hs6` code repeated. A new
#' description is detected when a continuation line starts with an uppercase
#' letter and the accumulated description text does not end with a grammatical
#' connector (comma, parenthesis, "and", "or", "excluding", etc.). Inspect
#' entries with `dplyr::filter(result, nchar(description_orig) < 10)` to
#' review any potential parsing artifacts.
#'
#' ## HS version extraction
#'
#' The `hs_version` value is extracted from the first six pages of the PDF by
#' matching the pattern `"Nomenclature YYYY"`. If detection fails,
#' `hs_version` is set to `NA_character_` with a warning.
#'
#' @param pdf_path Character. File path or URL to the FAO & WCO fisheries HS
#'   codes handbook PDF. Accepts local paths or direct download URLs (e.g.,
#'   `"https://www.fao.org/3/cc6347en/cc6347en.pdf"`). The file is read with
#'   [pdftools::pdf_text()].
#'
#' @return
#' A data frame with one row per HS6 product description. The output has the
#' following properties:
#'
#' * Rows represent individual product descriptions as listed in Section II of
#'   the handbook. HS codes with multiple descriptions produce multiple rows.
#' * Columns are `hs_version` (character, e.g. `"HS22"`), `hs6` (character,
#'   6-digit code with no period separator, e.g. `"030111"`), and
#'   `description_orig` (character, full description text as extracted).
#' * Rows are sorted ascending by `hs6`.
#' * Passed to [clean_hs()] for standardization and taxonomic annotation.
#'
#' @note The `hs6` column stores codes as 6-character strings without a period
#'   (e.g. `"030111"`, not `"0301.11"`). The `description_orig` column
#'   preserves original case — [clean_hs()] lowercases descriptions as part
#'   of its standardization step.
#'
#' @seealso
#' * [clean_hs()] — receives the output of this function for standardization
#'   and taxa classification annotation
#' * FAO & WCO HS Codes handbook (HS2022):
#'   <https://doi.org/10.4060/cc6347en>
#' * FAO & WCO HS Codes handbook (HS2017):
#'   <https://doi.org/10.4060/cb3813en>
#'
#'
#' ---
#'
#' *Documentation generated with `claude-sonnet-4-5` using the
#' [`extract-fao-hs6-descriptions`](https://github.com/Seafood-Globalization-Lab/lab-genAI-toolbox/commit/3df26c93f2bade5a6440f57b2faf02d34b7e0990)
#' skill (commit `3df26c9`).*
#'
#' @import dplyr
#' @import cli
#' @import stringr
#' @importFrom pdftools pdf_text
#' @export
extract_fao_hs6_descriptions <- function(pdf_path) {

  # Validate dependencies --------------------------------------------------
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    cli::cli_abort(c(
      "!" = "Package {.pkg pdftools} is required but not installed.",
      "i" = "Install it with {.code install.packages('pdftools')}"
    ))
  }

  # Read PDF ---------------------------------------------------------------
  cli::cli_alert_info("Reading PDF: {.file {pdf_path}}")
  pages <- pdftools::pdf_text(pdf_path)
  cli::cli_alert_success("{.val {length(pages)}} page{?s} read from PDF")

  # Extract HS version from document ---------------------------------------
  early_text <- paste(pages[seq_len(min(6L, length(pages)))], collapse = "\n")
  hs_year <- stringr::str_match(early_text, "Nomenclature[^\\d]+(\\d{4})")[1, 2]

  if (is.na(hs_year)) {
    cli::cli_warn(c(
      "!" = "Could not extract HS nomenclature year from document text",
      "i" = "Set {.field hs_version} manually in the returned data frame"
    ))
    hs_version <- NA_character_
  } else {
    hs_version <- paste0("HS", substr(hs_year, 3, 4))
    cli::cli_alert_success("HS version detected: {.val {hs_version}}")
  }

  # Locate Section II page range -------------------------------------------
  # Section II: "Full description of fisheries and aquaculture products"
  # Every Section II page carries a range header: "XXXX.XX - XXXX.XX"
  # This header is unique to Section II and is the most reliable boundary marker.
  # NOTE: Do NOT use "Photo credits" or "Pictures, basic information" to detect
  # the section end — these appear near the end of some editions and would pull
  # Section III species photo pages into the parse.
  section2_start    <- NA_integer_
  section2_end      <- NA_integer_
  range_header_pat  <- "\\d{4}\\.\\d{2}\\s*-\\s*\\d{4}\\.\\d{2}"

  for (i in seq_along(pages)) {
    if (is.na(section2_start) &&
        stringr::str_detect(
          pages[i],
          "Full description of fish"
        )) {
      section2_start <- i
      section2_end   <- i
      next
    }

    if (!is.na(section2_start)) {
      if (stringr::str_detect(pages[i], range_header_pat)) {
        # Still in Section II: advance the end marker
        section2_end <- i
      } else {
        # First page after Section II without a range header: stop
        break
      }
    }
  }

  if (is.na(section2_start)) {
    cli::cli_abort(
      "Could not locate the {.emph Full description of fisheries and aquaculture products} section in the PDF"
    )
  }

  cli::cli_alert_info(
    "Section II located on PDF pages {.val {section2_start}}\u2013{.val {section2_end}}"
  )

  # Parse Section II -------------------------------------------------------
  result <- .parse_section2_fao(pages[section2_start:section2_end], hs_version)

  n_codes <- dplyr::n_distinct(result$hs6)
  cli::cli_alert_success(
    "{.val {nrow(result)}} description row{?s} extracted across {.val {n_codes}} unique HS6 code{?s}"
  )

  # Quality check: flag suspiciously short descriptions --------------------
  n_short <- sum(nchar(result$description_orig) < 10, na.rm = TRUE)
  if (n_short > 0) {
    cli::cli_alert_warning(
      "{.val {n_short}} description{?s} {?is/are} fewer than 10 characters — possible parsing artifact{?s}"
    )
    cli::cli_alert_info(
      "Inspect with: {.code dplyr::filter(result, nchar(description_orig) < 10)}"
    )
  }

  result
}
