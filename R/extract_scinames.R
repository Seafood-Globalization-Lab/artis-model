# extract_scinames() — Extract Scientific Names from HS Description Columns
# artis package | R/extract_scinames.R


# Regex patterns --------------------------

## Higher-rank taxa: family (-idae, -inae), superfamily (-oidea), order (-formes) suffixes.
## Safe to apply to full raw text — these Latin suffixes produce zero false positives
## in HS description data.
PATTERN_HIGHER_RANK <- stringr::regex("[A-Z][a-z]+(?:idae|inae|oidea|formes)\\b")

## Binomial and genus+spp. names.
## Applied to parenthetical content only to avoid false positives in raw HS text
## (e.g. "Pacific salmon", "Flat fish" match a naive binomial regex in raw text).
PATTERN_BINOMIAL <- stringr::regex("[A-Z][a-z]+\\s+(?:spp\\.|[a-z]+(?:-[a-z]+)?)")


# Internal helpers --------------------------

## Extract all taxonomic names from a single text string using a two-track approach.
## Returns a deduplicated character vector, or character(0) for NA input.
.extract_taxa_from_string <- function(text) {
  if (is.na(text)) return(character(0))

  # Track 1: Higher-rank taxa from full raw text --------------------------
  track1 <- stringr::str_extract_all(text, PATTERN_HIGHER_RANK)[[1]]

  # Track 2: Binomials and genus+spp. from parenthetical content ----------

  ## Pre-process: normalize subgenus notation before paren extraction to
  ## preserve the species epithet. Example:
  ##   "Euthynnus (Katsuwonus) pelamis" → "Katsuwonus pelamis"
  preprocessed <- stringr::str_replace_all(
    text,
    "([A-Z][a-z]+) \\(([A-Z][a-z]+)\\) ([a-z]+(?:-[a-z]+)?)",
    "\\2 \\3"
  )

  ## Extract all parenthetical content (non-nested parens only)
  paren_chunks <- stringr::str_extract_all(
    preprocessed,
    "(?<=\\()([^()]+)(?=\\))"
  )[[1]]

  if (length(paren_chunks) == 0L) {
    return(unique(track1))
  }

  ## Apply binomial regex to each paren chunk and flatten results
  track2_binomials <- purrr::map(paren_chunks, \(chunk) {
    stringr::str_extract_all(chunk, PATTERN_BINOMIAL)[[1]]
  }) |> unlist()

  ## Post-process: "of the genus X" → "X spp."
  ## Handles entries like "(of the genus Thunnus)" where no binomial is present
  genus_matches <- purrr::map(paren_chunks, \(chunk) {
    m <- stringr::str_match_all(chunk, "of the genus ([A-Z][a-z]+)")[[1]]
    if (nrow(m) == 0L) return(character(0))
    paste0(m[, 2], " spp.")
  }) |> unlist()

  track2 <- c(track2_binomials, genus_matches)

  # Remove Track 2 results where the "genus" token is actually a family/order name
  track2 <- track2[!stringr::str_detect(track2, "^[A-Z][a-z]+(?:idae|inae|oidea|formes)\\b")]

  # Combine both tracks and deduplicate --------------------------
  unique(c(track1, track2))
}


# Main function --------------------------

#' Extract scientific names from an HS description text column
#'
#' Scans a text column of an HS code description data frame and extracts all
#' taxonomic names — binomials, genus-level `spp.` names, and higher-rank taxa
#' (families, orders) — returning the data frame with a new list-column of
#' extracted names per row.
#'
#' @details
#' ## Two-track extraction strategy
#'
#' A two-track approach is required because binomial names and higher-rank names
#' behave differently in HS description text:
#'
#' * **Track 1 — Higher-rank taxa** (`PATTERN_HIGHER_RANK`): Latin suffixes
#'   `-idae`, `-inae`, `-oidea`, and `-formes` are distinctive enough to scan
#'   the full raw text string without false positives. Applied directly to
#'   `df[[col]]`.
#'
#' * **Track 2 — Binomials and genus+spp.** (`PATTERN_BINOMIAL`): Applying a
#'   binomial regex to raw HS text produces many false positives (e.g. "Pacific
#'   salmon", "Flat fish"). The safe approach is paren-first: extract
#'   parenthetical content, then apply the binomial regex only within those
#'   substrings.
#'
#' ## Pre- and post-processing steps
#'
#' Before parenthetical content is extracted, subgenus notation is normalized:
#' `Euthynnus (Katsuwonus) pelamis` becomes `Katsuwonus pelamis` so the species
#' epithet is not lost when parens are stripped. After paren extraction, entries
#' matching `"of the genus X"` are detected and returned as `"X spp."`.
#'
#' Results from both tracks are combined and deduplicated per row.
#'
#' @param df Data frame. Input data frame containing the text column to process.
#' @param col Character. Name of the column in `df` from which to extract
#'   scientific names. Must be a single string naming an existing column.
#'
#' @return
#' The input `df` with one additional list-column named `<col>_scinames`
#' (e.g. `"hs6_desc_scinames"` when `col = "hs6_desc"`). Each element of the
#' list-column is a `character` vector of extracted taxonomic names for that row.
#' Rows with no matches return `character(0)`. Returned invisibly.
#'
#' @note
#' Genus-level names with trailing `spp.` are retained as-is (e.g.
#' `"Penaeus spp."`), not stripped to just the genus. Rows where `col` is `NA`
#' return `character(0)` silently — no warning is emitted. The typo
#' `Huchohucho` (should be `Hucho hucho`) will not match the binomial regex and
#' is silently skipped; typo correction is an upstream concern.
#'
#'
#' @importFrom stringr str_extract_all str_replace_all str_match_all
#' @importFrom purrr map
#' @importFrom cli cli_abort
#' @export
extract_scinames <- function(df, col) {

  # Validate inputs --------------------------
  if (!is.data.frame(df)) {
    cli::cli_abort("{.arg df} must be a data frame, not {.cls {class(df)[1]}}")
  }
  if (!is.character(col) || length(col) != 1L) {
    cli::cli_abort("{.arg col} must be a single character string")
  }
  if (!col %in% names(df)) {
    cli::cli_abort(
      "Column {.field {col}} not found in {.arg df}",
      call = NULL
    )
  }

  # Extract scinames for each row --------------------------
  df[[paste0(col, "_scinames")]] <- purrr::map(df[[col]], .extract_taxa_from_string)

  invisible(df)
}
