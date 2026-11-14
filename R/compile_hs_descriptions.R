# compile_hs_descriptions() ----------------------------
# artis package | R/compile_hs_descriptions.R
#
# Reads WCO HS nomenclature chapter PDFs and returns a tidy data frame with
# one row per hs6 code, capturing the full 3-level description hierarchy.
#
# Compatible with HS editions: HS2002, HS2007, HS2012, HS2017, HS2022.
# Chapters: 03, 15, 16, 23 (fisheries-relevant chapters).


# Internal helpers ----------------------------

## .parse_hs_version_wco() ----------------------------
## Extracts the hs_version string from a WCO HS nomenclature file name.
## Returns e.g. "HS22" from "wco_hs_2022_chapter_03.pdf".
.parse_hs_version_wco <- function(filename) {
  m <- stringr::str_match(basename(filename), "wco_hs_(\\d{4})")
  if (is.na(m[1, 2])) return(NA_character_)
  paste0("HS", substr(m[1, 2], 3, 4))
}

## .detect_hs_col_split_wco() ----------------------------
## Detects the median start position of hs6 codes on a page. Returns NA if
## no hs6 code patterns are found (e.g. a chapter notes page).
.detect_hs_col_split_wco <- function(page_text) {
  lines <- stringr::str_split(page_text, "\n")[[1]]
  positions <- integer(0)
  for (line in lines) {
    m <- gregexpr("\\d{4}\\.\\d{2}", line)[[1]]
    if (length(m) >= 1 && m[1] != -1) {
      positions <- c(positions, m[1])
    }
  }
  if (length(positions) > 0) as.integer(stats::median(positions)) else NA_integer_
}

## .parse_wco_page() ----------------------------
## Parses the text of one or more WCO PDF pages (passed as a single
## character string) into a data frame of hs6 entries. Maintains a
## stateful scan that tracks the current hs4, hs5, and hs6 description
## context across lines.
##
## col_split is accepted for interface consistency but parsing uses
## regex-based detection rather than column-position splitting.
.parse_wco_page <- function(page_text, hs_version, col_split = NA_integer_) {

  lines <- stringr::str_split(page_text, "\n")[[1]]

  # Lines to skip: dividers, bare page numbers, table column headers
  skip_pat <- paste0(
    "^\\s*[_]+\\s*$",          # underline dividers
    "|^\\s*\\d{1,3}\\s*$",     # bare page numbers
    "|^\\s*Heading\\b",         # table column header
    "|^\\s*H\\.S\\.\\s*$",     # H.S. partial header
    "|^\\s*Code\\s*$"           # Code partial header
  )

  # Data-line patterns
  hs6_pat  <- "(\\d{4}\\.\\d{2})\\s+(.*)"   # hs6 code followed by description
  hs4_pat  <- "^\\s*(\\d{2}\\.\\d{2})\\s+"  # hs4 heading code at start of line
  dash_pat <- "^\\s*-\\s+(.*)"               # standalone single-dash group heading

  entries          <- list()
  cur_hs4_desc     <- NULL
  cur_hs5_desc     <- NA_character_
  cur_hs6          <- NULL
  cur_hs6_desc     <- NULL
  # Tracks whether the current hs6 came from a combined hs4+hs6 line,
  # so description continuations are appended to both cur_hs4_desc and cur_hs6_desc.
  hs4_hs6_combined <- FALSE
  state            <- "none"

  save_entry <- function() {
    if (!is.null(cur_hs6)) {
      entries[[length(entries) + 1]] <<- data.frame(
        hs_version = hs_version,
        hs6        = cur_hs6,
        hs4_desc   = if (!is.null(cur_hs4_desc)) stringr::str_squish(cur_hs4_desc) else NA_character_,
        hs5_desc   = if (!is.na(cur_hs5_desc)) stringr::str_squish(cur_hs5_desc) else NA_character_,
        hs6_desc   = stringr::str_squish(cur_hs6_desc),
        stringsAsFactors = FALSE
      )
      cur_hs6 <<- NULL
      cur_hs6_desc <<- NULL
    }
  }

  for (line in lines) {
    trimmed <- stringr::str_trim(line)
    if (nchar(trimmed) == 0) next
    if (stringr::str_detect(line, skip_pat)) next

    # --- Determine whether the line contains a valid hs6 code ------------
    # An hs6 code is valid only when:
    #   (1) nothing but whitespace (or an hs4 heading code) precedes it, AND
    #   (2) the description that follows starts with a dash or a letter.
    # This rejects code references embedded in hs5/hs4 description text
    # (e.g. "subheadings 0302.91 to\n  0302.99 :" where "0302.99 :" is a
    # continuation of the group-heading line, not a true hs6 entry).
    hs6_m     <- stringr::str_match(line, hs6_pat)
    valid_hs6 <- FALSE

    if (!is.na(hs6_m[1, 1])) {
      code_start   <- stringr::str_locate(line, stringr::fixed(hs6_m[1, 2]))[1, 1]
      left_of_code <- substr(line, 1, code_start - 1L)
      desc_raw     <- stringr::str_trim(hs6_m[1, 3])

      # For non-combined lines, description must start with a dash (-- or -).
      # For combined hs4+hs6 lines (e.g. "15.03  1503.00  Lard stearin..."),
      # description starts with a letter — allow it only when an hs4 heading
      # code is also present at the start of the line.
      has_hs4_on_line <- !is.na(stringr::str_match(line, hs4_pat)[1, 1])

      valid_hs6 <- (
        !stringr::str_detect(left_of_code, "[A-Za-z]") &&
        (
          stringr::str_detect(desc_raw, "^-") ||
          (has_hs4_on_line && stringr::str_detect(desc_raw, "^[A-Za-z]"))
        )
      )
    }

    if (valid_hs6) {
      # --- Valid hs6 code line -------------------------------------------
      save_entry()

      cur_hs6 <- gsub("\\.", "", hs6_m[1, 2])

      # Single-dash prefix (- but not --) = hs5-level group heading on the
      # same line as the code; store in cur_hs5_desc and leave cur_hs6_desc NA.
      # Double-dash or letter-start = hs6-level leaf description.
      if (stringr::str_detect(desc_raw, "^-(?!-)")) {
        cur_hs5_desc <- stringr::str_remove(desc_raw, "^-\\s*")
        cur_hs6_desc <- NA_character_
      } else {
        cur_hs6_desc <- stringr::str_remove(desc_raw, "^--?\\s*")
      }

      # Check if an hs4 heading code also appears at the start of this line.
      # This occurs when both the heading (e.g. 15.03) and hs6 code (1503.00)
      # are on the same line, with the description following the hs6 code.
      hs4_on_line  <- stringr::str_match(line, hs4_pat)
      if (!is.na(hs4_on_line[1, 1])) {
        # Combined hs4+hs6 line: description serves as hs4_desc too
        cur_hs4_desc     <- if (!is.na(cur_hs6_desc)) cur_hs6_desc else cur_hs5_desc
        cur_hs5_desc     <- NA_character_
        hs4_hs6_combined <- TRUE
      } else {
        hs4_hs6_combined <- FALSE
      }

      state <- "hs6"

    } else if (stringr::str_detect(line, hs4_pat)) {
      # --- hs4 heading line -----------------------------------------------
      save_entry()

      desc_after       <- stringr::str_trim(stringr::str_remove(line, hs4_pat))
      cur_hs4_desc     <- if (nchar(desc_after) > 0) desc_after else NULL
      cur_hs5_desc     <- NA_character_
      hs4_hs6_combined <- FALSE
      state            <- "hs4"

    } else {
      # --- Standalone single-dash group heading or continuation ----------
      dash_m <- stringr::str_match(line, dash_pat)

      if (!is.na(dash_m[1, 1])) {
        save_entry()
        cur_hs5_desc     <- stringr::str_trim(dash_m[1, 2])
        hs4_hs6_combined <- FALSE
        state            <- "hs5"

      } else if (stringr::str_detect(trimmed, "[A-Za-z0-9]")) {
        # Continuation line: append to whichever description is active
        if (state == "hs4" && !is.null(cur_hs4_desc)) {
          cur_hs4_desc <- paste(cur_hs4_desc, trimmed)
        } else if (state == "hs5" && !is.na(cur_hs5_desc)) {
          cur_hs5_desc <- paste(cur_hs5_desc, trimmed)
        } else if (state == "hs6" && !is.null(cur_hs6)) {
          # When the code line had a single-dash description (cur_hs6_desc is NA),
          # continuations belong to cur_hs5_desc, not cur_hs6_desc
          if (!is.na(cur_hs6_desc)) {
            cur_hs6_desc <- paste(cur_hs6_desc, trimmed)
            if (hs4_hs6_combined) cur_hs4_desc <- paste(cur_hs4_desc, trimmed)
          } else {
            cur_hs5_desc <- paste(cur_hs5_desc, trimmed)
          }
        }
      }
    }
  }

  # Flush the final pending entry
  save_entry()

  dplyr::bind_rows(entries)
}

## .combine_wco_pages() ----------------------------
## Processes all pages of a single WCO PDF, filtering out non-data pages
## (chapter title/notes pages that contain no hs6 codes). The remaining
## content pages are combined into a single text stream so that parsing
## state (cur_hs4_desc, cur_hs5_desc) is maintained correctly across
## page breaks.
.combine_wco_pages <- function(pages, hs_version) {
  # Filter to pages that contain at least one hs6 code
  has_codes     <- stringr::str_detect(pages, "\\d{4}\\.\\d{2}")
  content_pages <- pages[has_codes]

  if (length(content_pages) == 0) {
    return(dplyr::tibble(
      hs_version = character(),
      hs6        = character(),
      hs4_desc   = character(),
      hs5_desc   = character(),
      hs6_desc   = character()
    ))
  }

  # Parse all content pages as one stream to preserve context across pages
  combined_text <- paste(content_pages, collapse = "\n")
  result        <- .parse_wco_page(combined_text, hs_version)

  result %>%
    dplyr::filter(!is.na(hs6), hs6 != "", !is.na(hs6_desc) | !is.na(hs5_desc)) %>%
    dplyr::distinct() %>%
    dplyr::arrange(hs6)
}


# Main function ----------------------------

#' Parse WCO HS nomenclature PDFs into a tidy hs6 description table
#'
#' Reads all WCO HS nomenclature chapter PDFs from a directory and returns a
#' tidy data frame with one row per hs6 code, capturing the full three-level
#' description hierarchy (hs4, hs5, hs6) from each PDF.
#'
#' @details
#' ## PDF structure and parsing
#'
#' Each WCO HS nomenclature PDF uses a two-column layout (a "Heading" column
#' for 4-digit codes and an "H.S. Code" column for 6-digit codes). Descriptions
#' appear at three indentation levels:
#'
#' * **hs4-level** — no dash prefix; appears next to the 4-digit heading code
#'   (e.g. `03.01   Live fish.`). Establishes `hs4_desc` for all codes beneath
#'   it and resets `hs5_desc` to `NA`.
#' * **hs5-level** — prefixed by `- ` (single dash) on a standalone line with
#'   no 6-digit code; acts as a group heading for the hs6 codes below it.
#'   Stored in `hs5_desc` for all subsequent codes until the next group heading
#'   or new hs4 heading.
#' * **hs6-level** — appears on the same line as the 6-digit code; prefixed by
#'   either `-- ` (double dash, subdivided heading) or `- ` (single dash, leaf
#'   with no deeper subdivision).
#'
#' ## Two roles of single-dash lines
#'
#' A single-dash line plays one of two structural roles determined by whether a
#' 6-digit code appears on the same line:
#'
#' * **Code present** — the `-` text is the hs6-level description for that code.
#'   `hs5_desc` is inherited from the nearest preceding standalone group heading
#'   (or `NA` if none). `cur_hs5_desc` is not changed.
#' * **No code (standalone group heading)** — the `-` text becomes `hs5_desc`
#'   for all subsequent codes until the next group heading or hs4 heading.
#'
#' ## Combined hs4 + hs6 lines
#'
#' Some headings (e.g. `15.03  1503.00  Lard stearin...`) place both the hs4
#' heading code and the hs6 code on the same line. In this case the description
#' is used for both `hs4_desc` and `hs6_desc`, and `hs5_desc` is `NA`.
#'
#' ## State continuity across page breaks
#'
#' Content pages for each PDF are concatenated before parsing so that the
#' current `hs4_desc` and `hs5_desc` context is maintained correctly across
#' page boundaries (page-header lines are skipped automatically).
#'
#' ## Pages skipped
#'
#' Chapter title and legal-notes pages are skipped automatically: any page
#' that contains no hs6 code pattern (`\d{4}\.\d{2}`) is excluded from
#' parsing.
#'
#' @param dir_path Character. Path to the directory containing the WCO HS
#'   nomenclature PDFs. All files matching `wco_hs_*.pdf` are read and
#'   combined. The directory must exist.
#' @param outdir Character. Path to the directory where the cleaned data frame
#'   is written as `wco_hs_descriptions.csv`. The directory must exist; it is
#'   not created by this function.
#'
#' @return
#' A data frame with one row per hs6 code per chapter per hs_version. The
#' output has the following properties:
#'
#' * Columns: `hs_version` (character, e.g. `"HS22"`), `hs6` (character,
#'   6-digit code with no period, e.g. `"030111"`), `hs4_desc` (character,
#'   nearest parent heading description), `hs5_desc` (character, nearest
#'   preceding standalone group heading or `NA`), `hs6_desc` (character,
#'   description from the same line as the code, leading dashes stripped).
#' * `hs6_desc` is never `NA`.
#' * `hs5_desc` is `NA` for codes that fall under no group heading within
#'   their hs4 heading.
#' * Rows are sorted by `hs_version`, then `hs6`.
#' * Written to `outdir/wco_hs_descriptions.csv` as a side effect.
#' * Returned invisibly.
#'
#' @note Descriptions are returned with original casing and punctuation,
#'   including trailing colons on group headings (e.g. `"Ornamental fish :"`).
#'   Downstream standardization (lowercasing, punctuation cleanup) should be
#'   applied by the caller.
#'
#' @seealso
#' * WCO HS nomenclature: <https://www.wcoomd.org/en/topics/nomenclature/instrument-and-tools/hs-nomenclature-2022-edition.aspx>
#'
#'
#' *Documentation generated with `claude-sonnet-4-5` using the
#' [`roxygen2-function-documentation`](https://github.com/Seafood-Globalization-Lab/lab-genAI-toolbox/commit/40860d41b1ae5f18742dd264855c20b2e2bbb10d)
#' skill (commit `40860d4`).*
#'
#' @import dplyr
#' @import cli
#' @import stringr
#' @importFrom pdftools pdf_text
#' @importFrom data.table fwrite
#' @export
compile_hs_descriptions <- function(dir_path, outdir) {

  # Validate inputs ----------------------------
  if (!dir.exists(dir_path)) {
    cli::cli_abort(c(
      "!" = "{.arg dir_path} does not exist: {.path {dir_path}}"
    ))
  }
  if (!dir.exists(outdir)) {
    cli::cli_abort(c(
      "!" = "{.arg outdir} does not exist: {.path {outdir}}",
      "i" = "Create the directory before calling {.fn compile_hs_descriptions}"
    ))
  }

  pdf_files <- list.files(
    dir_path,
    pattern    = "^wco_hs_.*\\.pdf$",
    full.names = TRUE
  )
  if (length(pdf_files) == 0) {
    cli::cli_abort(c(
      "!" = "No files matching {.code wco_hs_*.pdf} found in {.path {dir_path}}"
    ))
  }

  # Read and parse each PDF ----------------------------
  all_results <- vector("list", length(pdf_files))

  for (i in seq_along(pdf_files)) {
    f          <- pdf_files[i]
    hs_version <- .parse_hs_version_wco(f)
    fname      <- basename(f)

    cli::cli_alert_info(
      "Reading {.file {fname}} [{.val {hs_version}}]"
    )

    pages  <- pdftools::pdf_text(f)
    result <- .combine_wco_pages(pages, hs_version)
    n_rows <- nrow(result)

    cli::cli_alert_success(
      "{.val {n_rows}} row{?s} extracted from {.file {fname}}"
    )

    all_results[[i]] <- result
  }

  # Combine and sort ----------------------------
  combined <- dplyr::bind_rows(all_results) %>%
    dplyr::arrange(hs_version, hs6)

  n_files  <- length(pdf_files)
  n_unique <- dplyr::n_distinct(combined$hs6)

  cli::cli_alert_success(
    "{.val {nrow(combined)}} total row{?s} from {.val {n_files}} file{?s} | {.val {n_unique}} unique hs6 code{?s}"
  )

  # Quality check: flag suspiciously short hs6 descriptions ----------------------------
  # Threshold is 3 characters: legitimate short descriptions ("Carp", "Crab", "Lard",
  # "Eels") are 4+ chars; artifacts from code-reference continuations would be 1-2 chars.
  n_short <- sum(nchar(combined$hs6_desc) < 3L, na.rm = TRUE)
  if (n_short > 0) {
    cli::cli_alert_warning(
      "{.val {n_short}} {.field hs6_desc} value{?s} {?is/are} fewer than 3 characters — possible parsing artifact{?s}"
    )
    cli::cli_alert_info(
      "Inspect with: {.code dplyr::filter(result, nchar(hs6_desc) < 3)}"
    )
  }

  # Write output ----------------------------
  out_path <- file.path(outdir, "wco_hs_descriptions.csv")
  data.table::fwrite(combined, out_path)
  cli::cli_alert_success("Written to {.file {out_path}}")

  invisible(combined)
}
