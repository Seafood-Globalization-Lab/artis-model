#' Resolve a vector of scientific names against FishBase and SeaLifeBase synonym tables
#'
#' Queries FishBase and SeaLifeBase synonym tables for each name in `scinames`
#' and returns a lookup table documenting the resolution outcome for every
#' input name. A sciname exists in either FishBase or SeaLifeBase, not both —
#' both tables are checked independently and only one will ever resolve for a
#' given name.
#'
#' @details
#' Called inside [match_prod_taxa_to_fbslb()]; The returned lookup table is used to apply
#' name swaps to `prod_ts` and to extend `prod_fb_full` / `prod_slb_full`
#' with newly resolved classification rows.
#'
#' ## Name cleaning
#'
#' Each sciname is cleaned before matching: lowercased, dots removed, commas
#' removed, and hyphens replaced with spaces. This mirrors the cleaning applied
#' in [query_synonyms()].
#'
#' ## Assumption violations
#'
#' Each synonym string is assumed to map to a single accepted name. If a
#' synonym string matches more than one row in the FB or SLB synonym table,
#' the per-name `try()` block captures the error and records
#' `status = "assumption_violation_fb"` or `"assumption_violation_slb"` in
#' the lookup table rather than aborting. A single `cli` warning is emitted
#' after the full table is assembled if any violations are present.
#'
#' The upstream function [clean_fb_slb_synonyms()] checks for and documents
#' synonym string ambiguity at data preparation time. Manual corrections there
#' are the resolution path for assumption violations.
#'
#' @param scinames Character vector. Taxa scientific names to resolve
#'   against the synonym tables. Typically the subset of unmatched names from
#'   the hierarchical FB/SLB joins in [match_prod_taxa_to_fbslb()] that
#'   contain a space (i.e. binomial names only).
#' @param fb_synonyms Data frame. FishBase synonym corrections table as read
#'   from `fb_synonyms_clean.csv`. Produced by [clean_fb_slb_synonyms()] and
#'   written to disk by [collect_fb_slb_data()]. Expected columns include
#'   `synonym` and `accepted_name`.
#' @param slb_synonyms Data frame. SeaLifeBase synonym corrections table as
#'   read from `slb_synonyms_clean.csv`. Same structure as `fb_synonyms`.
#'
#' @return
#' A data frame with one row per input sciname. Columns:
#'
#' * `sciname_original` — the input name, uncleaned.
#' * `sciname_accepted` — the accepted name if resolved; `NA` otherwise.
#' * `source` — `"fb"`, `"slb"`, or `NA` if unresolved or violation.
#' * `resolved` — `TRUE` if an accepted name was found; `FALSE` otherwise.
#' * `status` — one of `"resolved_fb"`, `"resolved_slb"`, `"unresolved"`,
#'   `"assumption_violation_fb"`, `"assumption_violation_slb"`.
#'
#' @seealso
#' * [match_prod_taxa_to_fbslb()] — calls this function; returns the lookup
#'   table as `$synonym_resolution`
#' * [clean_fb_slb_synonyms()] — produces the synonym tables consumed here;
#'   also checks for and documents synonym string ambiguity
#' * [collect_fb_slb_data()] — writes `fb_synonyms_clean.csv` and
#'   `slb_synonyms_clean.csv` to disk
#' * [query_synonyms()] — single-name synonym query used by other pipeline
#'   functions; applies the same name cleaning logic
#'
#' @import dplyr
#' @import cli
#' @importFrom magrittr %>%
#' @export

resolve_synonyms <- function(scinames, fb_synonyms, slb_synonyms) {

  results <- lapply(scinames, function(sciname_i) {

    # FIXIT - AM 2026-06-26 doesn't make sense to do this if we are tryign to identify scinames for correction
    # # Clean the sciname
    # sciname_clean <- tolower(sciname_i)
    # sciname_clean <- gsub("\\.", "",  sciname_clean)
    # sciname_clean <- gsub(",",  "",   sciname_clean)
    # sciname_clean <- gsub("-",  " ",  sciname_clean)

    # Query FishBase synonyms
    fb_result <- try({
      fb_match <- fb_synonyms %>% filter(synonym == sciname_i)
      if (nrow(fb_match) > 1) stop("multiple rows matched")
      fb_match
    }, silent = TRUE)

    if (inherits(fb_result, "try-error")) {
      return(data.frame(
        sciname_original = sciname_i,
        sciname_accepted = NA_character_,
        source           = "fb",
        resolved         = FALSE,
        status           = "assumption_violation"
      ))
    }

    if (nrow(fb_result) == 1) {
      return(data.frame(
        sciname_original = sciname_i,
        sciname_accepted = fb_result$accepted_name,
        source           = "fb",
        resolved         = TRUE,
        status           = "resolved"
      ))
    }

    # Query SeaLifeBase synonyms
    slb_result <- try({
      slb_match <- slb_synonyms %>% filter(synonym == sciname_i)
      if (nrow(slb_match) > 1) stop("multiple rows matched")
      slb_match
    }, silent = TRUE)

    if (inherits(slb_result, "try-error")) {
      return(data.frame(
        sciname_original = sciname_i,
        sciname_accepted = NA_character_,
        source           = "slb",
        resolved         = FALSE,
        status           = "assumption_violation"
      ))
    }

    if (nrow(slb_result) == 1) {
      return(data.frame(
        sciname_original = sciname_i,
        sciname_accepted = tolower(slb_result$accepted_name),
        source           = "slb",
        resolved         = TRUE,
        status           = "resolved"
      ))
    }

    # Neither FB nor SLB resolved this name
    data.frame(
      sciname_original = sciname_i,
      sciname_accepted = NA_character_,
      source           = NA_character_,
      resolved         = FALSE,
      status           = "unresolved"
    )
  }) # end of lapply()

  out <- bind_rows(results)

  # Emit a single warning if any assumption violations are present
  n_violations <- sum(out$status %in% c("assumption_violation"))

  if (n_violations > 0) {
    cli::cli_h2("Synonym assumption violation in {.fn resolve_synonyms}")
    cli::cli_alert_warning("{n_violations} violation{?s} detected in the {.fn resolve_synonyms} taxa vector supplied to {.field sciname} argument.")
    cli::cli_alert_info("Filter the returned dataframe column {.var synonym_resolution$status} by {.val assumption_violation} to inspect.")
    cli::cli_alert_info("Apply a manual correction in {.fn clean_fb_slb_synonyms} and rerun {.fn collect_fb_slb_data}.")
  }

  return(out)
}
