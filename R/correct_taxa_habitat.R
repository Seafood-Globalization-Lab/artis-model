#' Correct habitat encoding in production taxa classification
#'
#' Runs a diagnostic check for taxa missing habitat coding and applies manual
#' `Fresh01` corrections for known taxa before habitat-based calculations
#' operate on `prod_data`.
#'
#' @details
#' Called in `01-clean-input-data.R` after [correct_prod_common_names()] and
#' the inline manual taxonomy corrections, before [fill_prod_taxa_ranks()].
#' Output is assigned back to `prod_taxa_classification`.
#'
#' ## Diagnostic check
#'
#' Computes `habitat_sum` (`Fresh01 + Brack01 + Saltwater01`) for every row
#' and emits a `cli` warning for any taxa where the sum is `0` or `NA`.
#' Missing habitat codes may be intentional (e.g., taxa recorded at a coarse
#' rank with no single habitat assignment); the warning is informational and
#' does not stop execution. Add manual corrections to this function when a
#' missing code requires a fix.
#'
#' ## Manual habitat corrections
#'
#' Applies `Fresh01 = 1` for taxa that are known freshwater species but whose
#' FishBase / SeaLifeBase records lack a freshwater habitat code:
#'
#' * *Neocaridina denticulata*
#' * *Caridina nilotica*
#'
#' @param prod_taxa Data frame. The production taxa classification table
#'   containing at minimum `SciName`, `Fresh01`, `Brack01`, and `Saltwater01`
#'   columns. Typically `prod_taxa_classification` after
#'   [correct_prod_common_names()] and the inline taxonomy corrections in
#'   `01-clean-input-data.R`.
#'
#' @return A data frame with the same structure as `prod_taxa` with manual
#'   `Fresh01` corrections applied. Assigned to `prod_taxa_classification` in
#'   `01-clean-input-data.R`.
#'
#' @seealso
#' * [correct_prod_common_names()] — called directly upstream
#' * [fill_prod_taxa_ranks()] — called directly downstream
#' * [impute_prod_habitat()] — uses the corrected habitat columns to reconcile
#'   FAO-reported habitat with FishBase / SeaLifeBase at the production-record
#'   level
#'
#' @import dplyr
#' @import cli
#' @importFrom magrittr %>%
#' @export

correct_taxa_habitat <- function(prod_taxa) {

  # Diagnostic: missing habitat check ---------------------------------------

  missing_habitat_scinames <- prod_taxa %>%
    mutate(habitat_sum = Fresh01 + Brack01 + Saltwater01) %>%
    filter(habitat_sum == 0 | is.na(habitat_sum))

  cli::cli_h2("Missing Habitat information - production taxa data")

  if (nrow(missing_habitat_scinames) > 0) {
    cli::cli_alert_warning(
      "{nrow(missing_habitat_scinames)} {.field SciName}{?s} missing habitat information"
    )
    cli::cli_alert_info(
      "{.field SciName} without habitat coding: {.val {missing_habitat_scinames$SciName}}"
    )
    cli::cli_alert_info(
      "Check {.field Fresh01}, {.field Brack01}, and {.field Saltwater01} columns in {.var prod_taxa_classification}"
    )
    cli::cli_alert_info(
      "Add manual fixes to {.fn correct_taxa_habitat}"
    )
    cli::cli_alert_info("Some missing habitat encodings may be expected — verify before adding corrections")
  } else {
    cli::cli_alert_success("All taxa have habitat coding in {.field Fresh01}, {.field Brack01}, or {.field Saltwater01}")
  }

  # Manual habitat corrections ----------------------------------------------

  prod_taxa <- prod_taxa %>%
    mutate(
      Fresh01 = case_when(
        SciName %in% c(
          "neocaridina denticulata",
          "caridina nilotica"
        ) ~ as.integer(1),
        TRUE ~ as.integer(Fresh01)
      )
    )

  return(prod_taxa)
}
