#' Correct habitat encoding in production taxa classification
#'
#' Runs a diagnostic check for taxa missing habitat coding and applies manual
#' `Fresh01` corrections for known taxa before habitat-based calculations
#' operate on `prod_data` downstream.
#'
#' @details
#' Called in `01-clean-input-data.R`.
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
#' Uses an internal tribble `corrections_habitat` to hold manual corrections 
#' to FB / SLB aquarium habitat assignment columns. 
#'
#' ## Calculate `habitat_fb` column
#' 
#' Use binary encoded columns in `prod_taxa` (`Fresh01 + Brack01 + Saltwater01`) 
#' to calculate the `habitat_fb` column which represents taxa specific habitat. 
#' This is used downstream along with FAO production record habitat information
#' to create the final habitat assignment used within ARTIS. 
#'
#' 
#' @param prod_taxa Data frame. The production taxa classification table
#'   containing at minimum `SciName`, `Fresh01`, `Brack01`, and `Saltwater01`
#'   columns. Typically `prod_taxa_classification`.
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
#' @importFrom tibble tribble
#' @importFrom magrittr %>%
#' @export

calc_taxa_habitat <- function(prod_taxa) {

  # Diagnostic: missing habitat check ---------------------------------------

  missing_habitat_scinames <- prod_taxa %>%
    mutate(habitat_sum = Fresh01 + Brack01 + Saltwater01) %>%
    filter(habitat_sum == 0 | is.na(habitat_sum)) %>% 
    select(SciName, habitat_sum)

  cli::cli_h2("Production Taxa Habitat Check")

  if (nrow(missing_habitat_scinames) > 0) {
    cli::cli_alert_warning(
      "{nrow(missing_habitat_scinames)} {.field SciName}{?s} missing habitat information"
    )
    cli::cli_alert_info(
      "{.field SciName} without habitat coding: {.val {missing_habitat_scinames$SciName}}"
    )
    cli::cli_alert_info(
      "Check {.field Fresh01}, {.field Brack01}, and {.field Saltwater01} columns in {.var prod_taxa_classification} data frame"
    )
    cli::cli_alert_info(
      "Add manual fixes to {.fn calc_taxa_habitat}"
    )
    cli::cli_alert_info("Some missing habitat encodings may be expected — verify before adding corrections")
  } else {
    cli::cli_alert_success("All taxa have habitat coding in {.field Fresh01}, {.field Brack01}, or {.field Saltwater01}")
  }

  # Manual habitat corrections ----------------------------------------------

  # Add manual corrections to this Tribble
  corrections_habitat <- tribble(
    ~SciName,                   ~Fresh01,
    "neocaridina denticulata",  1L,
    "caridina nilotica",        1L
  )

  prod_taxa <- prod_taxa %>%
    rows_update(corrections_habitat, by = "SciName", unmatched = "ignore") 

  # Calculate Prod Taxa habitat (FB/SLB) values ----------------------------

  # calculate habitat column based on fb/slb aquarium table binary encoded columns
  # used downstream with prod_fao habitat to impute habitat value for ARTIS

  prod_taxa <- prod_taxa %>% 
    mutate(
      habitat_fb = case_when(
        Fresh01 == 1 & Saltwater01 == 0 ~ "inland",
        Fresh01 == 0 & Saltwater01 == 1 ~ "marine",
        Fresh01 == 1 & Saltwater01 == 1 ~ "diadromous",
        # If a species just exists in brackish water we classify as marine
        Brack01 == 1 & Fresh01 == 0 & Saltwater01 == 0 ~ "marine",
        TRUE ~ as.character(NA)
      )
    )

  return(prod_taxa)
}
