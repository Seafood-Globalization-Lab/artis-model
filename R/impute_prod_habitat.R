#' Impute final habitat column in production data
#'
#' Reconciles FAO-reported habitat with FishBase / SeaLifeBase habitat at the
#' species level to produce the final `habitat`, `fao_habitat`, `prod_method`,
#' and `taxa_source` columns in `prod_data`.
#'
#' @details
#' Called in `01-clean-input-data.R` after the SciName correction join and
#' after [fill_prod_taxa_ranks()] has finalized `prod_taxa_classification`.
#' Output is assigned to `prod_data`.
#'
#' ## Habitat reconciliation
#'
#' Habitat information enters at two resolutions:
#'
#' * **Taxa level** — `prod_taxa$habitat_fb`, derived from FishBase /
#'   SeaLifeBase per `SciName`.
#' * **Record level** — `prod_data$habitat`, reported by FAO per production
#'   record.
#'
#' A per-`SciName` reference table (`prod_taxa_habitat`) is extracted from
#' `prod_taxa` and joined to `prod_data`. The FishBase / SeaLifeBase
#' designation (`habitat_fb`) takes precedence over the FAO designation
#' (`fao_habitat`) when both of the following are true:
#'
#' 1. `SciName` contains a space (i.e., is a species-level name).
#' 2. `habitat_fb` is one of `"inland"` or `"marine"`.
#'
#' For all coarser taxa (genus, family, order, etc.) and when the two sources
#' agree, the FAO designation is used.
#'
#' @param prod_taxa Data frame. The finalized production taxa classification
#'   table containing `SciName`, `Fresh01`, `Brack01`, `Saltwater01`, and
#'   `habitat_fb` columns. Typically `prod_taxa_classification` after
#'   [fill_prod_taxa_ranks()].
#' @param prod_data Data frame. Production data after the SciName correction
#'   join, containing at minimum `SciName` and `habitat` columns. `SciName`
#'   here should already reflect corrected / resolved names (i.e., joined from
#'   `prod_taxa_classification$SciName` via `SciName_prod`).
#'
#' @return
#' A data frame with the same rows as `prod_data` with the following columns
#' added or updated:
#'
#' * `fao_habitat` — standardized FAO habitat label (`"inland"`, `"marine"`,
#'   or the original `habitat` value).
#' * `prod_method` — standardized production method (`"aquaculture"` or
#'   `"capture"`).
#' * `Fresh01`, `Brack01`, `Saltwater01` — binary habitat flags joined from
#'   `prod_taxa`.
#' * `habitat_fb` — FishBase / SeaLifeBase habitat label joined from
#'   `prod_taxa`.
#' * `habitat` — final reconciled habitat: `habitat_fb` where it overrides
#'   FAO, otherwise `fao_habitat`.
#' * `taxa_source` — composite key combining `SciName`, `habitat`, and
#'   `prod_method`, used in downstream ARTIS matching functions.
#'
#' @seealso
#' * [correct_taxa_habitat()] — corrects habitat encoding in `prod_taxa` before
#'   this function is called
#' * [fill_prod_taxa_ranks()] — finalizes `prod_taxa` directly upstream
#'
#' @import dplyr
#' @import stringr
#' @import cli
#' @importFrom magrittr %>%
#' @export

impute_prod_habitat <- function(prod_taxa, prod_data) {

  # Build per-SciName habitat reference from taxa table --------------------

  prod_taxa_habitat <- prod_taxa %>%
    select(SciName, Fresh01, Brack01, Saltwater01, habitat_fb) %>%
    distinct()

  # Reconcile FAO and FB/SLB habitat at the production-record level --------

  prod_data_out <- prod_data %>%
    # Standardize FAO habitat and production method labels
    mutate(
      fao_habitat = case_when(
        habitat == "Inland waters" ~ "inland",
        habitat == "Marine areas"  ~ "marine",
        TRUE ~ habitat
      ),
      prod_method = case_when(
        prod_method %in% c("FRESHWATER", "MARINE", "BRACKISHWATER") ~ "aquaculture",
        prod_method == "CAPTURE" ~ "capture",
        TRUE ~ prod_method
      )
    ) %>%
    # Join FishBase / SeaLifeBase habitat flags
    left_join(prod_taxa_habitat, by = "SciName") %>%
    # Use FB/SLB habitat when it conflicts with FAO for species-level names only
    mutate(
      habitat = case_when(
        str_detect(SciName, " ") &
          habitat_fb != fao_habitat &
          habitat_fb %in% c("inland", "marine") ~ habitat_fb,
        TRUE ~ fao_habitat
      )
    ) %>%
    mutate(
      taxa_source = paste(str_replace(SciName, " ", "."), habitat, prod_method, sep = "_")
    )

  cli::cli_alert_success(
    "Production data habitat reconciled — {.field habitat} column updated from FAO and FishBase / SeaLifeBase sources"
  )

  return(prod_data_out)
}
