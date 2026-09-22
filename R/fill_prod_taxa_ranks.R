#' Fill universal taxonomic rank gaps in production taxa classification
#'
#' Applies rule-based gap-filling for `Kingdom`, `Phylum`, and `Infraclass`
#' to the production taxa classification table.
#'
#' @details
#' Called in `01-clean-input-data.R` after [correct_taxa_habitat()] and the
#' inline manual taxonomy corrections. Output is assigned back to
#' `prod_taxa_classification` and written to `clean_fao_taxa.csv`.
#'
#' All gap-filling rules are deterministic and derived from adjacent rank
#' values already present in the table:
#'
#' * **`Kingdom`** — set to `"animalia"` for all rows.
#' * **`Phylum`** — derived from `Superclass` (for chordate superclasses) and
#'   `Class` (for `"thecostraca"`). All other rows retain their existing
#'   `Phylum` value.
#' * **`Infraclass`** — derived from `Order` for shark and ray clades, and
#'   assigned directly by `SciName` for the synthetic `"batoidea"` and
#'   `"selachii"` rows added upstream in `01-clean-input-data.R`.
#'
#' @param prod_taxa Data frame. The production taxa
#'   classification table after [correct_taxa_habitat()] and the inline manual
#'   taxonomy corrections in `01-clean-input-data.R`. Typically
#'   `prod_taxa_classification`.
#'
#' @return
#' A data frame with the same rows as `prod_taxa` with
#' `Kingdom`, `Phylum`, and `Infraclass` columns filled and `Infraclass`
#' relocated to follow `Order`. Assigned to `prod_taxa_classification` in
#' `01-clean-input-data.R`.
#'
#' @seealso
#' * [correct_taxa_habitat()] — called directly upstream
#' * [impute_prod_habitat()] — uses the finalized taxa table to reconcile
#'   production-record habitat
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @export

fill_prod_taxa_ranks <- function(
  prod_taxa) {

  # Fill Kingdom (universally animalia) ------------------------------------

  prod_taxa_expanded <- prod_taxa %>%
    mutate(Kingdom = "animalia") %>%

  # Fill missing Phylum ----------------------------------------------------

    mutate(Phylum = case_when(
      Superclass %in%
        c(
          "osteichthyes",
          "chondrichthyes",
          "agnatha",
          "sarcopterygii"
        ) ~ "chordata",
      Class %in%
        c(
          "thecostraca"
        ) ~ "anthropoda",
      .default = Phylum
    )) %>%

  # Add Infraclass column --------------------------------------------------
  # Infraclass does not exist in prod_taxa; initialize as NA then assign values
  # via rows_update(). Two passes are needed: SciName-keyed for the synthetic
  # non-FB/SLB rows added upstream, and Order-keyed for shark/ray clades.

    mutate(Infraclass = NA_character_) %>%
    relocate(Infraclass, .after = Order)

  # Phylum exceptions — taxa missing from SeaLifeBase / FishBase --------------
  # Known permanent exceptions not covered by the rule-based Phylum gap-fill above.
  phylum_exceptions <- tribble(
    ~SciName,           ~Phylum,
    "sipunculus nudus", "annelida"
  )

  prod_taxa_expanded <- prod_taxa_expanded %>%
    rows_update(phylum_exceptions, by = "SciName", unmatched = "ignore")

  # SciName-based Infraclass assignments (synthetic rows from 01-clean-input-data.R)
  infraclass_sciname <- tribble(
    ~SciName,   ~Infraclass,
    "batoidea", "batoidea",
    "selachii", "selachii"
  )

  # Order-based Infraclass assignments (WoRMS classification)
  infraclass_orders <- tribble(
    ~Order,                ~Infraclass,
    # Infraclass Selachii — Galeomorphi and Squalomorphi Superorder children
    "carcharhiniformes",   "selachii",
    "heterodontiformes",   "selachii",
    "lamniformes",         "selachii",
    "orectolobiformes",    "selachii",
    "echinorhiniformes",   "selachii",
    "hexanchiformes",      "selachii",
    "pristiophoriformes",  "selachii",
    "squaliformes",        "selachii",
    "squatiniformes",      "selachii",
    # Infraclass Batoidea
    "myliobatiformes",     "batoidea",
    "rajiformes",          "batoidea",
    "rhinopristiformes",   "batoidea",
    "torpediniformes",     "batoidea"
  )

  prod_taxa_expanded <- prod_taxa_expanded %>%
    rows_update(infraclass_sciname, by = "SciName", unmatched = "ignore") %>%
    rows_update(infraclass_orders,  by = "Order",   unmatched = "ignore")

  return(prod_taxa_expanded)
}
