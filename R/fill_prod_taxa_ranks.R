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
#' @param the_prod_taxa_classification Data frame. The production taxa
#'   classification table after [correct_taxa_habitat()] and the inline manual
#'   taxonomy corrections in `01-clean-input-data.R`. Typically
#'   `prod_taxa_classification`.
#'
#' @return
#' A data frame with the same rows as `the_prod_taxa_classification` with
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

fill_prod_taxa_ranks <- function(the_prod_taxa_classification) {

  # Fill Kingdom (universally animalia) ------------------------------------

  prod_taxa_classification_clean <- the_prod_taxa_classification %>%
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

    mutate(
      Infraclass = case_when(
        # Direct assignment for synthetic non-FB/SLB rows added in 01-clean-input-data.R
        SciName == "batoidea" ~ "batoidea",
        SciName == "selachii" ~ "selachii",
        # Infraclass Selachii — Galeomorphi and Squalomorphi Superorder children (WoRMS)
        Order %in%
          c(
            "carcharhiniformes",
            "heterodontiformes",
            "lamniformes",
            "orectolobiformes",
            "echinorhiniformes",
            "hexanchiformes",
            "pristiophoriformes",
            "squaliformes",
            "squatiniformes"
          ) ~ "selachii",
        # Infraclass Batoidea
        Order %in%
          c(
            "myliobatiformes",
            "rajiformes",
            "rhinopristiformes",
            "torpediniformes"
          ) ~ "batoidea",
        TRUE ~ NA
      )
    ) %>%
    relocate(Infraclass, .after = Order)

  return(prod_taxa_classification_clean)
}
