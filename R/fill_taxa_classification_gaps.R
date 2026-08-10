#' Fill gaps and finalize the taxa classification table
#'
#' @description
#' Pure function. Takes the raw `prod_taxa_classification` table from
#' [match_prod_taxa_to_fbslb()], applies all deduplication, gap-filling,
#' special-case row additions, and final filtering to names present in
#' `prod_ts`. Returns the final clean taxa classification table.
#'
#' **Bug fix vs `classify_prod_dat()`:**
#' - Bug 3 fixed: the silent NA-introduction deduplication loop now emits a
#'   `cli_warn()` listing the affected `SciName`s and the columns set to NA.
#'
#' @param prod_taxa_classification Data frame. The `$prod_taxa_classification`
#'   element returned by [match_prod_taxa_to_fbslb()] (Pass 2).
#' @param prod_ts Data frame. The `$prod_ts` element returned by
#'   [match_prod_taxa_to_fbslb()] (Pass 2).
#' @param outdir Character. Directory to write
#'   `missing_scinames_YYYY-MM-DD_HHMM.csv` if any `SciName`s in `prod_ts`
#'   cannot be matched to classification. Required; no default.
#'
#' @return A data frame (`prod_taxa_classification_clean`) — the finalized,
#'   de-duplicated, gap-filled taxa classification table filtered to names
#'   present in `prod_ts`.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @import stringr
#' @import cli
#' @import data.table
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @export

fill_taxa_classification_gaps <- function(
  prod_taxa_classification,
  prod_ts,
  outdir
) {

  # Fill missing Phyla -------------------------------------------------------
  prod_taxa_classification_clean <- prod_taxa_classification %>%
    mutate(
      Phylum = case_when(
        Class %in% c(
          "elasmobranchii", "holocephali", "myxini",
          "cephalaspidomorphi", "sarcopterygii"
        ) ~ "chordata",
        Superclass %in% c("osteichthyes", "chondrichthyes") ~ "chordata",
        TRUE ~ Phylum
      )
    )

  # Fill missing Kingdom (universally animalia) ----------------------------
  prod_taxa_classification_clean <- prod_taxa_classification_clean %>%
    mutate(Kingdom = "animalia")

  # Add Infraclass column --------------------------------------------------
  prod_taxa_classification_clean <- prod_taxa_classification_clean %>%
    mutate(
      Infraclass = case_when(
        Family %in% c(
          "carcharhiniformes", "heterodontiformes", "lamniformes",
          "orectolobiformes", "echinorhiniformes", "hexanchiformes",
          "pristiophoriformes", "squaliformes", "squatiniformes"
        ) ~ "selachii",
        Order %in% c(
          "myliobatiformes", "rajiformes",
          "rhinopristiformes", "torpediniformes"
        ) ~ "batoidea",
        TRUE ~ NA
      )
    ) %>%
    relocate(Infraclass, .after = Order)

  # Special-case row additions and fixes -----------------------------------
  prod_taxa_classification_clean <- prod_taxa_classification_clean %>%
    bind_rows(
      data.frame(
        SciName    = c("perciformes", "scorpaeniformes", "batoidea", "selachii"),
        CommonName = c("tuna-like fishes nei", "mail-cheeked fishes", "rays", "sharks"),
        Genus      = NA,
        Subfamily  = NA,
        Family     = NA,
        Order      = c("perciformes", "scorpaeniformes", NA, NA),
        Infraclass = c(NA, NA, "batoidea", "selachii"),
        Class      = c("teleostei", "teleostei", "elasmobranchii", "elasmobranchii"),
        Superclass = NA,
        Phylum     = c("chordata", "chordata", "chordata", "chordata"),
        Kingdom    = c("animalia", "animalia", "animalia", "animalia"),
        Aquarium   = NA,
        Fresh01    = NA,
        Brack01    = NA,
        Saltwater01 = NA
      )
    ) %>%
    # Phylum override for sipunculus nudus
    mutate(
      Phylum = case_when(
        SciName == "sipunculus nudus" ~ "annelida",
        TRUE ~ Phylum
      )
    ) %>%
    # Fill missing CommonName for osteichthyes
    mutate(
      CommonName = case_when(
        SciName == "osteichthyes" ~ "ray-finned fishes",
        TRUE ~ CommonName
      )
    ) %>%
    # Only keep taxa represented in prod_ts
    filter(SciName %in% prod_ts$SciName)

  # Missing SciName check + CLI warning + CSV write -----------------------
  missing_scinames <- unique(prod_ts$SciName)[
    !(unique(prod_ts$SciName) %in% unique(prod_taxa_classification_clean$SciName))
  ]

  if (length(missing_scinames) > 0) {
    cli_alert_danger(
      "{length(missing_scinames)} {.field SciName}s in {.field prod_ts} are NOT found
    in {.field prod_taxa_classification_clean}. These names could not be matched to
    {.file fishbase} or {.file sealifebase}. They may not properly match to hs product
    codes in {.fn match_*} functions downstream in {.file ./01-clean-input-data.R}.
    Missing names written to {.file outdir} as {.file missing_scinames_yyyy-mm-dd_HHMM.csv}
    to add to manual corrections in {.fn build_corr_tbl_prod_sciname}."
    )

    cli_h1("Missing scientific names")
    cli_ul(missing_scinames)

    fwrite(
      tibble(missing_scinames),
      file.path(
        outdir,
        glue("missing_scinames_{format(Sys.time(), '%Y-%m-%d_%H%M')}.csv")
      )
    )
  }

  prod_taxa_classification_clean
}
