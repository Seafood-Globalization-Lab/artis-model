#' Fill gaps and finalize the taxa classification table
#'
#' @description
#' Takes the raw `prod_taxa_classification` table from
#' [match_prod_taxa_to_fbslb()], applies all deduplication, gap-filling,
#' special-case row additions, and final filtering to names present in
#' `prod_data`. Returns the final clean taxa classification table.
#'
#'
#' @param the_prod_taxa_classification Data frame. The `$prod_taxa_classification`
#'   element returned by [match_prod_taxa_to_fbslb()] (Pass 2).
#' @param the_prod_data Data frame. The `$prod_data` element returned by
#'   [match_prod_taxa_to_fbslb()] (Pass 2).
#' @param outdir Character. Directory to write
#'   `missing_scinames_YYYY-MM-DD_HHMM.csv` if any `SciName`s in `prod_data`
#'   cannot be matched to classification. Required; no default.
#'
#' @return A data frame (`prod_taxa_classification_clean`) — the finalized,
#'   de-duplicated, gap-filled taxa classification table filtered to names
#'   present in `prod_data`.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @import stringr
#' @import cli
#' @import data.table
#' @importFrom glue glue
#' @importFrom tibble tibble tribble
#' @export

fill_taxa_classification_gaps <- function(
  the_prod_taxa_classification,
  the_prod_data,
  outdir
) {

  # Replace Missing Values ----------------------------

    # "not assigned" - shows up in Class column, but check others. Think about other non-taxa values that might need replacing as well. 
  
  # Fill missing Kingdom (universally animalia) ----------------------------
  prod_taxa_classification_clean <- the_prod_taxa_classification %>%
    mutate(Kingdom = "animalia")

  # Fill missing Phyla -------------------------------------------------------

  prod_taxa_classification_clean <- prod_taxa_classification_clean %>%
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
    ))

  # Add Infraclass column --------------------------------------------------

  prod_taxa_classification_clean <- prod_taxa_classification_clean %>%
    mutate(
      Infraclass = case_when(
        # create Infraclass Selachii - two children Superorder (taxa rank not included in FB/SLB/ARTIS)
        Order %in%
          c(
            # Galeomorphi Superorder direct children Orders (WoRMS)
            "carcharhiniformes",
            "heterodontiformes",
            "lamniformes",
            "orectolobiformes",
            # Squalomorphi Superorder direct children Orders (WoRMS)
            "echinorhiniformes",
            "hexanchiformes",
            "pristiophoriformes",
            "squaliformes",
            "squatiniformes"
          ) ~ "selachii",
        # create Infraclass Batoidea
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


  # Add information for missing taxa in `taxa_need_corrections_2` ----------
  
  # FIXIT: AM 2026-0-8-31 - Do we want to retain these manual additions? these are accounted for in the gap filling above, do we need to specifically make these taxa values Scinames?
  # Probably right? taxa_need_corrections_2 has "batoidea", "perciformes", and "selachii" as the remaining values from matching process upstream. That means they are in prod but not
  # in Fishbase/sealifebase? 

  prod_taxa_classification_clean <- prod_taxa_classification_clean %>%
    bind_rows(
      tribble(
        ~SciName,          ~CommonName,            ~Genus, ~Subfamily, ~Family, ~Order,           ~Infraclass,    ~Class,          ~Superclass,   ~Phylum,    ~Kingdom,   ~Aquarium, ~Fresh01, ~Brack01, ~Saltwater01,
        "perciformes",     "tuna-like fishes nei", NA,     NA,         NA,      "perciformes",    NA,             "teleostei",     "osteichthyes", "chordata", "animalia", NA,        NA,       NA,       NA,
        "batoidea",        "rays",                 NA,     NA,         NA,      NA,               "batoidea",     "elasmobranchii", "chondrichthyes", "chordata", "animalia", NA,        NA,       NA,       NA,
        "selachii",        "sharks",               NA,     NA,         NA,      NA,               "selachii",     "elasmobranchii", "chondrichthyes", "chordata", "animalia", NA,        NA,       NA,       NA,
      ))

  # Special-case row additions and fixes -----------------------------------
  
  prod_taxa_classification_clean <- prod_taxa_classification_clean %>% 
    # Fill in taxa rank values for sipunculus nudus - missing from Sealfiebase
    # Want to keep "not assigned" value in Class?
    mutate(
      Phylum = case_when(
        SciName == "sipunculus nudus" ~ "annelida",
        TRUE ~ Phylum
      )
    ) %>% 
  
    # Fill missing CommonName for osteichthyes -----------------------------------
  
    # FIXIT: AM 2026-08-31 - multiple common names originating from production data. Do we want to overwrite this distinction? 
    # mutate(
    #   CommonName = case_when(
    #     SciName == "osteichthyes" ~ "ray-finned fishes",
    #     TRUE ~ CommonName
    #   )
    # ) %>%
  
    # Only keep taxa represented in prod_data -----------------------------------

    filter(SciName %in% the_prod_data$SciName)

  # Missing SciName check + CLI warning + CSV write -----------------------
  missing_scinames <- unique(the_prod_data$SciName)[
    !(unique(the_prod_data$SciName) %in% unique(prod_taxa_classification_clean$SciName))
  ]

  if (length(missing_scinames) > 0) {
    cli_alert_danger(
      "{length(missing_scinames)} {.field SciName}s in {.field the_prod_data} are NOT found
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
