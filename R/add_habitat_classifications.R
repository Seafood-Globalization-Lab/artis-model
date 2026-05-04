#' Add habitat classifications to HS-taxa matches
#'
#' Takes the output from match_hs_to_taxa() and adds habitat information,
#' calculates habitat distributions per HS code, and applies habitat compatibility filters.
#'
#' @param hs_taxa_match Data frame. Output from match_hs_to_taxa()
#' @param sciname_habitat Data frame. Contains species habitat information
#' @param prod_data Data frame. Production data with SciName column
#' @param habitat_threshold Numeric. Threshold for habitat classification (default = 0)
#' @param hs_version Character. HS version (e.g., "12")
#'
#' @return Data frame with added habitat classifications and filtered matches
#' @importFrom dplyr left_join group_by ungroup mutate filter select rename tally summarize distinct
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_detect str_length
#' @importFrom cli cli_alert_danger
#' @export
add_habitat_classifications <- function(hs_taxa_match, sciname_habitat, prod_data, habitat_threshold = 0, hs_version) {
  
  # Merge on habitat information onto hs_taxa_match (primary)
  hs_taxa_match <- hs_taxa_match %>%
    left_join(sciname_habitat, by = c("SciName"))
  
  # Filter down to species level taxa matches (NOT SPECIES GROUPS) (secondary)
  tmp <- hs_taxa_match %>%
    distinct() %>%
    filter(str_detect(SciName, " "))
  
  # Calculate percent marine and inland by Code (secondary)
  tmp <- tmp %>%
    group_by(Code, habitat) %>%
    tally() %>%
    rename(habitat_count = n) %>%
    ungroup() %>%
    group_by(Code) %>%
    mutate(total = sum(habitat_count)) %>%
    ungroup() %>%
    mutate(habitat_percent = 100 * habitat_count / total) %>%
    group_by(Code, habitat) %>%
    summarize(habitat_percent = sum(habitat_percent, na.rm = TRUE), .groups = "keep") %>%
    pivot_wider(names_from = habitat, 
                values_from = habitat_percent) %>%
    replace_na(list(marine = 0, inland = 0, diadromous = 0))
  
  # Classify Codes to accept habitats where at least one true species matched with that habitat
  tmp <- tmp %>%
    mutate(habitat_classification = "") %>%
    mutate(habitat_classification = case_when(
      inland > habitat_threshold ~ paste(habitat_classification, "inland", sep = "."),
      TRUE ~ habitat_classification
    )) %>%
    mutate(habitat_classification = case_when(
      marine > habitat_threshold ~ paste(habitat_classification, "marine", sep = "."),
      TRUE ~ habitat_classification
    )) %>%
    mutate(habitat_classification = case_when(
      diadromous > habitat_threshold ~ paste(habitat_classification, "diadromous", sep = "."),
      TRUE ~ habitat_classification
    )) %>%
    mutate(habitat_classification = substr(habitat_classification, 2, str_length(habitat_classification)))
  
  # Merge code-habitat classifications and filter
  hs_taxa_match <- hs_taxa_match %>%
    left_join(tmp %>% select(Code, habitat_classification),
              by = c("Code")) %>%
    mutate(habitat_test = case_when(
      str_detect(habitat_classification, habitat) ~ 1,
      habitat == "diadromous" ~ 1,
      str_detect(SciName, " ") == 0 ~ 1,
      TRUE ~ 0
    )) %>%
    filter(habitat_test == 1) %>%
    select(-c(Fresh01, Brack01, Saltwater01))
  
  # Check habitat assignments
  diadromous_codes <- hs_taxa_match %>%
    filter(habitat_classification == "diadromous") %>%
    filter(habitat != "diadromous")
  
  if (nrow(diadromous_codes) > 0) {
    cli::cli_alert_danger("{hs_version} {.fn add_habitat_classiciations} Non diadromous species going into diadromous only codes")
  }
  
  marine_codes <- hs_taxa_match %>%
    filter(habitat_classification == "marine") %>%
    filter(habitat != "marine" & habitat != "diadromous")
  
  if (nrow(marine_codes) > 0) {
    cli::cli_alert_danger("{hs_version} {.fn add_habitat_classiciations} Non marine or diadromous species going into marine codes")
  }
  
  inland_codes <- hs_taxa_match %>%
    filter(habitat_classification == "inland") %>%
    filter(habitat != "inland" & habitat != "diadromous")
  
  if (nrow(inland_codes) > 0) {
    cli::cli_alert_danger("{hs_version} {.fn add_habitat_classiciations} Non inland or diadromous species going into inland codes")
  }
  
  # Check sciname-habitat combinations
  taxa_habitat_prod <- sciname_habitat %>%
    mutate(taxa_habitat = paste(SciName, habitat, sep = "_")) %>%
    filter(str_detect(SciName, " "))
  
  hs_taxa_habitat_check <- hs_taxa_match %>%
    mutate(taxa_habitat = paste(SciName, habitat, sep = "_")) %>%
    filter(str_detect(SciName, " "))
  
  missing_combos <- unique(taxa_habitat_prod$taxa_habitat)[
    !unique(taxa_habitat_prod$taxa_habitat) %in% unique(hs_taxa_habitat_check$taxa_habitat)
  ]
  
  if (length(missing_combos) > 0) {
    cli::cli_alert_danger(c(
      "!" = "Not all SciName habitat combinations match to an HS code",
      "i" = "in {hs_version} {.fn add_habitat_classiciations}",
      "i" = "Missing combinations: {.val {missing_combos}}"
    ))
  }
  
  # Check higher order taxa habitat classifications
  higher_order_taxa_habitat <- hs_taxa_match %>%
    filter(!str_detect(SciName, " "),
           is.na(habitat_classification))
  
  if (nrow(higher_order_taxa_habitat) > 0) {
    cli::cli_alert_danger("{hs_version} {.fn add_habitat_classiciations} Not all higher order taxa names have a habitat classification")
  }
  
  # Check SciNames in production matched to HS codes
  missing_scinames <- unique(prod_data$SciName)[
    !unique(prod_data$SciName) %in% unique(hs_taxa_match$SciName)
  ]
  
  if (length(missing_scinames) > 0) {
    cli::cli_alert_danger(c(
      "!" = "Not all SciNames matched to HS codes",
      "i" = "in {hs_version} {.fn add_habitat_classiciations}",
      "i" = "Missing species: {.val {missing_scinames}}"
    ))
  }
  
  # Check HS codes have at least 1 SciName
  empty_codes <- hs_taxa_match %>% 
    group_by(Code) %>% 
    tally() %>% 
    filter(n == 0)
  
  if (nrow(empty_codes) > 0) {
    cli::cli_alert_danger("{hs_version} {.fn add_habitat_classiciations} Not every HS code matched to at least one SciName")
  }
  
  # Final cleanup and column renaming
  hs_taxa_match <- hs_taxa_match %>%
    select(-habitat_test) %>%
    rename(sciname_habitat = habitat, 
           code_habitat = habitat_classification)
  
  return(hs_taxa_match)
}