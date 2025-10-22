#' Generate ISSCAAP Attribute Table
#'
#' Creates a attribute table mapping species scientific names to ISSCAAP groups,
#' handling multiple ISSCAAP classifications and adding custom mappings for 
#' unknown origin species names commonly used in ARTIS workflows.
#'
#' @param prod_fao Data frame containing FAO production data with SciName and 
#'   isscaap_group columns from FAO or other production sources.
#' @param output_dir Character string. Directory path where the ISSCAAP attribute 
#'   CSV file will be saved. If NULL, no file is written.
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item SciName - Species scientific name 
#'     \item isscaap_group - ISSCAAP group classification or "Multiple ISSCAAP groups"
#'   }
#'
#' @details 
#' This function performs the following operations:
#' \itemize{
#'   \item Creates unique SciName to isscaap_group mappings from production data
#'   \item Identifies species with multiple ISSCAAP classifications and labels them as "Multiple ISSCAAP groups"
#'   \item Adds custom ISSCAAP mappings for higher-order taxa commonly used in ARTIS (arthropoda, chondrichthyes, etc.)
#'   \item Optionally saves the result as "isscaap_attribute.csv" in the specified directory
#' }
#' 
#' The function handles taxonomic names at various levels (species, genus, family, etc.)
#' that may not have clear ISSCAAP classifications in the original production data.
#'
#' @import dplyr
#' @importFrom utils write.csv
#' @export

build_attr_isscaap <- function(prod_fao, output_dir = NULL) {
  
  # Create 1-to-1 matching for SciName to isscaap_group
  isscaap_attribute <- prod_fao %>%
    select(SciName, isscaap_group) %>%
    rename(sciname = SciName) %>%
    distinct()
  
  # Identify species with multiple ISSCAAP groups
  multiple_isscaap <- isscaap_attribute %>% 
    group_by(sciname) %>%
    tally() %>%
    filter(n > 1) %>%
    pull(sciname)
  
  # Handle multiple ISSCAAP classifications
  isscaap_attribute <- isscaap_attribute %>%
    mutate(isscaap_group = case_when(
      sciname %in% multiple_isscaap ~ "Multiple ISSCAAP groups",
      !(sciname %in% multiple_isscaap) ~ isscaap_group
    )) %>%
    distinct()
  
  # Add ISSCAAP groups for custom "unknown origin" scinames
  unknown_isscaap <- data.frame(
    sciname = c("arthropoda", "chondrichthyes", "engraulis", "actinopteri", "homarus",
                "mytilinae", "clupea", "hippoglossinae", "scombrinae", "salmoninae", 
                "animalia", "dissostichus", "cypriniformes", "micromesistius", 
                "echinoida", "chordata"),
    isscaap_group = c("Multiple ISSCAAP groups", "Sharks, rays, chimaeras",
                      "Herrings, sardines, anchovies", "Multiple ISSCAAP groups",
                      "Lobsters, spiny-rock lobsters", "Mussels",
                      "Herrings, sardines, anchovies", "Flounders, halibuts, soles", 
                      "Multiple ISSCAAP groups", "Salmons, trouts, smelts", 
                      "Multiple ISSCAAP groups", "Miscellaneous demersal fishes", 
                      "Carps, barbels and other cyprinids", "Cods, hakes, haddocks",
                      "Sea-urchins and other echinoderms", "Multiple ISSCAAP groups")
  )
  
  # Combine original and custom mappings
  isscaap_attribute <- isscaap_attribute %>%
    bind_rows(unknown_isscaap) %>%
    distinct()
  
  # Optionally save to file
  if (!is.null(output_dir)) {
    write.csv(isscaap_attribute, 
              file.path(output_dir, "isscaap_attribute.csv"), 
              row.names = FALSE)
  }
  
  return(invisible(NULL))
}