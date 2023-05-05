#' @importFrom dplyr filter
#' @export
match_by_common_name <- function(hs_codes_row, possible_prod_taxa, match_code_output, remove_after_match = TRUE, match_type, hs_version = hs_version){
  
  # Note: crab_not_in_common_name and shrimp_not_in_common_name - modify these to include other production taxa where the word "crab", "shrimp", or "prawn" are not part of the common name
  
  code_descript <- tolower(hs_codes_row$Description)
  
  #### MATCH CRABS BY COMMON NAME
  if (grepl("crab", code_descript)){
    crab_not_in_common_name <- c("chaceon|chaceon maritae|geryon longipes|goneplax rhomboides|xanthidae") # Scientific names of crabs that don't have "crab" as part of their common name
    match_common <- possible_prod_taxa %>% 
      filter(grepl("crab", possible_prod_taxa$CommonName) | grepl(crab_not_in_common_name, possible_prod_taxa$SciName)) %>%
      pull(SciName) %>%
      unique()
    if (length(match_common) > 0){
      match_code_output <- rbind(match_code_output, data.table(Code = hs_codes_row$Code, 
                                                               SciName = match_common, 
                                                               Match_category = match_type,
                                                               HS_version = hs_version, 
                                                               Description = code_descript, 
                                                               Modification = hs_codes_row$Modification))
      # Remove matched scinames from list of possible_prod_taxa (Shouldn't match multiple times within a HS code parent, especially now that "special cases" have been removed)
      possible_prod_taxa <- possible_prod_taxa %>%
        filter(!SciName %in% match_common)
    }
  } # end section if (grepl("crab", code_descript)){
  
  
  #### MATCH SHRIMPS/PRAWNS BY COMMON NAME
  if (grepl("shrimp", code_descript)){
    shrimp_not_in_common_name <- c("pasiphaea tarda|plesionika trispinus|squilla biformis|squilla mantis|xiphopenaeus kroyeri|stomatopoda") # Scientific names of shrimp that don't have "shrimp" or "prawn" as part of their common name
    match_common <- possible_prod_taxa %>% 
      filter(grepl("shrimp|prawn", possible_prod_taxa$CommonName) | grepl(shrimp_not_in_common_name, possible_prod_taxa$SciName)) %>%
      pull(SciName) %>%
      unique()
    if (length(match_common) > 0){
      match_code_output <- rbind(match_code_output, data.table(Code = hs_codes_row$Code, 
                                                               SciName = match_common, 
                                                               Match_category = match_type,
                                                               HS_version = hs_version, 
                                                               Description = code_descript,
                                                               Modification = hs_codes_row$Modification))
      # Remove matched scinames from list of possible_prod_taxa (Shouldn't match multiple times within a HS code parent, especially now that "special cases" have been removed)
      possible_prod_taxa <- possible_prod_taxa %>%
        filter(!SciName %in% match_common)
    }
  } # end section if (grepl("shrimp", code_descript))
  
  return(list(match_code_output, possible_prod_taxa))
  
}
  