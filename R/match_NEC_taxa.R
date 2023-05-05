#' @importFrom dplyr filter
#' @export
match_NEC_taxa <- function(hs_codes_row, possible_hs_codes, parent_output_list, possible_prod_taxa, match_code_output, match_type, hs_version = hs_version){
  non_NEC_codes <- possible_hs_codes %>%
    filter(NEC==0) %>%
    pull(Code)
  
  all_matches_so_far <- data.table::rbindlist(parent_output_list)
  
  if (nrow(all_matches_so_far)>0){
    non_NEC_matches <- all_matches_so_far %>%
      filter(Code %in% non_NEC_codes) %>%
      pull(SciName) %>%
      unique()
  } else {
    non_NEC_matches <- "" # Add this section so code doesn't break if all_matches_so_far is blank
  }

  nec_prod_taxa <- possible_prod_taxa %>%
    filter(!SciName %in% non_NEC_matches) # only keep species that haven't yet been matched
  
  match_code_output <- rbind(match_code_output, data.table(Code = hs_codes_row$Code, 
                                                           SciName = nec_prod_taxa %>% pull(SciName), 
                                                           Match_category = match_type,
                                                           HS_version = hs_version, 
                                                           Description = hs_codes_row$Description,
                                                           Modification = hs_codes_row$Modification))
  match_code_output <- unique(match_code_output)
  
  return(match_code_output)
  
}