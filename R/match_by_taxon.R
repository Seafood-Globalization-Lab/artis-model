#' @importFrom dplyr filter
#' @export
match_by_taxon <- function(hs_codes_row, possible_prod_taxa, match_code_output, remove_after_match = TRUE, match_type, hs_version = hs_version){
  
  # First, match to species: extract species names in code_descript that match list of SciNames in possible_prod_taxa
  # Note: Matching at the species level is slightly different than matching for genus, family, order, class because the SciName column in 
  # possible_prod_taxa is a mix of different classification levels, not just species
  code_descript <- tolower(hs_codes_row$Description)
  
  # Pattern for matching all taxa names structured like this:
  # fish; pacific salmon, (oncorhynchus nerka/gorbuscha/keta/tschawytscha/kisutch/masou/rhodurus),
  # frozen, (excluding fillets, livers, roes and other fish meat of heading no. 0304)
  possible_taxa_pattern <- possible_prod_taxa %>%
    select(SciName) %>%
    distinct() %>%
    mutate(SciName = paste(SciName, "(/+[a-z]+)+", sep = "")) %>%
    pull(SciName)

  multiple_sciname_matches <- str_extract(code_descript, pattern = (possible_taxa_pattern))
  multiple_sciname_matches <- unique(multiple_sciname_matches[!is.na(multiple_sciname_matches)])
  
  additional_matches <- c()
  
  if (length(multiple_sciname_matches) > 0) {
    
    for (idx in 1:length(multiple_sciname_matches)) {
      curr_match <- multiple_sciname_matches[idx]
      # get everything before the first forward slash
      first_sciname <- str_extract(string = curr_match, pattern = ".+?(?=/)")
      curr_genus = str_extract(string = curr_match, pattern = ".+?(?=\\s)")
      # get everything after the first forward slash (including the slash)
      remainder_scinames <- str_extract(string = curr_match, pattern = "/.+")
      # grab all groups of lowercase letters (NO slashes)
      remainder_scinames <- str_extract_all(string = remainder_scinames, pattern = "[a-z]+")
      # transform to true species names
      remainder_scinames <- unlist(lapply(remainder_scinames, FUN = function(x) paste(curr_genus, x, sep = " ")))
      remainder_scinames <- c(first_sciname, remainder_scinames)
      
      additional_matches <- c(additional_matches, remainder_scinames)
      additional_matches <- additional_matches[additional_matches %in% (possible_prod_taxa %>% pull(SciName))]
    }
    
  }
  
  match_sciname <- str_extract(string = code_descript, pattern = (possible_prod_taxa %>% pull(SciName)))
  
  if (length(additional_matches) > 0) {
    match_sciname <- c(match_sciname, additional_matches)
  }
  
  match_sciname <- unique(match_sciname[is.na(match_sciname)==FALSE])
  match_sciname <- match_sciname[grep(" ", match_sciname)] # This is where FAO genera (no space between words) are removed as potential matches to HS Codes that specify to the species level
  
  if (length(match_sciname) > 0){
    match_code_output <- rbind(match_code_output, data.table(Code = hs_codes_row$Code, 
                                                             SciName = match_sciname, 
                                                             Match_category = match_type, 
                                                             HS_version = hs_version, 
                                                             Description = code_descript, 
                                                             Modification = hs_codes_row$Modification))

    # Remove matched scinames from possible_prod_taxa (Shouldn't match multiple times within a HS code parent, especially now that "special cases" have been removed)
    if (remove_after_match == TRUE){
      possible_prod_taxa <- possible_prod_taxa %>%
        filter(!SciName %in% match_sciname)
    }
  }
  
  # Then match taxa from possible_prod_taxa based on names stored in hs_codes columns: Genus, Family, Order, Class 
  taxa_level<-c("Genus", "Family", "Order", "Class")
  
  for (i in 1:length(taxa_level)){
    if (is.na(hs_codes_row[,taxa_level[i]])==FALSE){
      # Note that each taxa_level column is a list of vectors (each vector is a list of scientific names); index this taxa_level[[1]] to flatten into simple vector
      for (taxa_name in 1:length(hs_codes_row[,taxa_level[i]][[1]])){
        # NOTE: requires carful use of indexing functions - data.table vs base R data.frame produce different values
        # this syntax works for both data.table and base R data.frame
        taxa_to_match <- tolower(hs_codes_row[[ taxa_level[i] ]][[1]][taxa_name])

        # use taxa classification info to grab all entries in the genus
        match_taxa <- possible_prod_taxa %>% 
          filter(!!sym(taxa_level[i])==taxa_to_match) %>%
          pull(SciName) %>%
          unique()

        if (length(match_taxa) > 0){
          match_code_output <- rbind(match_code_output, data.table(Code = hs_codes_row$Code, 
                                                                   SciName = match_taxa, 
                                                                   Match_category = match_type,
                                                                   HS_version = hs_version, 
                                                                   Description = code_descript, 
                                                                   Modification = hs_codes_row$Modification))
          if (remove_after_match == TRUE){
            # Remove matched scinames from list of possible_prod_taxa (Shouldn't match multiple times within a HS code parent, especially now that "special cases" have been removed)
            possible_prod_taxa <- possible_prod_taxa %>%
              filter(!SciName %in% match_taxa)
          }
        }
      }
    }
  }
  
  match_code_output <- unique(match_code_output)

  return(list(match_code_output, possible_prod_taxa))
}