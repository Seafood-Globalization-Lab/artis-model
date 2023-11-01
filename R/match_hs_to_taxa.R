#' @importFrom data.table data.table
#' @import dplyr
#' @import stringr
#' @export
match_hs_to_taxa <- function(hs_data_clean, prod_taxa_classification, fmfo_species_list, hs_version){

  hs_tibble <- tibble(HS_year = c("HS92", "HS96", "HS02", "HS07", "HS12", "HS17"),
                      Classification = c("H0", "H1", "H2", "H3", "H4", "H5"))
  HS_classification <- hs_tibble %>% 
    filter(HS_year==hs_version) %>% 
    pull(Classification)
  
  # Retrieve HS codes for relevant year
  hs_data_version <- hs_data_clean %>% 
    filter(Classification==HS_classification)
  
  # FILTER OUT SPECIAL CASES:
  # Assumption is that HS codes are grouped by broad commodity type at the 4-digit level (only one type of fish, crustaceans, molluscs, or crustaceans for a given 4 digit parent)
  # Filter these codes out of because they: 
  # (1) are matched based on non-taxonomic info (e.g., ornamentals, fish oils, flours meals and pellets)
  # (2) do not follow the matching logic code below which assumes that within a parent group taxa can be assigned to an HS code based on taxonomic info or NEC filtering (e.g., codes are matched based on broad commodity type such as heading 0511)
  # Note: see all_codes_arranged_for_matching_logic.xlsx for documentation on matching logic within parent groups
  parents_to_filter <- hs_data_version %>%
    filter(str_detect(hs_data_version$Code, pattern = "^0511|^1504|^1605|^2301")) %>%     # 0511: Single code that contains all commodity types; 1504: Fish oils; 1605: Multiple commodity types; 2301: Flours, meals and pellets
    pull(Code)
  
  special_cases <- c("030110", "030111", "030119",                                       # 0301: Ornamentals
                       "030270", "030290", "030291", "030292", "030299",                   # 0302: Broad commodity types: Livers, shark fins, and fish fins
                       "030380", "030390", "030391", "030392", "030399",                   # 0303: Broad commodity types: Livers, shark fins, and fish fins
                       "030410", "030420", "030490",                                       # 0304: From earlier years when matching logic was completely different (no taxa info)
                       "030510", "030520", "030530", "030571", "030572", "030579",         # 0305: Broad commodity types: Livers, shark fins, fish heads, fish offal, dried/salted fillets
                       "160420", "160430", "160431", "160432",                             # 1604: Broad commodity types: Minced preparations, caviar and caviar substitutes
                       "160590")                                                           # 1605: Single code matched to all aquatic invertebrates and molluscs (only found in older versions of HS codes)
                      
  codes_to_filter <- c(special_cases, parents_to_filter)
  
  hs_second_match <- hs_data_version %>%
    filter(Code %in% codes_to_filter) # these are matched in second output list
  
  hs_first_match <- hs_data_version %>%
    filter(!Code %in% codes_to_filter) # these are matched in first output list
  
  # Do separate matching for each batch of four digit HS codes
  first_four_digits <- substr(hs_first_match$Code, 1, 4)
  HS_groups <- unique(first_four_digits)
  
  ##############################################################################################
  # FIRST OUTPUT LIST
  # Note: Code is divided into first output, second output, and special output
  first_output_list = list()
  
  for (p in 1:length(HS_groups)){

    # Staging area: Create groups of possible HS codes and possible taxa
    parent_p <- HS_groups[p]
    possible_hs_codes <- hs_first_match %>%
      filter(substr(hs_first_match$Code, 1, 4)==parent_p)
    
    hs_categories <- possible_hs_codes %>%
      select(Fishes, Crustaceans, Molluscs, Aquatic_invertebrates) %>%
      distinct() 
    # ASSUMPTION IS THAT hs_categories WILL ALWAYS BE ONE ROW, i.e., one commodity type. If not, these should have been filtered as part of the special cases
    
    # Create data frame (possible_prod_taxa) of possible taxa matches to hs_categories
    # function get_taxa_group filters prod_taxa_classification based on whether category contains fish, molluscs, crustaceans, and/or aquatic invertebrates
    possible_prod_taxa <- get_taxa_group(hs_code_row = hs_categories, 
                                                    prod_taxa_classification = prod_taxa_classification)
 
    if (nrow(possible_prod_taxa) > 0) { 
      # Add this conditional to allow code to run for subsets of prod_taxa_classification
      # i.e., if trying to run match_hs_to_taxa for just sharks, this will result in possible_prod_taxa == 0 for any of the Mollusc, Crustacean, or Aquatic Invert Codes
      
      # BASIC STRUCTURE OF MATCHING CODE 
      #if (parent_p %in% c("XXXXXX")){ # Next list of HS parents with similar matching logic  
      #  parent_output_list = list()
      #  for (c in 1:length(possible_hs_codes$Code)){
      #    match_code_output <- NULL # reset match_code output
      #    hs_codes_row_c <- possible_hs_codes[c,]
      #    if (hs_codes_row_c$NEC == 0){ # DO ONE TYPE OF MATCHING  
      #      parent_output_list[[c]] <- match_code_output
      #    }
      #    else if (hs_codes_row_c$NEC == 1){ DO ANOTHER TYPE OF MATCHING
      #      parent_output_list[[c]] <- match_code_output
      #    }
      #    parent_output <- data.table::rbindlist(parent_output_list)
      #    first_output_list[[p]] <- parent_output
      #  }
      #} 
      
      # Depending on HS parent (4 digit code), pattern matching logic will be different
      if (parent_p %in% c("0301", "0302", "0303")){ # List of HS parents with similar matching logic
        parent_output_list = list() # Need to reset parent_output_list for each group of codes in parent_p
        for (c in 1:length(possible_hs_codes$Code)){
          # NOTE: matching is done as literally as possible
          # Examples: anguilliformes doesn't match to 030192 because it's not exclusively made of just the one genus)
          # oncorhynchus doesn't match to 030211 or 030213 because we assume that FAO data would be specific enough to match popular food fish in HS descriptions
          # thunnus doesn't match to 030194 or 030195 for the same reason

          match_code_output <- NULL # reset match_code output for each hs_codes_row_c
          hs_codes_row_c <- possible_hs_codes[c,]
          
          
          
          # MATCH TO SPECIES: extract species names in code_descript that match list of SciNames in possible_prod_taxa
          # Use function match_and_remove_by_classification.R to match to genera (next section) or any other taxonomic level besides species
          if (hs_codes_row_c$NEC == 0){ # if the code description calls for simple matching
            
            match_by_classification_output <- match_by_taxon(hs_codes_row = hs_codes_row_c, 
                                                             possible_prod_taxa = possible_prod_taxa, 
                                                             match_code_output = match_code_output, 
                                                             remove_after_match = TRUE, 
                                                             match_type = "explicit_taxa_match",
                                                             hs_version = hs_version)
            match_code_output <- match_by_classification_output[[1]]
            possible_prod_taxa <- match_by_classification_output[[2]]
            
            parent_output_list[[c]] <- match_code_output # before matching to next HS code, add all simple matches to parent_output_listparent_output_list, a list of length "c" - equivalent to number of codes in parent_p
            
          } # end section for all "simple matches": if (hs_codes_row_c$NEC == 0)
          
          if (hs_codes_row_c$NEC == 1) { # DO this group last, any remaining non-matches in possible_prod_taxa belong here because "not elsewhere considered"
            
            # In some cases, multiple NECs are found within a parent group, if so, these should specify some type of classification info in the description for taxa to match to
            match_by_classification_output <- match_by_taxon(hs_codes_row = hs_codes_row_c, 
                                                             possible_prod_taxa = possible_prod_taxa, 
                                                             match_code_output = match_code_output, 
                                                             remove_after_match = TRUE, 
                                                             match_type = "NEC_by_taxa_match",
                                                             hs_version = hs_version)
            match_code_output <- match_by_classification_output[[1]]
            possible_prod_taxa <- match_by_classification_output[[2]]
            
            # Finally, if still no matches in match_code_output, then this must be a "true" NEC code - i.e., catch-all category for each parent code (e.g., 030289, fish NEC in heading 0302)
            # i.e., any remaining non-matches in possible_prod_taxa gets assigned to this code
            if (length(match_code_output)==0) { # use length(match_code_output) as conditional (nrow() produces error if match_code_output is NULL)
              match_code_output <- rbind(match_code_output, data.table(Code = hs_codes_row_c$Code, 
                                                                       SciName = possible_prod_taxa %>% pull(SciName), 
                                                                       Match_category = "NEC_match",
                                                                       HS_version = hs_version, # Note HS_version = hs_version (HS_version is the column name, hs_version is the input parameter passed into the function)
                                                                       Description = hs_codes_row_c$Description,
                                                                       Modification = hs_codes_row_c$Modification))
              match_code_output <- unique(match_code_output)
              # i.e., any remaining non-matches in possible_prod_taxa gets assigned to this code
            }
            
            parent_output_list[[c]] <- match_code_output # before matching to next HS code, add all NEC matches to parent_output_listparent_output_list, a list of length "c" - equivalent to number of codes in parent_p
          } # end section of non-simple matches (i.e., NECs)
        } # GO BACK TO for (c in 1:length(possible_hs_codes$Code)) and begin matching next code c in parent p
        
        # after looping through all of parent p:
        parent_output <- data.table::rbindlist(parent_output_list)
        first_output_list[[p]] <- parent_output
        
      } # end section of all codes within parent p with the same matching logic: if (parent_p %in% c("0301"))
      
      # Next list of HS parents with similar matching logic  
      ################################################################################################################################
      if (parent_p %in% c("0304", "0305", "0306")){ # For these parent groups, possible_prod_taxa needs to be reset multiple times within the parent group
        parent_output_list = list()
        # For these parent codes, if a description has a 4, 5, or 6-digit HS-code heading number, this is an indicator that all unmatched possible_prod_taxa should be matched, and possible_prod_taxa should be reset before moving to next code
        # Four_digit_breaks are the points in the loop when this should happen
        description_digits <- str_extract_all(possible_hs_codes$Description, pattern = "[[:digit:]]") 
        description_digit_lengths <- lapply(description_digits, FUN = length)
        four_digit_breaks <- (which(description_digit_lengths >= 4))
        four_digit_breaks <- c(0, four_digit_breaks) 
        
        for (b in 1:(length(four_digit_breaks)-1)){ # Do matching in batches
          b_start <- four_digit_breaks[b]+1
          b_end <- four_digit_breaks[b+1]
          five_digit_batch <- seq(from = b_start, to = b_end, by = 1)
          possible_prod_taxa_reset <- possible_prod_taxa # RESET POSSIBLE PROD TAXA BEFORE STARTING NEXT BATCH (Everything below should be working with "possible_prod_taxa_reset")
          for (c in five_digit_batch){ # match codes for c = break_start through break_end, then restart possible_prod_taxa_reset
            match_code_output <- NULL # reset match_code output
            hs_codes_row_c <- possible_hs_codes[c,] # get next code
            if (c < b_end){ # as long as we're not matching the last code in the batch, match to SciName, Genus, Family, or Order
              # DO MATCHING BY CLASSIFICATION INFO
              match_by_classification_output <- match_by_taxon(hs_codes_row = hs_codes_row_c, 
                                                               possible_prod_taxa = possible_prod_taxa_reset, 
                                                               match_code_output = match_code_output, 
                                                               remove_after_match = TRUE, 
                                                               match_type = "explicit_taxa_match",
                                                               hs_version = hs_version)
              match_code_output <- match_by_classification_output[[1]]
              possible_prod_taxa_reset <- match_by_classification_output[[2]]
              
              # If still no matches, match by common name
              # Applies to shrimp or crab (i.e., classification metadata doesn't contain enough resolution for shrimp/crab groups so match by common name instead)
              if (length(match_code_output) < 1) { 
                
                match_by_classification_output <- match_by_common_name(hs_codes_row = hs_codes_row_c, 
                                                                       possible_prod_taxa = possible_prod_taxa_reset, 
                                                                       match_code_output = match_code_output, 
                                                                       remove_after_match = TRUE, 
                                                                       match_type = "explicit_taxa_match",
                                                                       hs_version = hs_version)
                match_code_output <- match_by_classification_output[[1]]
                possible_prod_taxa_reset <- match_by_classification_output[[2]]
                
                
              } # end common name matching section  if (length(match_code_output) < 1) 
              
              parent_output_list[[c]] <- match_code_output # before matching to next HS code, add all matches to parent_output_listparent_output_list, a list of length "c" - equivalent to number of codes in parent_p
            } # end section if c < b_end
            
            if (c == b_end){ # if this is the last code in the batch
              
              match_code_output <- rbind(match_code_output, data.table(Code = hs_codes_row_c$Code, 
                                                                       SciName = possible_prod_taxa_reset %>% pull(SciName), 
                                                                       Match_category = "NEC_match",
                                                                       HS_version = hs_version, 
                                                                       Description = hs_codes_row_c$Description, 
                                                                       Modification = hs_codes_row_c$Modification))
              match_code_output <- unique(match_code_output)
              # i.e., any remaining non-matches in possible_prod_taxa gets assigned to this code
              
              parent_output_list[[c]] <- match_code_output
              
            } # End section for last code in the batch (c == b_end)
            
          } # End loop for all codes in a batch (five_digit_batch); batch of codes that uses the same posible_prod_taxa list without reseting
          
          
        } # End loop of all all batches: for b in 1:length(four_digit_breaks)-1 (each time, possible_prod_taxa was reset)
        
        parent_output <- data.table::rbindlist(parent_output_list)
        first_output_list[[p]] <- parent_output
        
      } # end section if (parent_p %in% c("0304"))
      
      # Next list of HS parents with similar matching logic  
      ################################################################################################################################
      # These codes match taxa multiple times within the parent group
      # Don't filter taxa out of possible_prod_taxa
      # For final NEC code, will need to figure out what hasn't been matched yet since we aren't filtering possible_prod_taxa 
      
      if (parent_p %in% c("0307", "0308", "1604")){
        parent_output_list = list()
        for (c in 1:length(possible_hs_codes$Code)){
          match_code_output <- NULL # reset match_code output
          hs_codes_row_c <- possible_hs_codes[c,]
          if (hs_codes_row_c$NEC == 0){ # Do simple matching
            
            match_by_classification_output <- match_by_taxon(hs_codes_row = hs_codes_row_c, 
                                                             possible_prod_taxa = possible_prod_taxa, 
                                                             match_code_output = match_code_output, 
                                                             remove_after_match = FALSE, 
                                                             match_type = "explicit_taxa_match",
                                                             hs_version = hs_version)
            match_code_output <- match_by_classification_output[[1]]
     
            parent_output_list[[c]] <- match_code_output
            
          } # end if (hs_codes_row_c$NEC == 0)
          
          # Since possible_prod_taxa not being filtered, assemble list of past matches from parent_output_list, all non-matches get matched here
          if (hs_codes_row_c$NEC == 1){
            
            match_code_output <- match_NEC_taxa(hs_codes_row = hs_codes_row_c, 
                                                possible_hs_codes = possible_hs_codes, 
                                                parent_output_list = parent_output_list, 
                                                possible_prod_taxa = possible_prod_taxa, 
                                                match_code_output = match_code_output, 
                                                match_type = "NEC_match", 
                                                hs_version = hs_version)
            
            parent_output_list[[c]] <- match_code_output
          }
          
        } # end for (c in 1:length(possible_hs_codes$Code))
        
        parent_output <- data.table::rbindlist(parent_output_list)
        first_output_list[[p]] <- parent_output
      } # end if (parent_p %in% c("0307"))
    } # end loop if nrow(possible_prod_taxa) > 0
    
  } # end loop: for (p in 1:HS_groups) End all first-pass matches
  
  first_output <- data.table::rbindlist(first_output_list)

  ################################################################################################################################
  # SECOND_OUTPUT_LIST:
  # Heading 1605 must be broken up into groups based on commodity type:
  hs_1605_heading <- hs_second_match %>%
    filter(str_detect(Code, pattern = "^1605") & (Code %in% special_cases == FALSE)) %>%
    mutate(group = if_else(str_detect(Code, pattern = "^16051|^16052|^16053|^16054"), true = "hs_1605_crust",
                           if_else(str_detect(Code, pattern = "^16055"), true = "hs_1605_moll",
                                   if_else(str_detect(Code, pattern = "^16056"), true = "hs_1605_invert",
                                           "none")))) %>% # There shouldn't be any "nones" but if_else requires that a value be supplied for when the condition is false
    mutate(group = as.factor(group))
    
  # hs_1605_groups <- c("hs_1605_crust", "hs_1605_moll", "hs_1605_invert") # can't hard code this, needs to be flexible enough for older HS versions
  hs_1605_groups <- levels(hs_1605_heading$group)  
  
  
  second_output_list = list()
    
  for (g in 1:length(hs_1605_groups)){
    possible_hs_codes <- hs_1605_heading %>%
      filter(group == hs_1605_groups[g])
    
    hs_categories <- possible_hs_codes %>%
      select(Fishes, Crustaceans, Molluscs, Aquatic_invertebrates) %>%
      distinct() 
    # ASSUMPTION IS THAT hs_categories WILL ALWAYS BE ONE ROW, i.e., one commodity type. If not, these should have been filtered as part of the special cases
    
    # Create data frame (possible_prod_taxa) of possible taxa matches to hs_categories
    # function get_taxa_group filters prod_taxa_classification based on whether category contains fish, molluscs, crustaceans, and/or aquatic invertebrates
    possible_prod_taxa <- get_taxa_group(hs_code_row = hs_categories, 
                                         prod_taxa_classification = prod_taxa_classification)
    
    group_output_list = list()
    
    if (nrow(possible_prod_taxa) > 0) {
      
      for (c in 1:length(possible_hs_codes$Code)){
        
        
        match_code_output <- NULL # reset match_code output for each hs_codes_row_c
        hs_codes_row_c <- possible_hs_codes[c,]
        
        # MATCH TO SPECIES: extract species names in code_descript that match list of SciNames in possible_prod_taxa
        # Use function match_and_remove_by_classification.R to match to genera (next section) or any other taxonomic level besides species
        if (hs_codes_row_c$NEC == 0){ # if the code description calls for simple matching
          
          match_by_classification_output <- match_by_taxon(hs_codes_row = hs_codes_row_c, 
                                                           possible_prod_taxa = possible_prod_taxa, 
                                                           match_code_output = match_code_output, 
                                                           remove_after_match = FALSE, 
                                                           match_type = "explicit_taxa_match",
                                                           hs_version = hs_version)
          match_code_output <- match_by_classification_output[[1]]
          # possible_prod_taxa <- match_by_classification_output[[2]]
          # No filtering is done because shrimps and prawns are matched twice 160521 and 160529 in H4 Classification year
          
          # If no matches by scientific name, match by common name (applies to crabs and shrimps/prawns):
          if (length(match_code_output) < 1) { 
            
            match_by_classification_output <- match_by_common_name(hs_codes_row = hs_codes_row_c, 
                                                                   possible_prod_taxa = possible_prod_taxa, # use original possible_prod_taxa, not possible_prod_taxa_reset
                                                                   match_code_output = match_code_output, 
                                                                   remove_after_match = FALSE, 
                                                                   match_type = "explicit_taxa_match",
                                                                   hs_version = hs_version)
            match_code_output <- match_by_classification_output[[1]]
            # possible_prod_taxa_reset <- match_by_classification_output[[2]]
            
            
          } # end common name matching section  if (length(match_code_output) < 1) 
          
          group_output_list[[c]] <- match_code_output
          
        } # end section for all "simple matches": if (hs_codes_row_c$NEC == 0)
        
        # Since possible_prod_taxa not being filtered, assemble list of past matches, all non-matches get matched here
        if (hs_codes_row_c$NEC == 1){
          
          match_code_output <- match_NEC_taxa(hs_codes_row = hs_codes_row_c, 
                                              possible_hs_codes = possible_hs_codes, 
                                              parent_output_list = group_output_list, 
                                              possible_prod_taxa = possible_prod_taxa, 
                                              match_code_output = match_code_output, 
                                              match_type = "NEC_match", 
                                              hs_version = hs_version)
          
          group_output_list[[c]] <- match_code_output
        }
        
      }  # end section: for (c in 1:length(possible_hs_codes$Code))
      
      group_output <- data.table::rbindlist(group_output_list)
      second_output_list[[g]] <- group_output
      
    } # end section if nrow(possible_prod_taxa) > 0
    
  } # end section for (g in 1:length(hs_1605_groups))

  second_output <- data.table::rbindlist(second_output_list)
  
  ################################################################################################################################
  # NOW MATCH SPECIAL CASES
  
  # Break up remaining cases into ornamentals, FMFO's (i.e. fish meals, oils), and everything else (i.e., match by commodity type)
  hs_special_cases <- hs_second_match %>%
    filter(Code %in% hs_1605_heading$Code == FALSE) %>%
    mutate(Special_Treatment = if_else(str_detect(string = Description, pattern = "ornamental")==TRUE, 
                                       "ornamental",
                                       if_else(str_detect(string = Description, pattern = "meal|oil")==TRUE, 
                                               "fmfo",
                                               "commodity type")))
  
  special_output_list = list()
  
  for (c in 1:length(hs_special_cases$Code)){
    match_special_output <- NULL
    special_codes_row_c <- hs_special_cases[c,]
    special_descript <- tolower(hs_special_cases$Description[c])
    possible_prod_taxa <- get_taxa_group(hs_code_row = special_codes_row_c, 
                              prod_taxa_classification = prod_taxa_classification)
  
    if (nrow(possible_prod_taxa) > 0) {
      
      if (special_codes_row_c$Special_Treatment=="commodity type"){
        
        # Before doing any matching, remove any "other thans" from possible_prod_taxa
        if (str_detect(string = special_codes_row_c$Description, pattern = "(other than.*)|(excluding.*)")){ # Is there an "other than" phrase?
          other_than_phrase <- str_extract_all(string = special_codes_row_c$Description, pattern = "(other than.*)|(excluding.*)") # What is the phrase that comes after "other than"
          other_than_phrase <- tolower(other_than_phrase)
          
          # Check if any species are listed as "other thans"
          match_sciname <- str_extract(string = other_than_phrase, pattern = (possible_prod_taxa %>% pull(SciName)))
          match_sciname <- unique(match_sciname[is.na(match_sciname)==FALSE])
          match_sciname <- match_sciname[grep(" ", match_sciname)] # This is where FAO genera (no space between words) are removed as potential matches to HS Codes that specify to the species level
          
          # If there were taxa that matched  the description of "other thans":
          if (length(match_sciname) > 0){
            # Remove species names from possible_prod_taxa
            possible_prod_taxa <- possible_prod_taxa %>%
              filter(SciName %in% match_sciname==FALSE)
          }
          
          taxa_level<-c("Genus", "Family", "Order", "Class") 
          for (i in 1:length(taxa_level)){ # Cycle through each column of classification info in special_codes_row_c 
            if (is.na(special_codes_row_c[,taxa_level[i]])==FALSE){ # Is there classification information in the Description?
              for (taxa_name in 1:length(special_codes_row_c[,taxa_level[i]][[1]])){ # Cycle through each taxa_name, Note that each taxa_level column is a list of vectors (each vector is a list of genera); index this as Genus[[1]] to flatten into simple vector
                taxa_to_match <- tolower(special_codes_row_c[,taxa_level[i]][[1]][taxa_name])
                if (str_detect(string = other_than_phrase, pattern = taxa_to_match)){  # Does the classification information in the Description match the phrase that come after "other than"
                  # use taxa classification info to remove all entries in possible_prod_taxa
                  possible_prod_taxa <- possible_prod_taxa %>% 
                    filter((!!sym(taxa_level[i])==taxa_to_match)==FALSE)
                }
              } 
            }
          } 
        } # end section: Is there an "other than" phrase?
        
        # After checking for "Other thans" and filtering them out, now match
        match_by_classification_output <- match_by_taxon(hs_codes_row = special_codes_row_c, 
                                                         possible_prod_taxa = possible_prod_taxa, 
                                                         match_code_output = match_special_output, 
                                                         remove_after_match = FALSE, 
                                                         match_type = "explicit_taxa_match",
                                                         hs_version = hs_version)
        match_special_output <- match_by_classification_output[[1]]
        # possible_prod_taxa <- match_by_classification_output[[2]] # this line is not necessary for this parent group because possible_prod_taxa is not being filtered
        
        if (length(match_special_output)<1) { # If there are no matches (i.e., no classification info), then this is a broad commodity type category; match all remaining taxa in possible_prod_taxa
          match_special_output <- rbind(match_special_output, data.table(Code = special_codes_row_c$Code, 
                                                                         SciName = possible_prod_taxa %>% pull(SciName), 
                                                                         Match_category = "broad_commodity_match",
                                                                         HS_version = hs_version, 
                                                                         Description = special_codes_row_c$Description, 
                                                                         Modification = special_codes_row_c$Modification))
          match_special_output <- unique(match_special_output)
        }
        
        
        special_output_list[[c]] <- match_special_output
        
        
      } # end section: if (special_codes_row_c$Special_Treatment=="commodity type")
      
      if (special_codes_row_c$Special_Treatment=="ornamental"){
        # MATCH ORNAMENTALS  
        match_special_output <- NULL
        special_codes_row_c <- hs_special_cases[c,]
        special_descript <- tolower(hs_special_cases$Description[c])
        
        
        # Note: 621 out of 2181 fish species have missing info (NA) for the "Aquarium" column; 
        # NAs also for Fresh01, Brack01, and Saltwater01 (some of these are genera or anything higher than species-level, and therefore not enough resolution to be considered)
        if (grepl(pattern = "freshwater", x = special_descript)==TRUE & grepl(pattern = "other than", x = special_descript)==FALSE){
          possible_prod_taxa <- possible_prod_taxa %>%
            filter(Fresh01 == 1 & (Aquarium == "commercial" | Aquarium == "highly commercial" | Aquarium == "public aquariums")) # other values: "never/rarely" and "potential"
          match_special_output <- rbind(match_special_output, data.table(Code = special_codes_row_c$Code, 
                                                                         SciName = possible_prod_taxa %>% pull(SciName), 
                                                                         Match_category = "aquarium_trade_match",
                                                                         HS_version = hs_version, 
                                                                         Description = special_codes_row_c$Description, 
                                                                         Modification = special_codes_row_c$Modification))
          match_special_output <- unique(match_special_output)
        } else if (grepl(pattern = "other than freshwater", x = special_descript)==TRUE){
          possible_prod_taxa <- possible_prod_taxa %>%
            filter(Fresh01 == 0 & (Brack01 == 1 | Saltwater01 == 1) & (Aquarium == "commercial" | Aquarium == "highly commercial" | Aquarium == "public aquariums")) # some species like multiple habitat types; explicitly remove any species that like freshwater, keep those that like brackish or salt
          match_special_output <- rbind(match_special_output, data.table(Code = special_codes_row_c$Code, 
                                                                         SciName = possible_prod_taxa %>% pull(SciName), 
                                                                         Match_category = "aquarium_trade_match",
                                                                         HS_version = hs_version, 
                                                                         Description = special_codes_row_c$Description, 
                                                                         Modification = special_codes_row_c$Modification))
          match_special_output <- unique(match_special_output)
        } else { # For Code 030110 there is no differentiation between freshwater or other than freshwater
          possible_prod_taxa <- possible_prod_taxa %>%
            filter(Aquarium == "commercial" | Aquarium == "highly commercial" | Aquarium == "public aquariums") # ignore habitat type, match all aquarium species
          match_special_output <- rbind(match_special_output, data.table(Code = special_codes_row_c$Code, 
                                                                         SciName = possible_prod_taxa %>% pull(SciName), 
                                                                         Match_category = "aquarium_trade_match",
                                                                         HS_version = hs_version, 
                                                                         Description = special_codes_row_c$Description, 
                                                                         Modification = special_codes_row_c$Modification))
          match_special_output <- unique(match_special_output)
        }
        
        special_output_list[[c]] <- match_special_output
      } # end if (special_codes_row_c$Special_Treatment=="ornamental")
      
      if (special_codes_row_c$Special_Treatment=="fmfo"){
        # MATCH FISH MEALS and FISH OILS (FMFO Species List)
        possible_prod_taxa <- possible_prod_taxa %>%
          filter(possible_prod_taxa$SciName %in% tolower(fmfo_species_list$scientific_name)) 
        match_special_output <- rbind(match_special_output, data.table(Code = special_codes_row_c$Code, 
                                                                       SciName = possible_prod_taxa %>% pull(SciName), 
                                                                       Match_category = "fmfo_match",
                                                                       HS_version = hs_version, 
                                                                       Description = special_codes_row_c$Description, 
                                                                       Modification = special_codes_row_c$Modification))
        match_special_output <- unique(match_special_output)
        special_output_list[[c]] <- match_special_output
      } # end (special_codes_row_c$Special_Treatment=="fmfo")
      
    } # if (nrow(possible_prod_taxa) > 0)
  } # end section: for (c in 1:length(hs_special_cases$Code))
  
  special_output <- data.table::rbindlist(special_output_list)
  
  
  #############################################################################################
  # COMBINE ALL OUTPUTS INTO FINAL_OUTPUT
  final_output <- data.table::rbindlist(list(first_output, second_output, special_output)) %>%
    arrange(Code, SciName, Match_category)
  
  #############################################################################################
  # ADDENDUM:
  # In most cases, we interpret the HS code description literally
  # Example: HS codes define Jellyfish as Rhopilema spp. even though scientifically it should be all organisms in class = scyphozoa
  # So for Jellyfish HS codes we only allow Rhopilema to match but then all other non-Rhopilema Scyphozoids go in the NEC category for aquatic invertebrates
  # In some cases, production data, however are reported as overly-broad taxa (e.g., osteichthyes); interpreting HS code descriptions literally means this taxa would only match to the NEC categories
  # We've decided to match any production taxa identified at the level of "Order" and above that were previously only matching to NEC categories to match to additional codes as defined below:
  # Rather than incorporating each of these cases individually to the original matching code below, we've included it instead as an "addendum" to the code below to be explicit about these exceptions
  # List of addendum taxa:
  # bivalvia
  # cephalopoda
  # clupeiformes
  # echinodermata
  # echinoidea
  # elasmobranchii
  # gadiformes
  # osteichthyes
  # perciformes
  # rajiformes
  # siluriformes
  
  # Note - The following Order-and-above production taxa are not dealt with below because they are either 
  # 1 - already part of the HS code descriptions (e.g., Teuthida) or 
  # 2 - have nowhere else to match besides an NEC code and therefore are correctly handled by the matching code above:
  # Asiciacea
  # Asteroidea
  # Branchiopoda
  # Carcharhiniformes
  # Chimaeriformes
  # Decapoda
  # holothuroidea
  # Mollusca
  # pleuronectiformes
  # polychaeta
  # Stomatopoda
  # Teuthida
  
  # BIVALVIA: allow to match to clam, oyster, mussel, scallop
  bivalvia_new <- final_output %>%
    filter(str_detect(Description, "clam|oyster|mussel|scallop")) %>%
    select(Code, Description, Modification) %>%
    unique() %>%
    mutate(SciName = "bivalvia",
           Match_category = "broad_taxa_match",
           HS_version = hs_version)
  
  # CEPHALOPODA: allow to match to squid, octopus, cuttle fish
  cephalopoda_new <- final_output %>%
    filter(str_detect(Description, "cuttle|squid|octopus")) %>%
    select(Code, Description, Modification) %>%
    unique() %>%
    mutate(SciName = "cephalopoda",
           Match_category = "broad_taxa_match",
           HS_version = hs_version)
  
  # CLUPEIFORMES: allow to match to codes that contain herring, anchovy, or sardines in the description; retain original matches since there are also clupeiformes that are non-herring, non-anchovy, non-sardines
  clupeiformes_new <- final_output %>%
    filter(str_detect(Description, "herring|anchov|sardine")) %>%
    select(Code, Description, Modification) %>%
    unique() %>%
    mutate(SciName = "clupeiformes",
           Match_category = "broad_taxa_match",
           HS_version = hs_version)
  
  # ECHINODERMATA: allow to match to sea urchin and sea cucumber
  echinodermata_new <- final_output %>%
    filter(str_detect(Description, "urchin|cucumber")) %>%
    select(Code, Description, Modification) %>%
    unique() %>%
    mutate(SciName = "echinodermata",
           Match_category = "broad_taxa_match",
           HS_version = hs_version)
  
  # ECHINOIDEA: allow to match to sea urchin
  echinoidea_new <- final_output %>%
    filter(str_detect(Description, "urchin")) %>%
    select(Code, Description, Modification) %>%
    unique() %>%
    mutate(SciName = "echinoidea",
           Match_category = "broad_taxa_match",
           HS_version = hs_version)
  
  # ELASMOBRANCHII: allow to match to sharks and rays/skates
  elasmobranchii_new <- final_output %>%
    filter(str_detect(Description, "sharks|rays")) %>%
    select(Code, Description, Modification) %>%
    unique() %>%
    mutate(SciName = "elasmobranchii",
           Match_category = "broad_taxa_match",
           HS_version = hs_version)
  
  # GADIFORMES: allow to match to "gadus" (includes all codes describing cod or pollock), "hake", "coalfish", "whiting", or "bregmacerotidae"
  # gadiformes includes multiple families not mentioned above but all of these families are listed in the same HS code for "bregmacerotidae"
  gadiformes_new <- final_output %>%
    filter(str_detect(Description, "gadus|hake|coalfish|whiting|bregmacerotidae")) %>%
    select(Code, Description, Modification) %>%
    unique() %>%
    mutate(SciName = "gadiformes",
           Match_category = "broad_taxa_match",
           HS_version = hs_version)
  
  # RAJIFORMES: allow to match to rays and skates
  rajiformes_new <- final_output %>%
    filter(str_detect(Description, "rays and skates")) %>%
    select(Code, Description, Modification) %>%
    unique() %>%
    mutate(SciName = "rajiformes",
           Match_category = "broad_taxa_match",
           HS_version = hs_version)
  
  # SILURIFORMES: allow to match to catfish
  siluriformes_new <- final_output %>%
    filter(str_detect(Description, "catfish")) %>%
    select(Code, Description, Modification) %>%
    unique() %>%
    mutate(SciName = "siluriformes",
           Match_category = "broad_taxa_match",
           HS_version = hs_version)
  
  # Allow OSTEICHTHYES and PERCIFORMES to match to all codes:
  # 0301: live fish
  # 0302: fresh or chilled fish EXCEPT 030281 (sharks), 030282 (rays)
  # 0303: frozen fish EXCEPT 030381 (sharks), 030382 (rays)
  # 0304: fillets and meats
  # 0305: salted, smoked, dried EXCEPT 030571 (shark fins)
  # 051191: animal products unfit for human consumption
  # 150410 and 150420: oils
  # 1604: fish preparations
  # 230120: Flours, meals, and pellets
  # 0306, 0307, 0308, 1605: crustaceans, molluscs, aquatic inverts
  
  # Add broad taxa matches
  broad_matches <- final_output %>%
    left_join(prod_taxa_classification, by = "SciName") %>% 
    select(Code, HS_version, Description, Modification, Genus:Kingdom) %>% 
    pivot_longer(cols = Genus:Kingdom, names_to = "Taxa_level", values_to = "SciName") %>%
    select(-Taxa_level) %>% 
    filter(SciName %in% unique(prod_taxa_classification$SciName)) %>%
    distinct() %>%
    mutate(Match_category = "broad_taxa_match")
  
  broad_matches <- broad_matches %>% 
    anti_join(final_output, by = c("Code", "SciName"))
  
  # Now bind all the new matches to the original final_output and unique()
  final_output <- final_output %>%
    bind_rows(bivalvia_new) %>%
    bind_rows(cephalopoda_new) %>%
    bind_rows(clupeiformes_new) %>%
    bind_rows(echinodermata_new) %>%
    bind_rows(echinoidea_new) %>%
    bind_rows(elasmobranchii_new) %>%
    bind_rows(gadiformes_new) %>%
    bind_rows(rajiformes_new) %>%
    bind_rows(siluriformes_new) %>%
    bind_rows(broad_matches) %>%
    # Change all the original matches for these groups to "broad_taxa_match" to allow unique() to filter out the double matches
    mutate(Match_category = if_else(condition = SciName %in% c("bivalvia", "cephalopoda", "clupeiformes", "echinodermata", "echinoidea", "elasmobranchii",
                                                               "gadiformes", "osteichthyes", "perciformes", "rajiformes", "siluriformes"),
                                    true = "broad_taxa_match",
                                    false = Match_category)) %>% 
    unique() %>% # to remove duplicates for when these taxa matched via the "normal" match_hs_to_taxa code above
    arrange(Code)
  
  # Ensure no vertebrates are matched to invertebrate descriptions
  vertebrates <- prod_taxa_classification %>% 
    filter(Phylum == "chordata" & Class != "ascidiacea") %>%
    pull(SciName) %>%
    unique()
  
  invertebrate_codes <- final_output %>% 
    filter(str_detect(Description, "invertebrate")) %>% 
    pull(Code)%>% 
    unique()
  invertebrate_codes <- invertebrate_codes[!(invertebrate_codes %in% c("051191", "230120"))]
  
  final_output <- final_output %>%
    filter(!((Code %in% invertebrate_codes) & (SciName %in% vertebrates)))
  
  #############################################################################################
  # DATA CHECK: all taxa in prod_taxa_classification should go into at least one commodity; and all commodities should have at least one taxa match
  prod_to_fix <- unique(prod_taxa_classification$SciName)[!unique(prod_taxa_classification$SciName) %in% final_output$SciName]
  
  if (length(prod_to_fix) != 0){
    prod_to_fix <- paste(prod_to_fix, sep = "", collapse = ", ")
    taxa_warning_message <- paste("The following production taxa do not match any HS codes: ", prod_to_fix, sep = "")
    warning(taxa_warning_message, call. = FALSE)
  }
  
  hs_codes_to_fix <- unique(hs_data_version$Code)[!unique(hs_data_version$Code) %in% unique(final_output$Code)]
  
  if (length(hs_codes_to_fix) != 0){
    hs_codes_to_fix <- paste(hs_codes_to_fix, sep = "", collapse = ", ")
    hs_warning_message <- paste("The following HS codes were not matched to any production taxa: ", hs_codes_to_fix, sep = "")
    warning(hs_warning_message, call. = FALSE)
  }
  
  return(final_output)
  
}
