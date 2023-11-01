#' @import dplyr
#' @export
match_hs_to_clade <- function(hs_taxa_match, prod_taxa_classification, match_to_prod = TRUE){
  # Match each HS code to the most specific taxa grouping that encompasses all taxa in that code
  # i.e., if every taxa in the code has the same species, match to species; if there are different species but all match the same genus, match to genus; 
  # if there are different genera but all match the same family, match to family, etc
  hs_taxa_classification <- hs_taxa_match %>%
    left_join(prod_taxa_classification %>% select(-c(CommonName, Aquarium, Fresh01, Brack01, Saltwater01)) %>% unique(), by = "SciName") %>% # Remove multiple rows of identical SciNames that have different Common Names
    # If a clade is not reported in the prod_data (i.e., is not in unique(SciName)), then change to NA - ultimately can't assign this value to hs_clade_match output
    {if (match_to_prod == TRUE) 
      mutate(., Genus = if_else(Genus %in% unique(hs_taxa_match$SciName), true = Genus, false = as.character(NA)),
             Subfamily = if_else(Subfamily %in% unique(hs_taxa_match$SciName), true = Subfamily, false = as.character(NA)),
             Family = if_else(Family %in% unique(hs_taxa_match$SciName), true = Family, false = as.character(NA)),
             Order = if_else(Order %in% unique(hs_taxa_match$SciName), true = Order, false = as.character(NA)),
             Class = if_else(Class %in% unique(hs_taxa_match$SciName), true = Class, false = as.character(NA)),
             Superclass = if_else(Superclass %in% unique(hs_taxa_match$SciName), true = Superclass, false = as.character(NA)),
             Phylum = if_else(Phylum %in% unique(hs_taxa_match$SciName), true = Phylum, false = as.character(NA)),
             Kingdom = if_else(Kingdom %in% unique(hs_taxa_match$SciName), true = Kingdom, false = as.character(NA)))
    else .}
  
  # Remove all Match_category == "broad_taxa_match", i.e., taxa that were allowed to match broadly (e.g., bivalvia, cephalopoda, osteichthyes, etc); 
  # see hs_taxa_classification %>% filter(Match_category == "broad_taxa_match") %>% select(SciName) %>% distinct() %>% arrange(SciName)
  
  # Column names of Classification levels
  all_classification_levels <- c("Genus", "Subfamily", "Family", "Order", "Class", "Superclass", "Phylum", "Kingdom")
  
  hs_clade_match <- NULL
  for (i in 1:length(unique(hs_taxa_classification$Code))){
    code_i <- unique(hs_taxa_classification$Code)[i]
    hs_i <- hs_taxa_classification %>%
      filter(Code == code_i) %>% 
      filter(Match_category != "broad_taxa_match") %>%
      select(contains(c("SciName", all_classification_levels))) %>%
      mutate(across(everything(), as.factor)) 
    
    hs_clade_test <- hs_i %>%
      mutate(across(everything(), as.numeric)) %>%
      colSums() # Keep the test strict and don't remove NA (this helps avoid a scenario where a group of 4 taxa where Genus levels are 1, 1, 2, and NA which sums to 4 - i.e., false positive that they are the same genus)
    
    # Test for which classification levels are matching throughout all taxa (i.e., colSums == nrow)
    hs_all_clades <- hs_clade_test[hs_clade_test==nrow(hs_i) & is.na(hs_clade_test)==FALSE] # include is.na==FALSE test to remove where colSums returns an NA
    
    if (nrow(hs_i)==1){
      hs_level <- "SciName" # SciName is the name of the column, but if there's only one row this gets mutated to species in the final dataframe
    } else if ("Genus" %in% names(hs_all_clades)) {
      hs_level <- "Genus"
    } else if ("Subfamily" %in% names(hs_all_clades)) {
      hs_level <- "Subfamily"
    } else if ("Family" %in% names(hs_all_clades)) {
      hs_level <- "Family"
    } else if ("Order" %in% names(hs_all_clades)) {
      hs_level <- "Order"
    } else if ("Class" %in% names(hs_all_clades)) {
      hs_level <- "Class"
    } else if ("Superclass" %in% names(hs_all_clades)) {
      hs_level <- "Superclass"
    } else if ("Phylum" %in% names(hs_all_clades)) {
      hs_level <- "Phylum"
    } else if ("Kingdom" %in% names(hs_all_clades)) {
      hs_level <- "Kingdom"
    } else {
      hs_level <- as.character(NA)
    }
    
    if (is.na(hs_level)==FALSE){
      hs_clade <- hs_i %>%
        pull(hs_level) %>%
        unique()
    } else {
      hs_clade <- as.character(NA)
    }
    
    hs_clade_match <- hs_clade_match %>%
      bind_rows(data.frame(Code = code_i, hs_clade = hs_clade, classification_level = hs_level) %>% mutate(classification_level = if_else(classification_level == "SciName", true = "Species", false = as.character(classification_level))))
    
    rm(hs_level, hs_clade) # remove matches so they don't get carried over into next loop
    
  }

  return(hs_clade_match)
}
