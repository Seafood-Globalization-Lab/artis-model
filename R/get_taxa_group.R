#' @importFrom dplyr filter
#' @export
get_taxa_group <- function(hs_code_row, prod_taxa_classification){
  
  fish_classes <- c("actinopterygii", "cephalaspidomorphi", "elasmobranchii", "holocephali", 
                    "myxini", "sarcopterygii")
  fish_superclasses <- c("agnatha", "sarcopterygii", "chondrichthyes", "osteichthyes")
  crustacea_classes <- c("branchiopoda", "malacostraca", "maxillopoda", "merostomata", "thecostraca")
  invert_classes <- c("ascidiacea", "asteroidea", "echinoidea", "holothuroidea",
                      "polyplacophora", "scyphozoa", "thaliacea", "polychaeta", "sipunculidea", "demospongiae")
  invert_phyla <- c("chordata", "cnidaria", "echinodermata", "bryozoa")
  mollusca_phyla <- c("mollusca")
  
  # Test for taxa not falling in any defined taxa groups listed above
  taxa_test <- prod_taxa_classification %>% 
    filter(!Class %in% c(fish_classes, crustacea_classes, invert_classes)) %>% 
    filter(!Superclass %in% c(fish_superclasses)) %>% 
    filter(!Phylum %in% c(invert_phyla, mollusca_phyla)) %>% 
    pull(SciName)
  
  if(length(taxa_test) > 0) {
    warning(paste0("In get_taxa_group.R function the following taxa were not defined in any taxa group: ", taxa_test))
  }
  
  ##
  possible_scinames <- NULL
  
  if (hs_code_row$Fishes==1){ # add fishes to possible taxa
    taxa_fish <- prod_taxa_classification %>%
      filter(Class %in% fish_classes | Superclass %in% fish_superclasses) %>% # Add Superclass so that prod_taxa that only contain superclass info (e.g., osteichthyes) can go on to match with HS codes 
      pull(SciName)
    possible_scinames <- append(possible_scinames, taxa_fish)
  }
  if (hs_code_row$Crustaceans==1){ 
    # For list of all taxonomic Classes in production data: table(prod_taxa_classification$Class)
    taxa_crustacea <- prod_taxa_classification %>%
      filter(Class %in% crustacea_classes) %>%
      pull(SciName)
    possible_scinames <- append(possible_scinames, taxa_crustacea)
  }
  if (hs_code_row$Molluscs==1){ 
    taxa_mollusca <- prod_taxa_classification %>%
      filter(Phylum %in% mollusca_phyla) %>%
      pull(SciName)
    possible_scinames <- append(possible_scinames, taxa_mollusca)
  }
  if (hs_code_row$Aquatic_invertebrates==1){
    # For list of all taxonomic Classes production data: table(prod_taxa_classification$Class)
    # NOTE: malacostraca (horseshoe crabs) included here - assuming trade people are not considering these to be crustaceans
    taxa_inverts <- prod_taxa_classification %>%
      filter(Class %in% invert_classes | Phylum %in% invert_phyla)  %>% # Add Phylum so that prod_taxa that only contain Phylum info (e.g., echniodermata) can go on to match with HS codes 
      pull(SciName)
    possible_scinames <- append(possible_scinames, taxa_inverts)
  }
  
  possible_prod_taxa <- prod_taxa_classification %>%
    filter(SciName %in% possible_scinames)

  return(possible_prod_taxa)
}