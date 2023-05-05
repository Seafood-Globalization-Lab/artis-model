#' @export
add_taxa_info <- function(S_net_clean, prod_data, prod_taxa_classification){

  # Add common name, ISSCAAP group, and classification info
  
  # CHECK prod_data for sci names that have multiple common names and standardize these (only want one per sci name)
  common_name_key <- prod_data %>%
    select(SciName, CommonName) %>%
    unique() %>%
    # Clean SciNames with multiple CommonNames
    mutate(CommonName = case_when(SciName == "alosa" ~ "shads",
                                  SciName == "asteroidea" ~ "starfishes",
                                  SciName == "branchiopoda" ~ "cruastaceans",
                                  SciName == "carcharhiniformes" ~ "ground sharks",
                                  SciName == "clarias" ~ "catfishes",
                                  SciName == "clupeidae" ~ "herrings, sardines",
                                  SciName == "clupeiformes" ~ "herrings, sardines, anchovies",
                                  SciName == "dentex tumifrons" ~ "yellowback seabream",
                                  SciName == "gadus macrocephalus" ~ "pacific cod",
                                  SciName == "gobiidae" ~ "gobies",
                                  SciName == "lepidonotothen squamifrons" ~ "grey rockcod",
                                  SciName == "macrobrachium" ~ "prawns",
                                  SciName == "merluccius" ~ "hakes",
                                  SciName == "mollusca" ~ "molluscs",
                                  SciName == "mullus" ~ "red mullets",
                                  SciName == "myliobatidae" ~ "mantas, rays",
                                  SciName == "oreochromis" ~ "tilapias",
                                  SciName == "osteichthyes" ~ "ray-finned fishes",
                                  SciName == "palaemonidae" ~ "prawns, shrimps",
                                  SciName == "parastacidae" ~ "crayfish",
                                  SciName == "penaeidae" ~ "penaeid shrimps",
                                  SciName == "perciformes" ~ "perch-like ray-finned fishes",
                                  SciName == "planiliza haematocheila" ~ "redlip mullet",
                                  SciName == "planiliza haematocheilus" ~ "so-iuy (redlip) mullet",
                                  SciName == "salmonidae" ~ "salmonids",
                                  SciName == "sardinops sagax" ~ "pilchard",
                                  SciName == "sebastes" ~ "redfishes",
                                  SciName == "serrasalmidae" ~ "serrasalmids",
                                  SciName == "xiphopenaeus kroyeri" ~ "seabob",
                                  TRUE ~ CommonName)) %>%
    unique()
  
  # Create warning for when there are multiple common names per sci name
  warning_check <- common_name_key %>%
    group_by(SciName) %>%
    mutate(n_common_name = n()) %>%
    filter(n_common_name > 1)
  
  if (nrow(warning_check) > 0){
    #fix_country_collapse <- paste(production_country_to_clean, sep = "", collapse = ", ")
    warning_sci_list <- paste(warning_check %>% pull(SciName) %>% unique(), sep = "", collapse = ", ")
    warning_message <- paste("Multiple common names for: ", warning_sci_list, sep = "")
    warning(warning_message, call. = FALSE)
  } else if ("species" %in% names(S_net_clean)){
    S_net_with_common_name <- S_net_clean %>%
      left_join(common_name_key, by = c("species" = "SciName")) %>%
      rename(common_name = CommonName)
  } else if ("SciName" %in% names(S_net_clean)){ # for data requests, sometimes I'm joining hs_taxa_match to common_name in which case join by SciName
    S_net_with_common_name <- S_net_clean %>%
      left_join(common_name_key, by = "SciName") %>%
      rename(common_name = CommonName)
  }

  # Add isccaap group
  isscaap_unique <- prod_data %>%
    select(SciName, isscaap_group) %>%
    unique() %>%
    arrange(SciName)

  multi_isscaap <- data.frame(table(isscaap_unique$SciName)) %>%
    filter(Freq>1) %>%
    pull(Var1)
  
  # View SciNames with multiple matching isscaap_groups
  # isscaap_unique %>%
  #   filter(SciName %in% multi_isscaap)

  # Re-label these as "multiple possible isscaap groups"
  isscaap_standardized <- isscaap_unique %>%
    mutate(isscaap_group = if_else(SciName %in% multi_isscaap, true = "Multiple matching ISSCAAP groups", false = isscaap_group)) %>%
    unique()

  S_net_tmp <- S_net_with_common_name %>%
    left_join(prod_taxa_classification %>% select(-c(CommonName, Aquarium, Fresh01, Brack01, Saltwater01)) %>% unique(), by = c("species" = "SciName")) %>%
    left_join(isscaap_standardized, by = c("species" = "SciName"))
  
  return(S_net_tmp)
  
}