#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom stringr str_detect
#' @export
clean_species_names <- function(fao_species, sau_species){
  
  # Standardize fao_species and sau_species data: column names and formats (lowercase); 
  # Rename genus to genus01 to specify that it's binary and not the genus name
  sau_species <- sau_species %>%
    dplyr::rename(CommonName=common_name,
           SciName=Scientific_name,
           Species01=Species,
           Genus01=Genus,
           Family01=Family,
           Other01=Other) %>%
    mutate(CommonName=tolower(as.character(CommonName)),
           SciName=tolower(as.character(SciName)))
  
  fao_species <- fao_species %>%
    dplyr::rename(CommonName=Species.ASFIS,
           SciName=Scientific_name,
           Species01=Species,
           Genus01=Genus,
           Other01=Other,
           Family01=Family) %>%
    mutate(CommonName=tolower(as.character(CommonName)),
           SciName=tolower(as.character(SciName)))
  
  # EXTRA steps for FAO data:
  # 1: some entries have CommonName=="[species name]", filter these out
  # 2: aristeidae and haemulidae incorrectly labelled as Other01=1; mutate family column based on suffix
  fao_species <- fao_species %>%
    filter(!(str_detect(CommonName, pattern="\\["))) %>%
    mutate(Family01 = ifelse(c(grepl(pattern = " ", SciName)==FALSE & grepl(pattern = "([^\\s])*dae", SciName)), 1, 0), # Call it a family (Family =1) if SciName is one word and ends in "dae"
           Other01 = ifelse(Family01==1, 0, Other01))
  
  
  # EXTRA steps for SAU data:
  # 1: incorrectly labels "brachyura" as a family name, should be labelled as "Other" taxa level
  sau_species <- sau_species %>%
    mutate(Other01 = ifelse(SciName=="brachyura", 1, Other01),
           Family01 = ifelse(SciName=="brachyura", 0, Family01))

  
  # Final cleaning before joining
  # Now that info is summarized in column Commodity Type, select only taxonomic info (only choose columns common to rfishbase) and metadata columns
  sau_names <- sau_species %>% 
    select(SciName, CommonName, Species01, Genus01, Family01, Other01)

  fao_names <- fao_species %>% 
    select(SciName, CommonName, Species01, Genus01, Family01, Other01)
 
  # combine FAO and SAU taxa data by all common columns
  prod_taxa_names <- fao_names %>%
    full_join(sau_names, by = c("SciName", "CommonName", "Species01", "Genus01", "Family01", "Other01")) %>% 
    arrange(SciName) %>%
    distinct()
  
  # NOTE: resulting tibble is allowed to have duplicate scientific names (but each should have a different CommonName)
  
  # Are there any conflicting metadata? i.e., same SciName but different metadata on Species, Genus, Family, Other
  # duplicates in SciName are allowed - these correspond to different CommonNames
  duplicated_names <- prod_taxa_names %>% filter(duplicated(SciName)) %>% pull(SciName)
  conflicting_names <- prod_taxa_names %>% 
    filter(SciName %in% duplicated_names) %>% # only look at rows that have multiple entries for SciName
    group_by(SciName, Species01, Genus01, Family01, Other01) %>% # specify columns that should be identical (all except CommonName)
    dplyr::summarise(TotalEntries=n()) %>% # count how many entries are identical across the selected columns, should always be greater than or equal to 2 (i.e., no conflicts)
    filter(TotalEntries<2) # confirm that "conflicting_names" is an empty tibble; originally, only conflict was for brachyura - cleaned this at the top of the script
    
  return(prod_taxa_names)
  
}