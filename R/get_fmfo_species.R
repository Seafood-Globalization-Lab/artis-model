#' @export
get_fmfo_species <- function(datadir, sau_fp, taxa_fp,
                             fb_slb_dir = "model_inputs_raw/fishbase_sealifebase",
                             threshold = 1) {
  # Percent threshold default is 1% of production going to FM
  
  #-----------------------------------------------------------------------------
  # Production Data
  sau <- read.csv(sau_fp)
  # Taxanomic information
  taxa <- read.csv(taxa_fp) %>%
    mutate(scientific_name = tolower(scientific_name))
  
  #-----------------------------------------------------------------------------
  # Get initial NON-standardized fmfo list based on percent produced (SAU)
  sau_grouped <- sau %>%
    group_by(scientific_name, end_use) %>%
    summarize(quantity = sum(sum, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(scientific_name) %>%
    mutate(total = sum(quantity, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(percent = 100 * quantity / total)
  
  fmfo_species <- sau_grouped %>%
    filter(end_use == "Fishmeal and fish oil" & percent > threshold)
  
  #-----------------------------------------------------------------------------
  # standardize fmfo species names
  
  # reads and cleans fishbase and sealifebase synonym datasets
  fb_df <- read.csv(file.path(fb_slb_dir, "fb_synonyms_clean.csv"))
  slb_df <- read.csv(file.path(fb_slb_dir, "slb_synonyms_clean.csv"))
  
  # Get a list of standardized species names
  non_standard_scinames <- unique(tolower(sau$scientific_name))
  accepted_scinames <- rep(NA, length(non_standard_scinames))
  
  # Species name standardizing (just for true species)
  for (i in 1:length(non_standard_scinames)) {
    curr_sciname <- non_standard_scinames[i]
    fb_result <- query_synonyms(fb_df, curr_sciname)
    
    # Only process synonyms for true species names
    if (str_detect(curr_sciname, " ")) {
      
      if (nrow(fb_result) > 0) {
        accepted_syn <- fb_result$synonym
        accepted_scinames[i] <- accepted_syn
      } else {
        slb_result <- query_synonyms(slb_df, curr_sciname)
        
        if (nrow(slb_result) > 0) {
          accepted_syn <- slb_result$synonym
        }
      }
    }
  }
  
  scinames_translated <- data.frame(
    sciname = non_standard_scinames,
    accepted_sciname = accepted_scinames
  )
  
  # Non true species name cleaning
  scinames_translated <- scinames_translated %>%
    left_join(
      taxa %>%
        select(scientific_name, taxon_level_id) %>%
        distinct(),
      by = c("sciname" = "scientific_name")
    ) %>%
    mutate(accepted_sciname = case_when(
      !str_detect(sciname, ' ')  & !is.na(taxon_level_id) ~ sciname,
      sciname == "marine finfishes not identified" ~ "actinopterygii", 
      sciname == "marine fishes not identified" ~ "actinopterygii",
      sciname == "marine groundfishes not identified" ~ "actinopterygii",
      sciname == "marine pelagic fishes not identified" ~ "actinopterygii", 
      sciname == "miscellaneous aquatic invertebrates" ~ "asteroidea", # assign to asteroidea for now; downstream code defines aquatic invertebrates as list of classes (if we went by phylum, ascidians would be omitted as chordata)
      sciname == "miscellaneous diadromous fishes" ~ "actinopterygii",
      sciname == "miscellaneous marine crustaceans" ~ "malacostraca", # assuming some sort of crab/lobster/shrimp/prawn/crayfish crustacean
      TRUE ~ accepted_sciname
    )) %>%
    mutate(accepted_sciname = case_when(
      is.na(accepted_sciname) ~ sciname,
      TRUE ~ accepted_sciname
    )) %>%
    select(scientific_name = accepted_sciname)
  
  
  if (sum(is.na(scinames_translated$scientific_name)) > 0) {
    warning('NOT ALL fmfo species names have been cleaned')
  }
  
  return(scinames_translated)
}

