#' @export
clean_fb_slb_synonyms <- function(df) {
  
  # clean sciname data
  df <- df %>%
    select(SynCode, SpecCode, SynGenus, SynSpecies, TaxonLevel, Status, Valid) %>%
    distinct() %>%
    mutate(sciname = tolower(paste(SynGenus, SynSpecies, sep = " "))) %>%
    select(sciname, taxon_level = TaxonLevel, status = Status, spec_code = SpecCode, syn_code = SynCode) %>%
    distinct()
  
  # filter rows to only include sciname or accepted name rows
  df <- df %>%
    mutate(status = tolower(status)) %>% # change status to lower case
    mutate(status = case_when(
      status == 'ambiguous sciname' ~ 'sciname', # ambiguous scinames are treated like scinames
      status == 'provisionally accepted name' ~ 'accepted', # provisionally accepted names are treated as accepted names
      status == 'accepted name' ~ 'accepted', # change all accepted names into single word accepted
      TRUE ~ status # leave the rest of the statuses the way they are - will get filtered out
    )) %>%
    filter((status == 'accepted' | status == 'synonym')) %>% # filter just for accepted and scinames
    mutate(sciname = gsub('\\.', '', sciname)) %>% # eliminate dots
    mutate(sciname = gsub(',', '', sciname)) %>% # eliminates commas
    mutate(sciname = gsub('-', ' ', sciname)) %>% # replaces hyphens with spaces
    mutate(sciname = tolower(sciname), taxon_level = tolower(taxon_level)) %>%
    filter(spec_code > 0) # there are multiple accepted names for spec code 0 seems to be a database mistake
  
  accepted_names <- df %>%
    filter(status == "accepted") %>%
    mutate(taxon_level_ranking = case_when(
      taxon_level == "species" ~ 1,
      taxon_level == "subspecies" ~ 2,
      taxon_level == "variety" ~ 3,
      taxon_level == "nominotypical" ~ 4,
      taxon_level == "infrasubspecific" ~ 5,
      taxon_level == "genus" ~ 6,
      taxon_level == "new combination" ~ 7,
      TRUE ~ 8
    )) %>%
    # prefer species level names where available otherwise choose next available accepted name
    # case: spec code 529 has 2 accepted names, one at a species level and another subspecies, we prefer the species level name
    arrange(taxon_level_ranking, desc(syn_code)) %>%
    group_by(spec_code) %>%
    mutate(name_ranking = row_number()) %>%
    ungroup() %>%
    group_by(spec_code) %>%
    filter(name_ranking == min(name_ranking)) %>% # only accept the latest accepted name
    ungroup() %>%
    select(-c(status, syn_code, taxon_level_ranking, name_ranking)) %>%
    rename(accepted_name = sciname, accepted_taxon_level = taxon_level)
  
  spec_code_counts <- accepted_names %>%
    group_by(spec_code) %>%
    count() %>%
    filter(n > 1)
  
  if (nrow(spec_code_counts)) {
    warning("There are mulitple accepted names per species code (there should only be one accepted name per code)")
    return(NULL)
  }
  
  synonyms <- df %>%
    select(-c(status, syn_code)) %>%
    rename(synonym = sciname, synonym_taxon_level = taxon_level)
  
  new_df <- synonyms %>%
    full_join(
      accepted_names,
      by = c("spec_code")
    ) %>%
    # remove any synonyms where the scientific name does not have an accepted name
    filter(!is.na(accepted_name)) %>%
    distinct()
  
  return(new_df)
}
