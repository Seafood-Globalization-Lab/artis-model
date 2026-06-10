#' @export
clean_fb_slb_synonyms <- function(
  the_df,
  the_snapshot,
  the_server
) {
  
# FIXIT: Document properly in Roxygen2 header
# https://www.fishbase.se/manual/english/FishBaseThe_SYNONYMS_Table.htm

  # clean sciname data
  the_df <- the_df %>%
    select(SynCode, SpecCode, SynGenus, SynSpecies, TaxonLevel, Status, Valid) %>%
    distinct() %>%
    mutate(sciname = tolower(paste(SynGenus, SynSpecies, sep = " "))) %>%
    select(
      sciname, 
      taxon_level = TaxonLevel, 
      status = Status, 
      spec_code = SpecCode, 
      syn_code = SynCode) %>%
    distinct()
  
  # Reassign status values
  the_df <- the_df %>%
    mutate(status = tolower(status)) %>% # change status to lower case
    # Unique status values - "accepted name", "synonym", "other", "misapplied name", "ambiguous synonym", "provisionally accepted name"
    mutate(status = case_when(
      status == 'ambiguous sciname' ~ 'sciname', # ambiguous scinames are treated like scinames
      status == 'provisionally accepted name' ~ 'accepted', # provisionally accepted names are treated as accepted names
      status == 'accepted name' ~ 'accepted', # change all accepted names into single word accepted
      status ==  "ambiguous synonym" ~ "synonym",
      TRUE ~ status # leave the rest of the statuses the way they are - will get filtered out
    )) %>%
    # filter rows to only include sciname or accepted name rows
    filter((status == 'accepted' | status == 'synonym')) %>% # filter just for accepted and scinames
    mutate(sciname = gsub('\\.', '', sciname)) %>% # eliminate dots
    mutate(sciname = gsub(',', '', sciname)) %>% # eliminates commas
    mutate(sciname = gsub('-', ' ', sciname)) %>% # replaces hyphens with spaces
    mutate(sciname = tolower(sciname), taxon_level = tolower(taxon_level)) %>% 
    filter(spec_code > 0) %>% # there are multiple accepted names for spec code 0 seems to be a database mistake
    
  # Apply manual data corrections to specific versions ---------------------
  # Apply manual corrections conditionally based on:
  # 1) the snapshot version and
  # 2) weather to apply to fishbase or sealifebase taxa table
  # This prevents our manual corretions from propegating quietly into future snapshot data
  
  # Corrections for the `rfishbase` pkg sealifebase 25.04 snapshot
  if(the_snapshot == "25.04" & the_server == "fishbase"){
    
    # Family veneridae had two unique values of order (nuculida and venerida). Looking 
    # at WoRMS - venerida is the correct Order value. Exclude nuculida to meet assumption
    # that each unique taxa rank value has only one set of unique higher taxa assignments. 
    the_df <- the_df %>% 
      filter(!(Family == "veneridae" & Order == "nuculida"))
  }

    # FIXIT: Apply Fishbase / Sealifebase version specific manual corrections
  # the_df <- the_df %>% 
  #   (\(x) if (rfishbase::available_releases() %>% tail(n = 1) == "25.04")
  #   )
    
  
  # prefer species level names where available otherwise choose next available accepted name
  # case: spec code 529 has 2 accepted names, one at a species level and another subspecies, we prefer the species level name
  accepted_names <- the_df %>%
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
    # puts species-level names first (subspecies etc next), then within taxaon_level sort by highest syn_code
    # Ensures that if a spec_code has multiple accepted names at different taxonomic levels, the most specific/preferred one bubbles to the top.
    arrange(taxon_level_ranking, desc(syn_code)) %>%
    group_by(spec_code) %>%
    # assign numbered rank to ordered spec_code
    mutate(name_ranking = row_number()) %>%
    ungroup() %>%
    group_by(spec_code) %>%

    # FIXIT - 2026-06-08 Is this accurate? syn_code is likely an auto-incrementing database key, a higher value = a more recently added record, so this acts as a tiebreaker preferring the newest entry.
    # only accept the latest accepted name
    filter(name_ranking == min(name_ranking)) %>% 
    ungroup() %>%
    select(-c(status, syn_code, taxon_level_ranking, name_ranking)) %>%
    rename(
      accepted_name = sciname, 
      accepted_taxon_level = taxon_level)
  
  # Check that assumed data cleaning above worked as intended
  spec_code_counts <- accepted_names %>%
    group_by(spec_code) %>%
    count() %>%
    filter(n > 1)
  
  # abort run if check fails
  if (nrow(spec_code_counts)) {
    cli::cli_abort(
      c(
        "Each cleaned FishBase / SealifeBase Synonym table {.field spec_code} is expected to have a single accepted name.",
        "x" = "{nrow(spec_code_counts)} {.field spec_code}{?s} matched to more than one accepted name: {.val {spec_code_counts$spec_code}}",
        "i" = "Requires developer to make a choice and add a manual data correction to {.fn clean_fb_slb_synonyms} and rerunning {.fn collect_fb_slb_data}"
      ),
      call = match.call()
    )
  }
  
  # Simplify after data prioritization abov
  synonyms <- the_df %>%
    select(-c(status, syn_code)) %>%
    rename(
      synonym = sciname, 
      synonym_taxon_level = taxon_level)
  
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
