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
    mutate(
      sciname = tolower(paste(SynGenus, SynSpecies, sep = " ")),
      taxon_level = tolower(TaxonLevel),
      status = tolower(Status)
    ) %>%
    select(
      sciname, 
      author = Author,
      status,
      synonymy = Synonymy,
      taxon_level,
      spec_code = SpecCode, 
      syn_code = SynCode,
      # This retains Aphia id unique identifier for matching to WoRMS and other databases
      # More identifers available in raw synonyms table 
      aphia_id = AphiaPK
    ) %>%
    distinct() %>% 
    # Remove spec_codes with 0 values - they appear to be a database backlog that need validation before being assigned a spec_code value
    filter(spec_code != 0)
  
  # Create accepted names table to join back to synonyms table 
  accepted_names <- the_df %>% 
  filter(
    status %in% c("accepted name", "provisionally accepted name"),
    # Only retain accepted names at the classification ranks used in ARTIS
    taxon_level %in% c("species", "genus", "family")
  ) %>% 
  select(
    spec_code, 
    accepted_name = sciname,
    accepted_status = status,
    accepted_name_aphia_id = aphia_id
  ) %>% 
  distinct()
  
  ## FIXIT Add manual corrections filter for multiple accepted names

  
  ## FIXIT - ADD check to detect multiple rows (i.e. multiple accepted names) 

  # Separate non-accepted names as the basis of the corrections table. 
  # NOTE: This assumes that all non-accepted names are synonyms, which is not always the case, 
  # This level of documentation from FishBase is not available to our knowledge - need to verify that this 
  # assumption that `status` "misapplied name" "ambiguous synonym" etc. can be 1:1 be applied to their associated `spec_code`
  synonyms_df <- the_df %>% 
    filter(!status %in% c("accepted name", "provisionally accepted name")) %>%
    rename(
      synonym = sciname,
      synonym_status = status,
      synonym_aphia_id = aphia_id
    ) %>% 
    distinct()
 
  # Assemble synonym corrections table 
  syn_corrections <- synonyms_df %>% 
    left_join(accepted_names, by = "spec_code") %>% 
    #rename(syn_taxon_level = taxon_level) %>% 
    select(
      synonym, 
      author,
      synonym_status,
      synonymy,
      #syn_taxon_level,
      syn_code,
      synonym_aphia_id,
      spec_code,
      accepted_name, 
      accepted_status,
      accepted_name_aphia_id
    )

  # Apply manual data corrections to specific snapshot ---------------------
  # Apply manual corrections conditionally based on:
  # 1) the snapshot version and
  # 2) weather to apply to fishbase or sealifebase taxa table
  # This prevents our manual corretions from propegating quietly into future snapshot data
  
  # Corrections for the `rfishbase` pkg sealifebase 25.04 snapshot
  if(the_snapshot == "25.04" & the_server == "fishbase"){
    # spec_code_counts showed spec_code 25690 had 2 accepted_names - "halichoeres vrolikii" and "julis vrolikii"
    # WoRMS lists "julis vrolikii" as `status` = "unaccepted > superseded combination"
    # WoRMS lists "halichoeres vrolikii" as `status` = "accepted"
    # Correction is to remove "julis vrolikii" as an accepted name
    syn_corrections <- syn_corrections %>% 
      filter(accepted_name != "julis vrolikii")
  }

  # Data Assumption Check
  spec_code_counts <- syn_corrections %>% 
    group_by(spec_code) %>% 
    summarize(n_accepted_names = n_distinct(accepted_name)) %>% 
    filter(n_accepted_names > 1) %>% 
    left_join(syn_corrections, by = "spec_code") %>%
    select(spec_code, synonym, author, synonym_status, accepted_name, accepted_status, synonymy, synonym_aphia_id, accepted_name_aphia_id)

  # FIXIT - update check for new table
  if (nrow(spec_code_counts)) {
    cli::cli_h3("Possible taxa synonym assumption violation")
    cli::cli_alert_warning("Each synonym is expected to have a single accepted name.")
    cli::cli_alert_info("{.val {length(unique(spec_code_counts$spec_code))}} {.field spec_code} matched to more than one accepted name.")
    cli::cli_alert_info("Requires developer to make a choice and add a manual data correction to {.fn clean_fb_slb_synonyms} and rerunning {.fn collect_fb_slb_data}")
    cli::cli_alert_info("The offending {.field spec_code} values are:")
    spec_code_counts
  } 

  # check each synonym has single accepted name
  # This will work in conjunction with the try catch loop in match_taxa
  synonym_counts <- syn_corrections %>% 
    group_by(synonym) %>% 
    summarize(n_accepted_names = n_distinct(accepted_name)) %>% 
    filter(n_accepted_names > 1)
  
  return(syn_corrections)
}
