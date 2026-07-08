#' Clean FishBase / SeaLifeBase synonym corrections table
#'
#' Transforms a raw FishBase or SeaLifeBase synonyms table into a cleaned
#' synonym corrections table that maps non-accepted synonym name strings to
#' their accepted taxonomic names via `spec_code`.
#'
#' @details
#' Called inside [collect_fb_slb_data()] for both FishBase and SeaLifeBase.
#' The output is written to `fb_synonyms_clean.csv` / `slb_synonyms_clean.csv`
#' and later read by multiple downstream functions that pass it to
#' [query_synonyms()] for synonym resolution:
#'
#' * [match_prod_taxa_to_fbslb()] — resolves unmatched production scientific
#'   names during taxa matching
#' * [clean_hs()] — resolves HS product code species names to accepted names
#' * [compile_cf()] — resolves unmatched species names during conversion factor
#'   compilation
#'
#' ## Data integrity checks
#'
#' Runs three data integrity checks and emits `cli` warnings if violations are
#' found. Both checks test the assumption that a single synonym name string
#' maps to a single accepted taxonomic name, but via different pathways:
#'
#' * **`spec_code` integrity check** — groups by `spec_code` and detects cases
#'   where a single FishBase/SeaLifeBase species ID resolves to more than one
#'   `accepted_name`. Indicates an upstream database error — the same species
#'   ID was assigned conflicting accepted names in FishBase/SeaLifeBase.
#' * **Synonym string ambiguity check** — groups by `synonym` name string and
#'   detects cases where the same synonym text appears as multiple distinct
#'   FishBase/SeaLifeBase entries (different `syn_code`s) pointing to different
#'   `spec_code`s. The synonym string alone is insufficient to uniquely
#'   identify a species, causing a one-to-many join error in [query_synonyms()].
#' * **`accepted_status` integrity check** — groups by `spec_code` and detects
#'   cases where both `"accepted name"` and `"provisionally accepted name"`
#'   coexist for the same species ID. Indicates FishBase/SeaLifeBase assigned
#'   conflicting acceptance statuses to the same species ID. No working
#'   protocol exists for this scenario; requires developer investigation via
#'   WoRMS.
#'
#' ## Manual corrections
#'
#' Manual corrections (snapshot- and server-specific) are applied after table
#' assembly and before the assumption checks. See the
#' *Apply manual corrections* section of the function body to add manual corrections.
#'
#' @param the_df Data frame. Raw synonyms table as returned by
#'   `rfishbase::fb_tbl("synonyms", ...)`.
#' @param the_snapshot Character. The `rfishbase` snapshot version (e.g.
#'   `"25.04"`). Used to scope manual corrections to the correct snapshot and
#'   prevent corrections from propagating silently into future snapshot data.
#' @param the_server Character. One of `"fishbase"` or `"sealifebase"`. Used
#'   alongside `the_snapshot` to scope manual corrections to the correct
#'   database.
#'
#' @return A data frame with one row per synonym–accepted name pair.
#'
#' @note `spec_code` values of `0` are dropped — these are FishBase/SeaLifeBase
#'   backlog entries awaiting validation and have not yet been assigned a valid
#'   species ID.
#'
#' @seealso
#' * [collect_fb_slb_data()] — calls this function and writes the output to
#'   disk
#' * [query_synonyms()] — performs the synonym lookup using the output CSV
#' * [match_prod_taxa_to_fbslb()] — reads the output CSV and passes it to
#'   [query_synonyms()] for synonym resolution
#' * [classify_prod_dat()] — reads the output CSV and passes it to
#'   [query_synonyms()] for synonym resolution
#' * [clean_hs()] — reads the output CSV and passes it to
#'   [query_synonyms()] for synonym resolutions
#' * [compile_cf()] — reads the output CSV and passes it to
#'   [query_synonyms()] for synonym resolution
#' * FishBase SYNONYMS table documentation:
#'   <https://www.fishbase.se/manual/english/FishBaseThe_SYNONYMS_Table.htm>
#'
#' @import dplyr
#' @import cli
#' @importFrom magrittr %>%
#' @export
 
clean_fb_slb_synonyms <- function(
  the_df,
  the_snapshot,
  the_server
) {

  # clean sciname data
  the_df <- the_df %>%
    mutate(
      sciname = tolower(paste(SynGenus, SynSpecies, sep = " ")),
      taxon_level = tolower(TaxonLevel),
      status = tolower(Status)
    ) %>%
    select(
      sciname, 
      synonym_author = Author,
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
    aphia_id_accepted_name = aphia_id
  ) %>% 
  distinct()

  # Separate non-accepted names as the basis of the corrections table. 
  # NOTE: This assumes that all non-accepted names are synonyms, which is not always the case, 
  # This level of documentation from FishBase is not available to our knowledge - need to verify that this 
  # assumption that `status` "misapplied name" "ambiguous synonym" etc. can be 1:1 be applied to their associated `spec_code`
  synonyms_df <- the_df %>% 
    filter(!status %in% c("accepted name", "provisionally accepted name")) %>%
    rename(
      synonym = sciname,
      synonym_status = status,
      aphia_id_synonym = aphia_id
    ) %>% 
    distinct()
 
  # Assemble synonym corrections table 
  syn_corrections <- synonyms_df %>% 
    left_join(accepted_names, by = "spec_code") %>% 
    #rename(syn_taxon_level = taxon_level) %>% 
    select(
      synonym, 
      synonym_author,
      synonym_status,
      synonymy,
      #syn_taxon_level,
      syn_code,
      aphia_id_synonym,
      spec_code,
      accepted_name, 
      accepted_status,
      aphia_id_accepted_name
    )

  # Apply manual corrections ---------------------

  # Corrections need to be conditionally based on:
  # 1) the snapshot version and
  # 2) weather to apply to fishbase or sealifebase taxa table
  # This prevents our manual corretions from propegating quietly into future snapshot data
  
  # Corrections for the `rfishbase` pkg 25.04 snapshot
  if(the_snapshot == "25.04" & the_server == "fishbase"){

    # spec_code_counts showed spec_code 25690 had 2 accepted_names - "halichoeres vrolikii" and "julis vrolikii"
    # WoRMS lists "julis vrolikii" as `status` = "unaccepted > superseded combination"
    # WoRMS lists "halichoeres vrolikii" as `status` = "accepted"
    # Correction is to remove "julis vrolikii" as an accepted name
    syn_corrections <- syn_corrections %>% 
      filter(accepted_name != "julis vrolikii")

    # Single synonym name with different syn_code and accepted names - causes a one-to-many join problem
    # when unmatched prod taxa run through the synonyms matching loop. 
    syn_corrections <- syn_corrections %>% 
      filter(!(synonym == "alectis indica" & syn_code == 8344))

    # "labeo sindensis" has two rows with two accepted names.
    # One is a "misapplied name" status that does not show up in Worms and unlikely 
    syn_corrections <- syn_corrections %>% 
      filter(!(synonym == "labeo sindensis" & syn_code == 155825))

    # "polymesoda expansa" has two rows with two accepted names
    # One is a "misapplied name" status that does not show up in Worms and unlikely 
    # accepted name Worms aphiaID 872679 has "unaccepted" status - remove
    syn_corrections <- syn_corrections %>% 
      filter(!(synonym == "polymesoda expansa" & syn_code == 92503))

    # "chrysophrys auratus" is used by two distinct species (Linnaeus 1758 -> sparus aurata;
    # Forster 1801 -> pagrus auratus). Collapse to a single ambiguous synonym pointing to family Sparidae.
    chrysophrys_row <- syn_corrections %>%
      filter(synonym == "chrysophrys auratus") %>%
      summarise(
        synonym         = first(synonym),
        synonym_author  = paste(synonym_author, collapse = "; "),
        synonym_status  = NA_character_,
        synonymy        = NA_character_,
        syn_code        = NA_integer_,
        aphia_id_synonym           = NA_integer_,
        spec_code                  = NA_integer_,
        accepted_name              = "sparidae",
        accepted_status            = "accepted name",
        aphia_id_accepted_name     = 125564
      )

    syn_corrections <- syn_corrections %>%
      filter(synonym != "chrysophrys auratus") %>%
      bind_rows(chrysophrys_row)    

  }

  if(the_snapshot == "25.04" & the_server == "sealifebase") {      
    
  }




# Data assumption checks -------------------------------------------------

  # -- Fb/slb integrity check - Each spec_code maps to a single accpeted_name --
  # Values in spec_code_counts mean Fb/slb assigned two accepted names to the same species ID in error.
  # This is the cause of an upstream one-to-many join problem
  spec_code_counts <- syn_corrections %>% 
    group_by(spec_code) %>% 
    mutate(n_accepted_names = n_distinct(accepted_name)) %>% 
    filter(n_accepted_names > 1)

  if (nrow(spec_code_counts)) {
    cli::cli_h2("Possible taxa synonym assumption violation - Check 1 {the_server}")
    cli::cli_alert_warning("Each unique {.field spec_code} needs to resolve to a single {.field accpeted_name}.")
    cli::cli_alert_info("{.val {length(unique(spec_code_counts$spec_code))}} {.field spec_code}{?s} matched to more than one accepted name.")
    cli::cli_alert_info("{.strong Developer Notes}:" ) 
    cli::cli_ul(c(
      "Step into {.fn clean_fb_slb_synonyms} to see the violations in internal dataframe {.field spec_code_counts}.",
      "Need to investigate situation on WoRMS (using {.field *aphia_id} columns) and determine if a manual correction 
      should be applied in {.fn clean_fb_slb_synonyms} and rerun {.fn collect_fb_slb_data}.",
      "Manual correction may not be needed for each {.field spec_code} depending on the number of synonym violations.",
      "Can think about cross referencing production taxa names to understand if these synonym violations will interact 
      with the data ingested into ARTIS model."
    ))
  } 

  # -- Data Assumption Check - Each synonym name has single accepted name --
  # artis::query_synonyms() matches unmatched prod scinames to synonym name strings (does not use author);
  # different synonyms may be reduced effectively to a single synonym name string and cause a join error in this process.
  # FIXIT: Developer may need to consider filtering out certain "status" of synonyms, or this may not be a problem if the synonyms are not 
  # using in the prod taxa matching process. 
  syn_code_counts <- syn_corrections %>%
    group_by(synonym) %>%
    mutate(
      n_syn_code = n_distinct(syn_code),
      n_spec_code = n_distinct(spec_code)) %>%
    # Find problem synonyms - same text synonym name with different accepted names
    filter(
      n_syn_code > 1,
      n_spec_code > 1
    )

  if(nrow(syn_code_counts)){
    cli::cli_h2("Possible taxa synonym assumption violation - Check 2 {the_server}")
    cli::cli_alert_warning("Each unique {.field synonym} name needs to map to a single {.field syn_code} and {.field spec_code}.")
    cli::cli_alert_info("{.val {length(unique(syn_code_counts$synonym))}} synonym {?name/names} matched to more than one {.field accepted_name}, 
      making the synonym string ambiguous in downstream prod taxa matching in {.fn query_synonyms} called in {.fn match_prod_taxa_to_fbslb}.")
    cli::cli_alert_info("{.strong Developer Notes}:") 
    cli::cli_ul(c(
      "Assumption violations documented internally in {.field syn_code_counts} data frame in {.fn clean_fb_slb_synonyms}.",
      "Manual corrections are made in in {.emph Apply manual corrections} section of {.fn clean_fb_slb_synonyms}.",
      "Manual corrections may not be required if they are not used in the prod taxa matching process."
    ))
  }

  # 3)
  # -- Fb/slb integrity check - Each spec_code maps to a single accepted_status --
  # Detects spec_codes where both "accepted name" and "provisionally accepted name" 
  # coexist, indicating FishBase/SeaLifeBase assigned conflicting acceptance statuses
  # to the same species ID.
  accepted_status_counts <- syn_corrections %>%
    group_by(spec_code) %>%
    filter(
      all(c("accepted name", "provisionally accepted name") %in% accepted_status)
    )

  if (nrow(accepted_status_counts)) {
    cli::cli_h2("Possible taxa synonym assumption violation - Check 3 {the_server}")
    cli::cli_alert_warning("Each {.field spec_code} needs to resolve to a single {.field accepted_status}.")
    cli::cli_alert_info(
      "{.val {length(unique(accepted_status_counts$spec_code))}} {.field spec_code}{?s} assigned both {.val accepted name} and {.val provisionally accepted name} statuses."
    )
    cli::cli_alert_info("{.strong Developer Notes}:")
    cli::cli_ul(c(
      "Need to determine if the {.val accepted name} or {.val provisionally accepted name} value should be retained; 
      No working protocol in place for this scenario.",
      "Step into {.fn clean_fb_slb_synonyms} to see the violations in internal dataframe {.field syn_code_counts}.", 
      "Investigate situation on WoRMS (using {.field *aphia_id} columns) and determine if a manual 
      correction should be applied in {.fn clean_fb_slb_synonyms} and rerun {.fn collect_fb_slb_data}."
    ))
  }

  return(syn_corrections)
}
