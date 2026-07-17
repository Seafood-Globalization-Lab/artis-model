#' Download FishBase and SeaLifeBase Data
#'
#' Downloads and writes taxonomic, synonym, species, and common name data from 
#' FishBase and SeaLifeBase database snapshots maintained by rfishbase (https://github.com/ropensci/rfishbase). 
#' Creates a timestamped directory with cleaned
#' datasets for use in taxonomic classification workflows.
#'
#' @param parent_outdir Character string. Path to the parent directory where the 
#'   timestamped fishbase_sealifebase_[database snapshot release] folder will be created.
#'
#' @return Character string. Path to the created output directory containing
#'   
#' @details 
#' The function creates the following files in a versioned subdirectory:
#' \itemize{
#'   \item fb_taxa_raw.csv, slb_taxa_raw.csv - Taxonomic classification data
#'   \item fb_synonyms_raw.csv, slb_synonyms_raw.csv - Raw synonym data
#'   \item fb_species_raw.csv, slb_species_raw.csv - Raw species information
#'   \item fb_common_raw.csv, slb_common_raw.csv - Raw common name data
#' }
#' 
#' @note This function requires internet access to download data from FishBase 
#'   and SeaLifeBase APIs. The SeaLifeBase common names table may require manual 
#'   download if API access fails.
#'
#' @export
collect_fb_slb_data <- function(parent_outdir) {

  # Label data folder with snapshot version
  # get value of most recent available snapshot versions from rfishbase
  snapshot <- rfishbase::available_releases() %>% tail(n = 1)
  outdir <- paste0("fishbase_sealifebase_", snapshot)
  outdir <- file.path(parent_outdir, outdir)

  cli_alert_info("Collecting new FB / SLB data - snapshot {.val {snapshot}}.")

  # Create directory if it does not exist
  # Assumes parent directory already exists
  if (!dir.exists(outdir)) { dir.create(outdir) }
  
# Taxonomic classification ---------------------------------------------------------------------
  
  # Species Codes, Scientific Names, Genus, Subfamily, Family, Order, Class, SuperClass
  fb_raw <- rfishbase::load_taxa(server = "fishbase", version = "latest") %>% distinct()
  slb_raw <- rfishbase::load_taxa(server = "sealifebase", version = "latest") %>% distinct()

  fwrite(fb_raw, file.path(outdir, "fb_taxa_raw.csv"), row.names = FALSE)
  fwrite(slb_raw, file.path(outdir, "slb_taxa_raw.csv"), row.names = FALSE)
  
# Synonyms -------------------------------------------- 
  
  fb_synonyms_raw <- rfishbase::fb_tbl("synonyms", server = "fishbase", version = "latest") %>% distinct()
  slb_synonyms_raw <- rfishbase::fb_tbl("synonyms", server = "sealifebase", version = "latest") %>% distinct()
  
  fwrite(fb_synonyms_raw, file.path(outdir, "fb_synonyms_raw.csv"), row.names = FALSE)
  fwrite(slb_synonyms_raw, file.path(outdir, "slb_synonyms_raw.csv"), row.names = FALSE)
  

# Aquarium information -----------------------------------------------
  
  fb_species_raw <- rfishbase::fb_tbl("species", server = "fishbase", version = "latest") %>%
    mutate(Species = paste(Genus, Species)) %>%
    distinct()
  slb_species_raw <- rfishbase::fb_tbl("species", server = "sealifebase", version = "latest") %>%
    mutate(Species = paste(Genus, Species)) %>%
    distinct()
  
  fwrite(fb_species_raw, file.path(outdir, "fb_species_raw.csv"), row.names = FALSE)
  fwrite(slb_species_raw, file.path(outdir, "slb_species_raw.csv"), row.names = FALSE)
  

# Common names -------------------------------------------
  
  fb_common_raw <- rfishbase::fb_tbl("comnames", server = "fishbase", version = "latest")
  slb_common_raw <- rfishbase::fb_tbl("comnames", server = "sealifebase", version = "latest")
  
  fwrite(fb_common_raw, file.path(outdir, "fb_common_raw.csv"), row.names = FALSE)
  fwrite(slb_common_raw, file.path(outdir, "slb_common_raw.csv"), row.names = FALSE)
  
  
  cli::cli_alert_success("Finished - new data dir {.file {outdir}}")

  # Return output directory path
  # FIXIT: This doesn't make sense to run this whole cleaning script to just output a path?
  # could separate out and return a success message instead - AM 2026-06-09
  return(outdir)

}
