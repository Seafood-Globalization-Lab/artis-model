#' Collect FishBase and SeaLifeBase Data
#'
#' Downloads and processes taxonomic, synonym, species, and common name data from 
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
#'   \item fb_taxa_info.csv, slb_taxa_info.csv - Taxonomic classification data
#'   \item fb_synonyms_raw.csv, slb_synonyms_raw.csv - Raw synonym data
#'   \item fb_synonyms_clean.csv, slb_synonyms_clean.csv - Cleaned synonym translation tables
#'   \item fb_species_raw.csv, slb_species_raw.csv - Raw species information
#'   \item fb_aquarium.csv, slb_aquarium.csv - Aquarium and habitat data
#'   \item fb_common_raw.csv, slb_common_raw.csv - Raw common name data
#'   \item fb_common_to_sci.csv, slb_common_to_sci.csv - Common to scientific name translations
#' }
#' 
#' @note This function requires internet access to download data from FishBase 
#'   and SeaLifeBase APIs. The SeaLifeBase common names table may require manual 
#'   download if API access fails.
#'
#' @export
collect_fb_slb_data <- function(parent_outdir) {
  
  # Label data folder with release version
  # get value of most recent available release versions from rfishbase
  release <- rfishbase::available_releases() %>% tail(n = 1)
  outdir <- paste0("fishbase_sealifebase_", release)
  outdir <- file.path(parent_outdir, outdir)
  
  # Create directory if it does not exist
  # Assumes parent directory already exists
  if (!dir.exists(outdir)) { dir.create(outdir) }
  
  # Collecting fishbase and sealifebase taxonomic classification information
  # Contains:
    # Species Codes, Scientific Names, Genus, Subfamily, Family, Order, Class, SuperClass
  fb_raw <- load_taxa(server = "fishbase", version = "latest") %>% distinct()
  slb_raw <- load_taxa(server = "sealifebase", version = "latest") %>% distinct()
  
  fwrite(fb_raw, file.path(outdir, "fb_taxa_info.csv"), row.names = FALSE)
  fwrite(slb_raw, file.path(outdir, "slb_taxa_info.csv"), row.names = FALSE)
  
  # Collecting fishbase and sealifebase synonym information (RAW FB SLB DATA)
  fb_synonyms_raw <- fb_tbl("synonyms", server = "fishbase", version = "latest") %>% distinct()
  slb_synonyms_raw <- fb_tbl("synonyms", server = "sealifebase", version = "latest") %>% distinct()
  
  fwrite(fb_synonyms_raw, file.path(outdir, "fb_synonyms_raw.csv"), row.names = FALSE)
  fwrite(slb_synonyms_raw, file.path(outdir, "slb_synonyms_raw.csv"), row.names = FALSE)
  
  # Cleaning synonym information to use as translation tables
  fb_synonyms_clean <- clean_fb_slb_synonyms(fb_synonyms_raw, version = "latest")
  slb_synonyms_clean <- clean_fb_slb_synonyms(slb_synonyms_raw, version = "latest")
  
  fwrite(fb_synonyms_clean, file.path(outdir, "fb_synonyms_clean.csv"), row.names = FALSE)
  fwrite(slb_synonyms_clean, file.path(outdir, "slb_synonyms_clean.csv"), row.names = FALSE)
  
  # Get fishbase and sealifebase aquarium information
  fb_species_raw <- fb_tbl("species", server = "fishbase", version = "latest") %>%
    mutate(Species = paste(Genus, Species)) %>%
    distinct()
  slb_species_raw <- fb_tbl("species", server = "sealifebase", version = "latest") %>%
    mutate(Species = paste(Genus, Species)) %>%
    distinct()
  
  fwrite(fb_species_raw, file.path(outdir, "fb_species_raw.csv"), row.names = FALSE)
  fwrite(slb_species_raw, file.path(outdir, "slb_species_raw.csv"), row.names = FALSE)
  
  # Clean fb and slb species data to include only aquarium data
  fb_aquarium_clean <- fb_species_raw %>%
    rename(SciName = Species) %>%
    mutate(SciName = tolower(SciName)) %>%
    select(c(SciName, Aquarium, Fresh, Brack, Saltwater)) %>%
    distinct()
  
  slb_aquarium_clean <- slb_species_raw %>%
    rename(SciName = Species) %>%
    mutate(SciName = tolower(SciName)) %>%
    select(c(SciName, Aquarium, Fresh, Brack, Saltwater)) %>%
    distinct()
  
  fwrite(fb_aquarium_clean, file.path(outdir, "fb_aquarium.csv"), row.names = FALSE)
  fwrite(slb_aquarium_clean, file.path(outdir, "slb_aquarium.csv"), row.names = FALSE)
  
  # Collect species common names and their scientific names
  fb_common_raw <- fb_tbl("comnames", server = "fishbase", version = "latest")
  # dev_mode 2025_08_14 - rfishbase error with GET query to remote slb common name table
  slb_common_raw <- fb_tbl("comnames", server = "sealifebase", version = "latest")
  # NOTE: slb common name table mannually downloaded from url provided in error
  # https://huggingface.co/datasets/cboettig/fishbase/resolve/main/data/slb/v24.07/parquet/comnames.parquet
  #slb_common_raw <- arrow::read_parquet(file.path(outdir, "comnames.parquet"))
  
  fwrite(fb_common_raw, file.path(outdir, "fb_common_raw.csv"), row.names = FALSE)
  fwrite(slb_common_raw, file.path(outdir, "slb_common_raw.csv"), row.names = FALSE)
  
  # Clean common to scientific names translation tables
  fb_common_clean <- fb_common_raw %>%
    filter(Language == "English") %>%
    select(ComName, Language, SpecCode) %>%
    distinct() %>%
    left_join(
      fb_raw %>%
        select(SpecCode, Species) %>%
        distinct(),
      by = c("SpecCode")
    ) %>%
    mutate(Species = tolower(Species),
           ComName = tolower(ComName)) %>%
    rename(SciName = Species, CommonName = ComName, spec_code = SpecCode) %>%
    distinct()
  
  slb_common_clean <- fb_common_raw %>%
    filter(Language == "English") %>%
    select(ComName, Language, SpecCode) %>%
    distinct() %>%
    left_join(
      slb_raw %>%
        select(SpecCode, Species) %>%
        distinct(),
      by = c("SpecCode")
    ) %>%
    mutate(Species = tolower(Species),
           ComName = tolower(ComName)) %>%
    rename(SciName = Species, CommonName = ComName, spec_code = SpecCode) %>%
    distinct()
  
  fwrite(fb_common_clean, file.path(outdir, "fb_common_to_sci.csv"), row.names = FALSE)
  fwrite(slb_common_clean, file.path(outdir, "slb_common_to_sci.csv"), row.names = FALSE)
  
  # Return output directory path
  return(outdir)

}
