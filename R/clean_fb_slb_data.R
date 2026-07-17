#' Clean FishBase and SeaLifeBase Data
#'
#' Cleans, corrects, and structures taxonomic, synonym, species, and common name data from
#' FishBase and SeaLifeBase database snapshots saved locally.
#'
#' @param fb_slb_dir Character string. Path to the local
#'   \code{fishbase_sealifebase_[snapshot]} directory containing raw CSV files.
#'
#' @return Invisibly returns \code{NULL}. Writes cleaned CSV files to \code{fb_slb_dir}.
#'
#' @details
#' Reads raw files from \code{fb_slb_dir} and writes the following cleaned outputs
#' to the same directory:
#' \itemize{
#'   \item \code{fb_taxa_info.csv}, \code{slb_taxa_info.csv} — cleaned taxonomic classification
#'   \item \code{fb_synonyms_clean.csv}, \code{slb_synonyms_clean.csv} — cleaned synonym tables
#'   \item \code{fb_aquarium.csv}, \code{slb_aquarium.csv} — aquarium and habitat flags
#'   \item \code{fb_common_to_sci.csv}, \code{slb_common_to_sci.csv} — common-to-scientific name tables
#' }
#'
#' @export

clean_fb_slb_data <- function(parent_outdir) {

  # Find latest existing fb slb data folder
  fb_slb_dir <- list.dirs(parent_outdir, full.names = TRUE, recursive = FALSE) %>%
    stringr::str_subset("fishbase_sealifebase_") %>%
    sort(decreasing = TRUE) %>%
    .[1]

  # Check if fb_slb_dir is valid
  if (is.na(fb_slb_dir)) {
    cli::cli_abort(c(
      "x" = "No fishbase_sealifebase directory found in {.file {path_fb_slb}}",
      "i" = "Set {.code need_new_fb_slb = TRUE} in {.file 00-local-machine-setup.R} to download new data",
      "i" = "OR ensure a fishbase_sealifebase_* directory exists"
    ))
  } 
  if (dir.exists(fb_slb_dir)) {
    cli::cli_alert_success("Using existing FB and SLB data in {.file {fb_slb_dir}}}")
  }

  # Get snapshot version from existing data folder
  snapshot <- sub(".*_(\\d+\\.\\d+)$", "\\1", basename(fb_slb_dir))

  # Taxonomic classification ---------------------------------------------------------------------

  fb_raw <- fread(file.path(fb_slb_dir, "fb_taxa_raw.csv"), data.table = FALSE)
  slb_raw <- fread(file.path(fb_slb_dir, "slb_taxa_raw.csv"), data.table = FALSE)
  
  fb_clean <- artis::clean_fb_slb_taxa(
    the_df = fb_raw,
    the_snapshot = snapshot,
    the_server = "fishbase"
  )
  slb_clean <- artis::clean_fb_slb_taxa(
    the_df = slb_raw,
    the_snapshot = snapshot,
    the_server = "sealifebase"
  )
  
  fwrite(fb_clean, file.path(fb_slb_dir, "fb_taxa_info.csv"), row.names = FALSE)
  fwrite(slb_clean, file.path(fb_slb_dir, "slb_taxa_info.csv"), row.names = FALSE)


  # Synonyms ---------------------------------------------------------------

  fb_synonyms_raw <- fread(file.path(fb_slb_dir, "fb_synonyms_raw.csv"), data.table = FALSE)
  slb_synonyms_raw <- fread(file.path(fb_slb_dir, "slb_synonyms_raw.csv"), data.table = FALSE)

  # Cleaning synonym information to use as translation tables
  fb_synonyms_clean <- artis::clean_fb_slb_synonyms(
    the_df = fb_synonyms_raw,
    the_snapshot = snapshot,
    the_server = "fishbase")
  slb_synonyms_clean <- artis::clean_fb_slb_synonyms(
    the_df = slb_synonyms_raw,
    the_snapshot = snapshot,
    the_server = "sealifebase")
  
  fwrite(fb_synonyms_clean, file.path(fb_slb_dir, "fb_synonyms_clean.csv"), row.names = FALSE)
  fwrite(slb_synonyms_clean, file.path(fb_slb_dir, "slb_synonyms_clean.csv"), row.names = FALSE)


  # Aquarium ---------------------------------------------------------------

  fb_species_raw <- fread(file.path(fb_slb_dir, "fb_species_raw.csv"), data.table = FALSE)
  slb_species_raw <- fread(file.path(fb_slb_dir, "slb_species_raw.csv"), data.table = FALSE)

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
  
  fwrite(fb_aquarium_clean, file.path(fb_slb_dir, "fb_aquarium.csv"), row.names = FALSE)
  fwrite(slb_aquarium_clean, file.path(fb_slb_dir, "slb_aquarium.csv"), row.names = FALSE)

# Common names -----------------------------------------------------------

  fb_common_raw <- fread(file.path(fb_slb_dir, "fb_common_raw.csv"), data.table = FALSE)
  fb_common_raw <- fread(file.path(fb_slb_dir, "slb_common_raw.csv"), data.table = FALSE)
  
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
  
  fwrite(fb_common_clean, file.path(fb_slb_dir, "fb_common_to_sci.csv"), row.names = FALSE)
  fwrite(slb_common_clean, file.path(fb_slb_dir, "slb_common_to_sci.csv"), row.names = FALSE)

  # file directory
  return(fb_slb_dir)
}