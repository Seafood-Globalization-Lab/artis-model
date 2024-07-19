#' @export
collect_fb_slb_data <- function(parent_outdir) {
  
  # Get today's date for record keeping
  current_date <- gsub("-", "", Sys.Date())
  outdir <- paste0("fishbase_sealifebase_", current_date)
  outdir <- file.path(parent_outdir, outdir)
  
  # Create directory if it does not exist
  # Assumes parent directory already exists
  if (!dir.exists(outdir)) { dir.create(outdir) }
  
  # Collecting fishbase and sealifebase taxonomic classification information
  # Contains:
    # Species Codes, Scientific Names, Genus, Subfamily, Family, Order, Class, SuperClass
  fb_raw <- load_taxa(server = "fishbase") %>% distinct()
  slb_raw <- load_taxa(server = "sealifebase") %>% distinct()
  
  write.csv(fb_raw, file.path(outdir, "fb_taxa_info.csv"), row.names = FALSE)
  write.csv(slb_raw, file.path(outdir, "slb_taxa_info.csv"), row.names = FALSE)
  
  # Collecting fishbase and sealifebase synonym information (RAW FB SLB DATA)
  fb_synonyms_raw <- fb_tbl("synonyms", server = "fishbase") %>% distinct()
  slb_synonyms_raw <- fb_tbl("synonyms", server = "sealifebase") %>% distinct()
  
  write.csv(fb_synonyms_raw, file.path(outdir, "fb_synonyms_raw.csv"), row.names = FALSE)
  write.csv(slb_synonyms_raw, file.path(outdir, "slb_synonyms_raw.csv"), row.names = FALSE)
  
  # Cleaning synonym information to use as translation tables
  fb_synonyms_clean <- clean_fb_slb_synonyms(fb_synonyms_raw)
  slb_synonyms_clean <- clean_fb_slb_synonyms(slb_synonyms_raw)
  
  write.csv(fb_synonyms_clean, file.path(outdir, "fb_synonyms_clean.csv"), row.names = FALSE)
  write.csv(slb_synonyms_clean, file.path(outdir, "slb_synonyms_clean.csv"), row.names = FALSE)
  
  # Get fishbase and sealifebase aquarium information
  fb_species_raw <- fb_tbl("species", server = "fishbase") %>%
    mutate(Species = paste(Genus, Species)) %>%
    distinct()
  slb_species_raw <- fb_tbl("species", server = "sealifebase") %>%
    mutate(Species = paste(Genus, Species)) %>%
    distinct()
  
  write.csv(fb_species_raw, file.path(outdir, "fb_species_raw.csv"), row.names = FALSE)
  write.csv(slb_species_raw, file.path(outdir, "slb_species_raw.csv"), row.names = FALSE)
  
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
  
  write.csv(fb_aquarium_clean, file.path(outdir, "fb_aquarium.csv"), row.names = FALSE)
  write.csv(slb_aquarium_clean, file.path(outdir, "slb_aquarium.csv"), row.names = FALSE)
  
  # Collect species common names and their scientific names
  fb_common_raw <- fb_tbl("comnames", server = "fishbase")
  slb_common_raw <- fb_tbl("comnames", server = "sealifebase")
  
  write.csv(fb_common_raw, file.path(outdir, "fb_common_raw.csv"), row.names = FALSE)
  write.csv(slb_common_raw, file.path(outdir, "slb_common_raw.csv"), row.names = FALSE)
  
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
  
  write.csv(fb_common_clean, file.path(outdir, "fb_common_to_sci.csv"), row.names = FALSE)
  write.csv(slb_common_clean, file.path(outdir, "slb_common_to_sci.csv"), row.names = FALSE)
  
}
