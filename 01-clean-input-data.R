# Clean raw input data

###############################################################################

# Setup -------------------------------------------------------------------

# clear environment
rm(list=ls())

### Set folder paths
# raw data
datadir <- "model_inputs_raw"
# where to write clean data files
outdir <- "AM_local/outputs"
# the BACI release version used [yyyymmX]
baci_version <- "202401b"

# HS versions to run through the pipeline - add new versions here
# No need to do HS92 when using BACI though as that data starts in 1996
HS_versions <- c("96", "02", "07", "12", "17", "22")
# Define start years for each HS_version - add new version here
hs_start_years <- c("96" = 1996, "02" = 2002, "07" = 2007, "12" = 2012, 
                    "17" = 2017, "22" = 2022) 

# Get the latest HS version dynamically
latest_data_year <- hs_start_years[[last(HS_versions)]]

# Generate all possible combinations of HS versions and years dynamically
hs_yr_combo_df <- do.call(rbind, lapply(HS_versions, function(hs) {
  data.frame(HS_version = hs, 
             analysis_year = seq(hs_start_years[[hs]], latest_data_year))
}))

# folder with raw BACI data
baci_raw_dir <- file.path("~/Documents/UW-SAFS/ARTIS/data/model_inputs_raw_v2_0", "baci_raw")
baci_filtered_dir <- file.path(outdir, "baci_filtered", paste0("baci_", baci_version))
# name of FAO Global Production .zip file (must be .zip)
global_prod_filename <- "GlobalProduction_2024.1.0.zip"

# Creating out folder if necessary
if (!dir.exists(outdir)) {
  dir.create(outdir)
} else {
  warning("OUTDIR already exists!")
}

# Load packages 
library(artis)
library(tidyverse)
library(countrycode)
library(doParallel)
library(rfishbase)
library(data.table)


# Set model variables -----------------------------------------------

# Set TRUE if running SAU mode, FAO mode is FALSE
running_sau <- FALSE

## Set if new Fishbase/SeaLifeBase data pull needed:
need_new_slb <- FALSE

# If running a test environment with specific codes scinames this variable should be true else false
test <- FALSE
test_year <- 2018
test_hs <- "12"

if(test == TRUE){
  test_scinames <- fread("demo/sciname_shrimps_prawns.csv") %>%
    select(sciname) %>%
    distinct() %>%
    pull(sciname)
  
  test_codes <- c("030617", "160529", "160521", "030627", "030616", "030626")
}else{}

# Production sciname cleaning ---------------------------------

# Generate new fishbase and sealifebase data files
# Note: these do not need to be generated for each model run and can be done once per year/quarter
# Directory Structure:
  # creates fishbase_sealifebase_[MOST RECENT DATE] within model_inputs_raw (ie. "model_inputs_raw/fishbase_sealifebase_[MOST_RECENT_DATE]")
if(need_new_slb == TRUE) {
  collect_fb_slb_data(datadir)
  message("New fishbase and sealifebase data files have been generated.")
} else {
  message("Existing fishbase and sealifebase data files are being used; Not collecting new data.")
}

# Find the most recent version of the fishbase and sealifebase data files needed
fb_slb_info <- get_most_recent_dir(datadir, "fishbase_sealifebase")
current_fb_slb_dir <- fb_slb_info$directory

# Clean scientific names and add classification info to production data: choose FAO or SAU
# NOTE: warning message about data_frame() being deprecated is fixed in the development version of rfishbase: run remotes::install_github("ropensci/rfishbase") to implement the fixed version
prod_list <- classify_prod_dat(datadir = datadir,
                               filename = global_prod_filename, 
                               prod_data_source = "FAO",
                               fb_slb_dir = current_fb_slb_dir)

# Reassign to separate objects:
prod_data_raw <- prod_list[[1]] 
prod_taxa_classification <- prod_list[[2]] %>%
  # FIXIT: can this go inside classify_prod_dat?
  mutate(Fresh01 = case_when(
    SciName == "neocaridina denticulata" ~ as.integer(1),
    TRUE ~ as.integer(Fresh01)
  )) %>%
  mutate(Saltwater01 = case_when(
    SciName == "anadara grandis" ~ as.integer(1),
    TRUE ~ as.integer(Saltwater01)
  ))

# Get fishbase habitat data from prod_taxa_classification to standardize habitat info in prod_data
prod_habitat <- prod_taxa_classification %>%
  select(SciName, Fresh01, Brack01, Saltwater01) %>%
  distinct()

# FIXIT: Can this go inside classify_prod_dat?
prod_data <- prod_data_raw %>%
  filter(quantity > 0) %>%
  # Filter to 1996 and on - earlier data quality is too low to justify cleaning 
  # required to ingest (i.e. breakup of Soviet Union)
  filter(year > 1995)  %>% 
  select(!c(any_of(c("alternate", "multiplier", "symbol", "symbol_identifier")), contains(c("_ar", "_cn", "_es", "_fr", "_ru")))) %>%
  # Create new column that combines SciName with souce info (i.e., habitat + production method)
  mutate(fao_habitat = case_when(habitat == "Inland waters" ~ "inland",
                             habitat == "Marine areas" ~ "marine",
                             TRUE ~ habitat),
         prod_method = case_when(prod_method %in% c("FRESHWATER", "MARINE", "BRACKISHWATER") ~ "aquaculture",
                                 prod_method == "CAPTURE" ~ "capture",
                                 TRUE ~ prod_method)) %>%
  mutate(taxa_source = paste0(str_replace(SciName, " ", "."), fao_habitat, prod_method)) %>%
  # Join fishbase habitat data to prod data and make new Fishbase habitat column to compare to FAO's habitat column 
  left_join(prod_habitat, by = "SciName") %>%
  mutate(fb_habitat = case_when(Fresh01 == 1 & Saltwater01 == 0 ~ "inland",
                                Fresh01 == 0 & Saltwater01 == 1 ~ "marine",
                                Fresh01 == 1 & Saltwater01 == 1 ~ "diadromous",
                                # If a species just exists in brackish water we classify as marine
                                Brack01 == 1 & Fresh01 == 0 & Saltwater01 == 0 ~ "marine",
                                TRUE ~ as.character(NA))) %>% # Taxa with fb_habitat = NA are higher order than species so habitat not necessarily universal 
  # if fishbase (marine/inland) conflicts with FAOs (marine/inland) then use Fishbase designation
  mutate(habitat = case_when(str_detect(SciName, pattern = " ") & fb_habitat != fao_habitat & fb_habitat %in% c("inland", "marine") ~ fb_habitat,
                                 TRUE ~ fao_habitat)) %>% # ELSE, use FAO's habitat designation, including for all non species-level data
  # UPDATE taxa source to match structure in get country solutions
  mutate(taxa_source = paste0(str_replace(SciName, " ", "."), habitat, prod_method)) %>%
  group_by(country_iso3_alpha, country_name_en, CommonName, SciName, taxa_source, habitat, prod_method, year, Fresh01, Saltwater01, Brack01, Species01, Genus01, Family01, Other01, isscaap_group) %>%
  summarize(quantity = sum(quantity, na.rm = TRUE)) %>%
  ungroup()

# Changing class name based on FAO 2022 species list
# some sources call actinopterygii a class others call it a superclass (might need to change with osteichthyes instead)
prod_taxa_classification <- prod_taxa_classification %>%
  mutate(Class = case_when(
    Class == 'actinopteri' ~ 'actinopterygii',
    TRUE ~ Class
  ))

if (test) {
  
  prod_data <- prod_data %>%
    filter(year == test_year) %>%
    filter(SciName %in% test_scinames)
  
  prod_taxa_classification<- prod_taxa_classification %>%
    filter(SciName %in% test_scinames)
}

# Save production output
fwrite(prod_data, file = file.path(outdir, "clean_fao_prod.csv"), row.names = FALSE)
fwrite(prod_taxa_classification, file = file.path(outdir, "clean_fao_taxa.csv"), row.names = FALSE)


# Standardize countries - FAO production ----------------------------------

prod_data <- standardize_countries(df = prod_data,
                                   data_source = "FAO")
fwrite(prod_data, file = file.path(outdir, "standardized_fao_prod.csv"), row.names = FALSE)

rm(prod_list)

# SAU data----------------------------------------------------------------------
if (running_sau) {
  # sciname cleaning
  prod_list_sau <- classify_prod_dat(datadir = datadir,
                                     filename = 'SAU_Production_Data.csv',
                                     prod_data_source = 'SAU',
                                     SAU_sci_2_common = "TaxonFunctionalCommercial_Clean.csv",
                                     fb_slb_dir = current_fb_slb_dir)
  
  prod_data_sau <- prod_list_sau[[1]] %>%
    #FIXIT: is this already in classify_prod_dat?
    mutate(year = as.numeric(year)) %>%
    mutate(quantity = as.numeric(quantity)) %>%
    filter(quantity > 0) %>%
    filter(year > 1995) 
  
  prod_data_sau <- prod_data_sau %>%
    # FIXIT: This may remove eez, sector, and end_use columns
   # select(colnames(prod_data_sau)[colnames(prod_data_sau) %in% c("country_name_en", colnames(prod_data))]) %>%
    mutate(habitat = "marine",
           prod_method = "capture") %>%
    mutate(taxa_source = paste0(str_replace(SciName, " ", "."), 
                               habitat, prod_method))
  
  prod_classification_sau <- prod_list_sau[[2]]
  
  prod_data_sau <- prod_data_sau %>%
    # Keep FAO format
    mutate(Fresh01 = 0, Saltwater01 = 1, Brack01 = 0)
  
  rm(prod_list_sau)
  
  fwrite(prod_data_sau, file.path(outdir, "clean_sau_prod.csv"), 
            row.names = FALSE)
  fwrite(prod_classification_sau, file.path(outdir, "clean_sau_taxa.csv"), 
            row.names = FALSE)
  
  # initial country name cleaning and adding iso3c for SAU data
  prod_data_sau <- prod_data_sau %>%
    mutate(country_name_en = str_remove(country_name_en, ' \\(.+\\)$')) %>%
    mutate(country_iso3_alpha = countrycode(country_name_en, origin = 'country.name', destination = 'iso3c')) %>%
    # Renaming for standardize countries function later
    mutate(country_name_en = case_when(
      country_name_en == 'Channel Isl.' ~ 'Channel Islands',
      country_name_en == 'Unknown Fishing Country' ~ 'Other nei',
      TRUE ~ country_name_en
    )) %>%
    mutate(country_iso3_alpha = case_when(
      country_name_en == 'Ascension Isl.' ~ 'SHN', # (will get standardized later)
      country_name_en == 'Azores Isl.' ~ 'PRT', # Azores Islands part of Portugal
      country_name_en == 'Bonaire' ~ 'BES', # Bonaire (will get standardized later)
      country_name_en == 'Brit. Indian Ocean Terr.' ~ 'IOT', # British Indian Ocean Territory (will get standardized later)
      country_name_en == 'Madeira Isl.' ~ 'PRT', # Madeira Islands part of Portugal
      country_name_en == 'Micronesia' ~ 'FSM', # Federated States of Micronesia
      country_name_en == 'Saba and Saint Eustaius' ~ 'BES', # Saba and Saint Eustaius (will get standardized later)
      country_name_en == 'St Martin' ~ 'MAF', # (will get standardized later)
      country_name_en == 'Tristan da Cunha Isl.' ~ 'SHN', # (will get standardized later)
      country_name_en == 'US Virgin Isl.' ~ 'VIR',
      country_name_en == 'Unknown Fishing Country' ~ 'NEI',
      TRUE ~ country_iso3_alpha
    )) %>%
    mutate(country_iso3_numeric = countrycode(country_iso3_alpha, 
                                              origin = 'iso3c', 
                                              destination = 'iso3n'))
  
  # standardize countries for SAU production
  prod_data_sau <- standardize_countries(df = prod_data_sau,
                                         data_source = "FAO", 
                                         all_sau_cols = TRUE)
  
  fwrite(prod_data_sau, file.path(outdir, 'standardized_sau_prod.csv'), row.names = FALSE)
  
  # Combine SAU production data with FAO data
  prod_data <- prod_data %>%
    filter(!(habitat == 'marine' & prod_method == 'capture')) %>%
    bind_rows(prod_data_sau)
  
  fwrite(prod_data, file.path(outdir, 'standardized_combined_prod.csv'),
            row.names = FALSE)
  
  # combine prod taxa classification
  prod_taxa_classification <- prod_taxa_classification %>%
    bind_rows(prod_classification_sau) %>%
    distinct() %>%
    filter(SciName %in% unique(prod_data$SciName))
  
  fwrite(prod_taxa_classification, 
            file.path(outdir, "clean_taxa_combined.csv"),
            row.names = FALSE)
}


# HS taxa classification - sciname habitat --------------------------------

# Make sure that prod taxa classification is classified to at least one of Species, Genus, Family, Other

# Creating sciname habitat dataframe for habitat classification in hs taxa classification
sciname_habitat <- prod_taxa_classification %>%
  select(SciName, Fresh01, Brack01, Saltwater01) %>%
  # Removing duplicates caused by having multiple common names for a single sciname
  distinct() %>%
  mutate(habitat = case_when(Fresh01 == 1 & Saltwater01 == 0 ~ "inland",
                             Fresh01 == 0 & Saltwater01 == 1 ~ "marine",
                             Fresh01 == 1 & Saltwater01 == 1 ~ "diadromous",
                             # If a species just exists in brackish water we classify as marine
                             Brack01 == 1 & Fresh01 == 0 & Saltwater01 == 0 ~ "marine",
                             TRUE ~ as.character(NA)))

# Create V1 and V2 for each HS version----------------------------------

# Load and clean the conversion factor data and run the matching functions. 
# This data will be used to create V1 and V2. 
hs_data_clean <- clean_hs(hs_data_raw = fread(file.path(datadir, "All_HS_Codes.csv"),
                                              colClasses = "character"),
                          fb_slb_dir = current_fb_slb_dir)

# Getting list of fmfo species
fmfo_species <- get_fmfo_species(datadir,
                                 sau_fp = file.path(datadir, 'SAU_Production_Data.csv'),
                                 taxa_fp = file.path(datadir,
                                                     'TaxonFunctionalCommercial_Clean.csv'),
                                 fb_slb_dir = current_fb_slb_dir
)

fwrite(fmfo_species, 
       file.path(datadir, 'fmfo_species_list.csv'), 
       row.names = FALSE)

# filter data if running test
if (test) {
  HS_versions <- HS_versions[HS_versions %in% test_hs]
  hs_data_clean <- hs_data_clean %>%
    filter(Code %in% test_codes)
}

for(i in 1:length(HS_versions)) {
  
  HS_versions_i <- paste0("HS", HS_versions[i])
  print(HS_versions_i)
  
  # Match HS codes to production taxa (can be FAO or SAU depending on which was used in clean_and_clasify_prod_dat function)
  hs_taxa_match <- match_hs_to_taxa(hs_data_clean = hs_data_clean,
                                    prod_taxa_classification = prod_taxa_classification,
                                    # species used for FMFO globally based on SAU data
                                    fmfo_species_list = fmfo_species,
                                    hs_version = HS_version_i)
  
  # Check that all species in hs taxa match are in the production data
  if (sum(!(unique(hs_taxa_match$SciName) %in% unique(prod_data$SciName))) > 0) {
    warning("Not all scinames in hs_taxa match are in production data")
    print(unique(hs_taxa_match$SciName)[!unique(hs_taxa_match$SciName) %in% unique(prod_data$SciName)])
  }
  
  # Merge on habitat information (found in clean_fao_prod) onto hs_taxa_match (primary)
  hs_taxa_match <- hs_taxa_match %>%
    left_join(sciname_habitat,
              by = c("SciName"))
  
  # Filter down to species level taxa matches (NOT SPECIES GROUPS) (secondary)
  tmp <- hs_taxa_match %>%
    distinct() %>%
    filter(str_detect(SciName, " "))
  
  # Calculate percent marine and inland by Code (secondary)
  # There shouldn't be any NAs in habitat information
  tmp <- tmp %>%
    group_by(Code, habitat) %>%
    tally() %>%
    rename(habitat_count = n) %>%
    ungroup() %>%
    group_by(Code) %>%
    mutate(total = sum(habitat_count)) %>%
    ungroup() %>%
    mutate(habitat_percent = 100 * habitat_count / total) %>%
    group_by(Code, habitat) %>%
    summarize(habitat_percent = sum(habitat_percent, na.rm = TRUE)) %>%
    pivot_wider(names_from = habitat, 
                values_from = habitat_percent) %>%
    replace_na(list(marine = 0, inland = 0, diadromous = 0))
  
  # Classify Codes to accept habitats where at least one true species matched with that habitat  (secondary)
  
  habitat_threshold <- 0 
  tmp <- tmp %>%
    mutate(habitat_classification = "") %>%
    # if there are any species of a habitat add this habitat to the code's habitat classification
    mutate(habitat_classification = case_when(
      inland > habitat_threshold ~ paste(habitat_classification, "inland", sep = "."),
      TRUE ~ habitat_classification
    )) %>%
    mutate(habitat_classification = case_when(
      marine > habitat_threshold ~ paste(habitat_classification, "marine", sep = "."),
      TRUE ~ habitat_classification
    )) %>%
    mutate(habitat_classification = case_when(
      diadromous > habitat_threshold ~ paste(habitat_classification, "diadromous", sep = "."),
      TRUE ~ habitat_classification
    )) %>%
    # cleaning up initial "." at the beginning of the habitat classification string
    mutate(habitat_classification = substr(habitat_classification, 2, str_length(habitat_classification)))
  
  # Merge code-habitat classifications
  hs_taxa_match <- hs_taxa_match %>%
    left_join(tmp %>%
                select(Code, habitat_classification),
              by = c("Code")) %>%
    mutate(habitat_test = case_when(
      str_detect(habitat_classification, habitat) ~ 1,
      habitat == "diadromous" ~ 1,
      str_detect(SciName, " ") == 0 ~ 1,
      TRUE ~ 0
    )) %>%
    # Use code habitat designations to remove genus and higher level matches where habitat doesnt match the code habitat (primary)
    filter(habitat_test == 1) %>%
    select(-c(Fresh01, Brack01, Saltwater01))
  
  diadromous_codes <- hs_taxa_match %>%
    filter(habitat_classification == "diadromous") %>%
    filter(habitat != "diadromous")
  
  if (nrow(diadromous_codes) > 0) {
    warning("Non diadromous species going into diadromous only codes")
  }
  
  marine_codes <- hs_taxa_match %>%
    filter(habitat_classification == "marine") %>%
    filter(habitat != "marine" & habitat != "diadromous")
  
  if (nrow(marine_codes) > 0) {
    warning("Non marine or diadromous species going into marine codes")
  }
  
  inland_codes <- hs_taxa_match %>%
    filter(habitat_classification == "inland") %>%
    filter(habitat != "inland" & habitat != "diadromous")
  
  if (nrow(inland_codes) > 0) {
    warning("Non inland or diadromous species going into marine codes")
  }
  
  # Check: All sciname-habitat combinations have been matched to at least one code
  taxa_habitat_prod <- sciname_habitat %>%
    mutate(taxa_habitat = paste(SciName, habitat, sep = "_")) %>%
    filter(str_detect(SciName, " "))
  
  hs_taxa_habitat_check <- hs_taxa_match %>%
    mutate(taxa_habitat = paste(SciName, habitat, sep = "_")) %>%
    filter(str_detect(SciName, " "))
  
  if (sum(!(unique(taxa_habitat_prod$taxa_habitat) %in% unique(hs_taxa_habitat_check$taxa_habitat)))) {
    warning("NOT ALL SciName habitat combinations match to an HS code")
    print("Missing SciName habitat combos")
    print(unique(taxa_habitat_prod$taxa_habitat)[!(unique(taxa_habitat_prod$taxa_habitat) %in% unique(hs_taxa_habitat_check$taxa_habitat))])
  }
  
  # Check all higher order taxa names, (NOT TRUE species) have a habitat classification
  higher_order_taxa_habitat <- hs_taxa_match %>%
    filter(!str_detect(SciName, " "),
           is.na(habitat_classification))
  
  if (nrow(higher_order_taxa_habitat) > 0) {
    warning("NOT ALL higher order taxa names have a habitat classification")
  }
  
  # Checking that all SciNames in production have been matched to an HS code
  if (sum(!(unique(prod_data$SciName) %in% unique(hs_taxa_match$SciName))) > 0) {
    warning("NOT ALL SciNames matched to HS codes")
    print("missing SciNames")
    print(unique(prod_data$SciName)[!(unique(prod_data$SciName) %in% unique(hs_taxa_match$SciName))])
  }
  
  # Checking every HS code has at least 1 SciName
  if (nrow(hs_taxa_match %>% group_by(Code) %>% tally() %>% filter(n == 0)) > 0) {
    warning("NOT EVERY HS code matched to at least one SciName")
  }
  
  hs_taxa_match <- hs_taxa_match %>%
    select(-habitat_test) %>%
    rename(sciname_habitat = habitat, code_habitat = habitat_classification)
  
  # if (test) {
  #   hs_taxa_match <- hs_taxa_match %>%
  #     filter(Code %in% test_codes)
  # }
  
  # SAVE HS TAXA MATCH OUTPUT:
  fwrite(hs_taxa_match, file = file.path(outdir, 
                                         paste0("hs-taxa-match_", HS_version_i, ".csv")), 
         row.names = FALSE)
  
  
  # Determine which HS codes can be processed and turned into another HS code
  hs_hs_match <- match_hs_to_hs(hs_taxa_match = hs_taxa_match,
                                hs_version = HS_version_i,
                                prod_taxa_classification) #Can use any HS code year
  
  # SAVE HS TO HS MATCH OUTPUT
  hs_hs_match_file <- paste0("hs-hs-match_", HS_version_i, ".csv")
  # Change column names to align with EU terminology
  # Remove columns for taxa list
  hs_hs_output <- hs_hs_match %>%
    select(-c(Taxa_full, Taxa_pre, Taxa_list, Taxa_post, Taxa_identical, Taxa_shared)) %>%
    rename(State_pre = Prep_pre,
           State_post = Prep_post,
           State_test = Prep_test,
           Presentation_pre = Sep_pre,
           Presentation_post = Sep_post,
           Presentation_test = Sep_test)
  
  hs_hs_output <- hs_hs_output %>%
    # Attaching Previous Code's habitat
    left_join(
      hs_taxa_match %>%
        select(Code_pre = Code, pre_code_habitat = code_habitat) %>%
        unique(),
      by = c("Code_pre")
    ) %>%
    # Attaching Processed Code's habitat
    left_join(
      hs_taxa_match %>%
        select(Code_post = Code, post_code_habitat = code_habitat) %>%
        unique(),
      by = c("Code_post")
    ) %>%
    # Only allowing hs hs matching if they share at least one habitat (inland/marine/diadromous) between codes
    filter(
      (str_detect(pre_code_habitat, "diadromous") & str_detect(post_code_habitat, "diadromous")) |
        (str_detect(pre_code_habitat, "inland") & str_detect(post_code_habitat, "inland")) |
        (str_detect(pre_code_habitat, "marine") & str_detect(post_code_habitat, "marine"))) %>%
    select(-c(pre_code_habitat, post_code_habitat))
  
  fwrite(hs_hs_output, file = file.path(outdir, hs_hs_match_file), row.names = FALSE)
  
  # Load and clean the live weight conversion factor data
  # These CF's convert from commodity to the live weight equivalent (min value is therefore 1, for whole fish)
  set_match_criteria = "strict"
  hs_taxa_CF_match <- compile_cf(
    conversion_factors = fread(file.path(datadir, "seafood_conversion_factors.csv"),
                               stringsAsFactors = FALSE),
    eumofa_data = fread(file.path(datadir, "EUMOFA_compiled.csv"), 
                        stringsAsFactors = FALSE),
    hs_hs_match,
    hs_version_i,
    match_criteria = set_match_criteria,
    fb_slb_dir = current_fb_slb_dir)
  
  # Check that everything in HS taxa match has a conversion factor value
  hs_taxa_matches <- hs_taxa_match %>%
    mutate(taxa_matches = paste(Code, SciName)) %>%
    select(taxa_matches) %>%
    distinct() %>%
    pull(taxa_matches)
  
  cf_matches <- hs_taxa_CF_match %>%
    mutate(cf_matches = paste(Code, Taxa)) %>%
    select(cf_matches) %>%
    distinct() %>%
    pull(cf_matches)
  
  if (sum(!(hs_taxa_matches %in% cf_matches)) > 0) {
    warning('NOT all hs taxa matches have a conversion factor value')
  }
  
  if (sum(!(cf_matches %in% hs_taxa_matches)) > 0) {
    warning('NOT all hs taxa matches have a conversion factor value')
  }
  
  
  # SAVE CONVERSION FACTORS OUTPUT
  cf_csv_name <- paste0("hs-taxa-CF_", set_match_criteria, 
                        "-match_", HS_version_i, ".csv")
  fwrite(hs_taxa_CF_match, file.path(outdir, cf_csv_name), row.names = FALSE)
  
} # end of HS version loop for HS codes and cfs



# BACI filter and standardize ------------------------------------------------

# hs_yr_combo_df with all hs year and analysis year combinations created in setup

if (test) {
  hs_yr_combo_df <- hs_yr_combo_df %>%
    filter(HS_version == test_hs,
           analysis_year == test_year)
}

# Load data file and filter for fish codes (i = exporter, j = importer, hs6 = HS code)
# Note on warning message "Some values were not matched unambiguously: NULL" means all values were matched

# filter baci to relevant products with quantity data available and add country details
for (i in 1:nrow(hs_yr_combo_df)){
  HS_version_i <- hs_yr_combo_df[i,]$HS_version
  analysis_year_i <- hs_yr_combo_df[i,]$analysis_year
  
  # Only run if filtered BACI file does not exist
  if (!file.exists(file.path(baci_filtered_dir, 
                             paste0("filtered_BACI_HS", HS_version_i, "_Y", 
                                    analysis_year_i, "_V", baci_version, 
                                    ".csv")))) {
    
    # read in individual baci (e.g. BACI_HS96_V202101/BACI_HS96_Y2021_V202101.csv)
    baci_data_i <- fread(
      file = file.path(baci_raw_dir, 
                       paste0("BACI_HS", HS_version_i, "_V", baci_version),
                       paste0("BACI_", "HS", HS_version_i, "_Y", analysis_year_i, 
                              "_V", baci_version, ".csv")),
      stringsAsFactors = FALSE)
    
    # FIXIT: this can be inside load_baci function?
    baci_data_i  <- baci_data_i  %>%
      mutate(q = as.numeric(q)) %>%
      # NAs should only arise when q is "           NA" (whitespace included)
      filter(!is.na(q))
    
    # join country info from `country_codes_V[release-version].csv` 
    baci_data_i <- load_baci(
      baci_data_i,
      hs_codes = as.numeric(unique(hs_data_clean$Code)),
      baci_country_codes = fread(file.path(baci_raw_dir, 
                                           paste0("BACI_HS", HS_version_i, "_V", 
                                                  baci_version),
                                           paste0("country_codes_V", 
                                                  baci_version, ".csv"))
                                 )
      )
    # write out filtered baci data
    fwrite(baci_data_i, 
           file.path(baci_filtered_dir, 
                     paste0("filtered_BACI_", "HS", HS_version_i, "_Y", 
                            analysis_year_i, "_V", baci_version, ".csv")),
           row.names = FALSE)
    
    message(paste0("HS",HS_version_i, " ", analysis_year_i, 
                   " filtered BACI file created"))
    
  } else {
    print("Filtered BACI file already exists")
    }
  } 

# Standardize BACI countries 
for (i in 1:nrow(hs_yr_combo_df)){
  HS_version_i <- hs_yr_combo_df[i,]$HS_version
  analysis_year_i <- hs_yr_combo_df[i,]$analysis_year
  #print(paste(HS_version, analysis_year))
  
  # read in filtered BACI data created above
  baci_data_i <- fread(
    file.path(baci_filtered_dir, 
              paste0("filtered_BACI_", "HS", HS_version_i, "_Y", analysis_year_i, "_V", 
                     baci_version, ".csv")))
  
  # add year and hs_version columns
  baci_data_i <- baci_data_i %>%
    mutate(year = analysis_year_i,
           hs_version = paste0("HS", HS_version_i))
  
  # Standardize countries
  baci_data_i <- standardize_countries(df = baci_data_i, 
                                       data_source = "BACI")
  
  # BACI output used to generate ARTIS (keeps legacy dataframe format)
  fwrite(
    baci_data_i %>%
      select(-c(total_v)),
    file.path(outdir, paste0("standardized_baci_seafood_hs", HS_version_i, "_y", 
                            analysis_year_i, ".csv")),
    row.names = FALSE
  )

  # BACI output with total and unit value
  fwrite(
    baci_data_i,
    file.path(outdir, paste0("standardized_baci_seafood_hs", HS_version_i, "_y", 
                             analysis_year_i, "_including_value.csv")),
    row.names = FALSE
  )
} # end of standardize baci data loop

# Clean FAO population data --------------------------------------------------
pop_raw <- fread(file.path(datadir, "Population_E_All_Data/Population_E_All_Data_NOFLAG.csv"))

clean_pop <- pop_raw %>%
  # Total population all inclusive
  filter(Element == "Total Population - Both sexes") %>%
  # Remove Regional Summaries to avoid double counting
  filter(Area.Code < 1000) %>%
  # Remove unnecessary columns
  select(-c("Area.Code", "Area.Code..M49.", "Item.Code", "Item", "Element.Code", "Element", "Unit")) %>%
  rename(country_name = Area) %>%
  # Structure table so years and values are in 2 columns
  pivot_longer(cols = -c(country_name), names_to = "year", values_to = "pop") %>%
  # Format year correctly
  mutate(year = substr(year, 2, 5)) %>%
  mutate(year = as.numeric(year)) %>%
  # Convert population estimate from 1000 persons to raw pop count
  mutate(pop = 1000 * pop) %>%
  # Filter for years included in ARTIS
  filter(year >= 1996 & year <= 2022)

Encoding(clean_pop$country_name) <- "latin1"

clean_pop <- clean_pop %>%
  # Note double counting occuring for China = China mainland + China Macao + China, Taiwan Province of + China Hong Kong
  filter(country_name != "China",
         country_name != "Yugoslav SFR",
         country_name != "Czechoslovakia",
         country_name != "Pacific Islands Trust Territory") %>%
  mutate(country_name = case_when(
    country_name == "China, mainland" ~ "China",
    country_name == "China, Hong Kong SAR" ~ "China",
    country_name == "China, Macao SAR" ~ "China",
    country_name == "China, Taiwan Province of" ~ "Taiwan",
    country_name == "Netherlands Antilles (former)" ~ "Netherlands",
    country_name == "Türkiye" ~ "Turkey",
    country_name == "Belgium-Luxembourg" ~ "Belgium",
    country_name == "Luxembourg" ~ "Belgium",
    TRUE ~ country_name
  )) %>%
  mutate(
    iso3c = countrycode(country_name, "country.name", "iso3c")
  ) %>%
  mutate(
    iso3c = case_when(
      country_name == "Serbia and Montenegro" ~ "SCG",
      TRUE ~ iso3c
    )
  )

# Standardizing Countries
clean_fao <- fread(file.path(datadir, "standard_fao_countries.csv"))
clean_pop <- clean_pop %>%
  filter(year <= 2020) %>% #FIXIT: does this need updating? make dynamic with setup variable
  left_join(
    clean_fao,
    by = c("iso3c", "year")
  ) %>%
  select(-c(country_name, iso3c)) %>%
  rename(iso3c = artis_iso3c, country_name = artis_country_name) %>%
  group_by(iso3c, year) %>%
  summarize(pop = sum(pop, na.rm = TRUE))

if (test) {
  clean_pop <- clean_pop %>%
    filter(year == test_year)
}

fwrite(clean_pop, file.path(outdir, "fao_annual_pop.csv"), row.names = FALSE)


