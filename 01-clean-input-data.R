# Clean raw input data


# Setup -----------------------------------------------------------

# Clear environment 
rm(list=ls())

# load packages
library(artis)
library(rfishbase)
library(data.table)
library(magrittr)
library(glue)
library(countrycode)
library(stringr)
library(dplyr)
library(tidyr)
library(cli)

# Run Local Machine Configuration (directory paths, parameters)
source("00-local-machine-setup.R")

# FishBase & SeaLifeDase Data ------------------------------------------------------
# Collect new fishbase and sealifebase data files with rfishbase package wrapped in artis::collect_fb_slb_data function
if(need_new_fb_slb == TRUE) {
  current_fb_slb_dir <- artis::collect_fb_slb_data(parent_outdir = datadir_raw)
  message(glue("New fishbase and sealifebase data collected at {current_fb_slb_dir}"))
} else {
  # Or use most recent existing fishbase and sealifebase data files
  current_fb_slb_dir <- list.dirs(datadir_raw, full.names = TRUE, recursive = FALSE) %>%
    stringr::str_subset("fishbase_sealifebase_") %>%
    sort(decreasing = TRUE) %>%
    .[1]
  # Check if current_fb_slb_dir is valid
  if (is.na(current_fb_slb_dir)) {
    cli::cli_abort(c(
      "x" = "No fishbase_sealifebase directory found in {.file {datadir_raw}}",
      "i" = "Set {.code need_new_fb_slb = TRUE} in 00-local-machine-setup.R to download new data",
      "i" = "OR ensure a fishbase_sealifebase_* directory exists"
    ))
  } 
  if (dir.exists(current_fb_slb_dir)) {
    cli::cli_alert_success(
      "Using existing FB and SLB data at {.file model_inputs_raw_{artis_version}/{basename(current_fb_slb_dir)}/}"
    )
  }
}

# FAO Production Data -------------------------------

# Read in raw FAO production data files and restructure into standard format with
# `rebuild_fao_[yyyy]_dat` function
# Directly fed into the prod_df arguement of classify_prod_dat function to save memory.
# Note: rebuild_fao_[yyyy]_dat function works for zipped and unzipped files
# FAO files are not always consistent schemaq, so multiple version exist of the function
# to account for these differences.

rebuilt_fao_prod <- rebuild_fao_2023_dat(
  datadir = datadir_raw,
  filename = "GlobalProduction_2025.1.0.zip"
) %>%
  # only keep data from 1996 onward and where quantity > 0
  filter(year >= 1996, quantity > 0)

# FAO Clean Taxa and Classification ---------------------------

prod_list <- artis::classify_prod_dat(
  datadir = datadir_raw,
  prod_data_source = "FAO",
  prod_df = rebuilt_fao_prod,
  fb_slb_dir = current_fb_slb_dir
)

# Reassign list to separate objects - basis of prod and taxa files
prod_data_raw <- prod_list[[1]] 
prod_taxa_classification <- prod_list[[2]]

# remove large less-clean environmental objects no longer needed
rm(prod_list, rebuilt_fao_prod)

## FAO Taxa Manual Habitat Adds ---------------------------------
prod_taxa_classification <- prod_taxa_classification %>%
  # Manually assign missing habitat coding
  mutate(
    Fresh01 = case_when(
      SciName %in% c("neocaridina denticulata", "caridina nilotica") ~
        as.integer(1),
      TRUE ~ as.integer(Fresh01)
    )
  ) %>%
  mutate(
    Saltwater01 = case_when(
      SciName == "anadara grandis" ~ as.integer(1),
      TRUE ~ as.integer(Saltwater01)
    )
  )

## DATA CHECK ---------------------------------
# Verify habitat information is complete
missing_habitat_species <- prod_taxa_classification %>% 
  mutate(habitat_sum = Fresh01 + Brack01 + Saltwater01) %>%
  filter(habitat_sum == 0 | is.na(habitat_sum))

if (nrow(missing_habitat_species) > 0) {
  cli::cli_alert_warning(c(
    "!" = "{nrow(missing_habitat_species)} species missing habitat information",
    "i" = "Species without habitat coding: {.val {missing_habitat_species$SciName}}",
    "i" = "Check prod_taxa_classification for Fresh01, Brack01, and Saltwater01 columns"
  ))
}

# SAVE PRODUCTION Taxa OUTPUT:
write.csv(prod_taxa_classification, file = file.path(datadir, "clean_fao_taxa.csv"), row.names = FALSE)

# Structure FAO prod for ARTIS -----------------------------------------------------

# Get fishbase habitat data from prod_taxa_classification to standardize habitat info in prod_data
prod_habitat <- prod_taxa_classification %>%
  select(SciName, Fresh01, Brack01, Saltwater01) %>%
  distinct()

# Filter down and restructure FAO production data for ARTIS
prod_data <- prod_data_raw %>%
  # Remove columns not needed for running ARTIS
  select(!c(any_of(c("alternate", "multiplier", "symbol", "symbol_identifier", 
                    "country_iso3_numeric", "country_identifier", "production_identifier", 
                    "sort", "unit_identifier")), # "species_identifier" still available here
            contains(c("_ar", "_cn", "_es", "_fr", "_ru")),
            CommonName)) %>%
  # clean up habitat and production method values
  mutate(fao_habitat = case_when(habitat == "Inland waters" ~ "inland",
                             habitat == "Marine areas" ~ "marine",
                             TRUE ~ habitat),
         prod_method = case_when(prod_method %in% c("FRESHWATER", "MARINE", "BRACKISHWATER") ~ "aquaculture",
                                 prod_method == "CAPTURE" ~ "capture",
                                 TRUE ~ prod_method)) %>%
  # Create new column that combines SciName with souce info (i.e., habitat + production method)
  mutate(taxa_source = paste(str_replace(SciName, " ", "."), fao_habitat, prod_method, sep = "_")) %>%
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
  mutate(taxa_source = paste(str_replace(SciName, " ", "."), habitat, prod_method, sep = "_")) 

write.csv(prod_data, file = file.path(datadir, "clean_fao_prod.csv"), row.names = FALSE)

## Attribute Table ISSCAAP ---------------------------------------------------------
# used to create code_max_resolved which is used in ARTIS calculate_consumption
# requires prod_data with isscaap_group column

build_attr_isscaap(prod_fao = prod_data, output_dir = outdir_attribute)

## Aggregate data down to ARTIS columns ----------------
prod_data <- prod_data %>% 
  group_by(SciName, year, taxa_source, habitat, prod_method, country_iso3_alpha, country_name_en, area.code) %>%
  summarize(quantity = sum(quantity, na.rm = TRUE)) %>%
  ungroup()

## If Running Test ---------------------------------
# `test <- TRUE` in 00-local-machine-setup.R config file
if (test) {
  
  prod_data <- prod_data %>%
    filter(year == test_year) %>%
    filter(SciName %in% test_scinames)
  
  prod_taxa_classification<- prod_taxa_classification %>%
    filter(SciName %in% test_scinames)
}

# FAO Standardize Countries ------------------------------
prod_data <- standardize_countries(df = prod_data, 
                                   data_source = "FAO")

# Write Prod (more columns)
# retain FAO area.code column
write.csv(prod_data, file = file.path(datadir, "standardized_fao_prod_more_cols.csv"), row.names = FALSE)

# Write Prod (ARTIS)
# remove area.code column to format to prod version used in model
prod_data <- prod_data %>% 
  group_by(SciName, year, taxa_source, habitat, prod_method, country_iso3_alpha, country_name_en) %>%
  summarize(quantity = sum(quantity, na.rm = TRUE)) %>%
  ungroup()

write.csv(prod_data, file = file.path(datadir, "standardized_fao_prod.csv"), row.names = FALSE)

# SAU Production Data -------------------------
## SAU Clean Taxa and Classification ------------------------------
prod_list_sau <- classify_prod_dat(datadir = datadir_raw,
                                   prod_data_source = "SAU", # Don't change for FAO model run
                                   prod_df = fread(file.path(datadir_raw, "SAU_Production_Data.csv"), 
                                                    stringsAsFactors = FALSE, 
                                                    header = TRUE, 
                                                    sep = ",", 
                                                    data.table = FALSE),
                                   SAU_sci_2_common = "TaxonFunctionalCommercial_Clean.csv",
                                   fb_slb_dir = current_fb_slb_dir)

# split list into two data objects: 1/2 production
prod_data_sau <- prod_list_sau[[1]] %>%
  mutate(
    year = as.numeric(year),
    quantity = as.numeric(quantity))

# Add habitat, prod_method, and taxa_source columns
prod_data_sau <- prod_data_sau %>%
  # All SAU data is marine capture
  mutate(habitat = "marine",
         prod_method = "capture") %>%
  mutate(taxa_source = paste(str_replace(SciName, " ", "."), 
                             habitat, prod_method, sep = "_"))

# split list into two data objects: 2/2 taxa classification
prod_classification_sau <- prod_list_sau[[2]]

prod_data_sau <- prod_data_sau %>%
  # Add columns to adhear to FAO format
  mutate(Fresh01 = 0, Saltwater01 = 1, Brack01 = 0)

# remove large object from environment memory
rm(prod_list_sau)

# Write Clean SAU Taxa 
write.csv(prod_classification_sau, file.path(datadir, "clean_sau_taxa.csv"), 
           row.names = FALSE)

# SAU Standardize Countries --------------------------------------------------
# This code will be represented in the new `standardize_countries()` and `standardize_country_data()` functions
# Not completed yet. https://github.com/Seafood-Globalization-Lab/artis-model/issues/57

# Pre Country Cleaning
prod_data_sau <- prod_data_sau %>%
  mutate(country_name_en = str_remove(country_name_en, ' \\(.+\\)$')) %>%
  # use countrycode package to translate names into iso3c codes
  mutate(country_iso3_alpha = countrycode::countrycode(country_name_en, origin = 'country.name', destination = 'iso3c')) %>%
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
  mutate(country_iso3_numeric = countrycode::countrycode(country_iso3_alpha, 
                                            origin = 'iso3c', 
                                            destination = 'iso3n'))

# standardize countries for SAU production (options are FAO or BACI, FAO correct for SAU)
prod_data_sau <- standardize_countries(prod_data_sau, "FAO")

# condense / aggregate data down to select columns
prod_data_sau <- prod_data_sau %>% 
  group_by(SciName, year, taxa_source, habitat, prod_method, country_iso3_alpha, 
           country_name_en, gear, eez, sector, end_use) %>% 
  summarise(quantity = sum(quantity), .groups = "drop") 

# Write SAU Prod (more columns) 
write.csv(prod_data_sau, file.path(datadir, 'standardized_sau_prod_more_cols.csv'), 
          row.names = FALSE)

# condense / aggregate data down to select columns
prod_data_sau <- prod_data_sau %>% 
    group_by(SciName,  year, taxa_source, habitat, prod_method, 
             country_iso3_alpha, country_name_en) %>% 
    summarise(quantity = sum(quantity), .groups = "drop") 

# Write SAU Prod (ARTIS) 
write.csv(prod_data_sau, file.path(datadir, 'standardized_sau_prod.csv'), row.names = FALSE)

# If Running SAU Model Mode -----------------------------------------------------
## Replace FAO prod marine capture with SAU prod (Marine Capture) --------------------------------
if (running_sau) {
  # Remove FAO production marine capture data - Add in SAU prod
  prod_data <- prod_data %>%
    filter(!(habitat == 'marine' & prod_method == 'capture')) %>%
    bind_rows(prod_data_sau)
  
  # Write Combined Prod
  write.csv(prod_data, file.path(datadir, 'standardized_combined_prod.csv'),
            row.names = FALSE)
  
## Combine FAO and SAU Taxa --------------------------------
  prod_taxa_classification <- prod_taxa_classification %>%
    bind_rows(prod_classification_sau) %>%
    distinct() %>%
    filter(SciName %in% unique(prod_data$SciName))
  
  # write combined clean taxa
  write.csv(prod_taxa_classification, 
            file.path(datadir, "clean_taxa_combined.csv"),
            row.names = FALSE)
} # End of if running_sau == TRUE


# Prep for Matching and CF Funs ------------------------------------------

## Create Habitat Info --------------------------------
# FIXIT: Add test Make sure that prod taxa classification is classified to at least one of Species, Genus, Family, Other
# FIXIT: Can this be added to sciname table? This habitat info isn't saved out anywhere else currently.
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

## Clean HS codes and descriptions --------------------------------
# Load and clean the conversion factor data and run the matching functions. 
# This data will be used to create V1 and V2. 

hs_data_raw <- fread(file.path(datadir_raw, "All_HS_Codes.csv"), colClasses = "character", data.table = FALSE)

hs_data_clean <- clean_hs(hs_data_raw = hs_data_raw,
                          fb_slb_dir = current_fb_slb_dir)

## FMFO Species from SAU --------------------------------------------------
# Getting list of fmfo species
fmfo_species <- get_fmfo_species(
  sau_fp = file.path(datadir, 'standardized_sau_prod_more_cols.csv'),
  fishmeal_min_threshold_sp = 1,
  fishmeal_min_threshold_global = 0.5,
  fishmeal_primary_threshold = 75
)

write.csv(fmfo_species, file.path(datadir, 'fmfo_species_list.csv'), row.names = FALSE)

# Get HS years for data cleaning loops below
HS_year <- unique(df_years$HS_year)

if (test) {
  HS_year <- HS_year[HS_year %in% test_hs]
  hs_data_clean <- hs_data_clean %>%
    filter(Code %in% test_codes)
}

# Matching Funs and CFs (HS version loop) --------------------------------

# No existing file check in place. These files need to be regenerated each time a change is made to prod or taxa data. 

for(i in 1:length(HS_year)) {
  
  # define HS version
  hs_version <- paste("HS", HS_year[i], sep = "")
  cli_alert_info("{.field {hs_version}} Matching taxa/products/conversion factors")
  
  ## match_hs_to_taxa --------------------------------
  # Match HS codes to production taxa (can be FAO or SAU depending on which was used in clean_and_clasify_prod_dat function)
  hs_taxa_match <- match_hs_to_taxa(hs_data_clean = hs_data_clean,
                                    prod_taxa_classification = prod_taxa_classification,
                                    # species used for FMFO globally based on SAU data
                                    fmfo_species_list = fmfo_species,
                                    hs_version = hs_version)
  
  ### DATA CHECK ---------------------------------
  # Check that all species in hs taxa match are in the production data
  missing_species <- unique(hs_taxa_match$SciName)[!unique(hs_taxa_match$SciName) %in% unique(prod_data$SciName)]

  if (length(missing_species) > 0) {
    cli::cli_warn(c(
      "!" = "{length(missing_species)} species in HS taxa match are not in production data",
      "i" = "Missing species: {.val {missing_species}}",
      "i" = "Check HS taxa matching process or production data completeness"
    ))
  }
  
  hs_taxa_match <- add_habitat_classifications(
    hs_taxa_match = hs_taxa_match,
    sciname_habitat = sciname_habitat,
    prod_data = prod_data,
    hs_version = hs_version
  )

  # Save HS to Taxa Match Output
  write.csv(hs_taxa_match, 
    file = file.path(datadir, paste("hs-taxa-match_", hs_version, ".csv", sep="")), 
    row.names = FALSE)
  
  # Determine which HS codes can be processed and turned into another HS code
  hs_hs_match <- match_hs_to_hs(hs_taxa_match = hs_taxa_match,
                                hs_version = hs_version,
                                prod_taxa_classification) #Can use any HS code year
  
  # SAVE HS TO HS MATCH OUTPUT
  hs_hs_match_file <- paste("hs-hs-match_", hs_version, ".csv", sep="")
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
        (str_detect(pre_code_habitat, "marine") & str_detect(post_code_habitat, "marine"))
    ) %>%
    select(-c(pre_code_habitat, post_code_habitat))
  
  write.csv(hs_hs_output, file = file.path(datadir, hs_hs_match_file), row.names = FALSE)
  
  ## compile_cf --------------------------------
  # Load and clean the live weight conversion factor data
  # These CF's convert from commodity to the live weight equivalent (min value is therefore 1, for whole fish)
  set_match_criteria = "strict" # FIXIT: AM 2025-08 this parameter can be moved to 00-local-setup.R
  hs_taxa_CF_match <- compile_cf(conversion_factors = read.csv(file.path(datadir_raw, "seafood_conversion_factors.csv"), stringsAsFactors = FALSE),
                                 eumofa_data = read.csv(file.path(datadir_raw, "EUMOFA_compiled.csv"), stringsAsFactors = FALSE),
                                 hs_hs_match,
                                 hs_version,
                                 match_criteria = set_match_criteria,
                                 fb_slb_dir = current_fb_slb_dir)
  
  ##### Data Check ######
  # Check that everything in HS taxa match has a conversion factor value
  hs_taxa_matches <- hs_taxa_match %>%
    mutate(taxa_matches = paste(Code, SciName, sep = "_")) %>%
    select(taxa_matches) %>%
    distinct() %>%
    pull(taxa_matches)
  
  cf_matches <- hs_taxa_CF_match %>%
    mutate(cf_matches = paste(Code, Taxa, sep = "_")) %>%
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
  cf_csv_name <- paste("hs-taxa-CF_", set_match_criteria, "-match_", hs_version, ".csv", sep = "")
  write.csv(hs_taxa_CF_match, file.path(datadir, cf_csv_name), row.names = FALSE)
  
} # end loop - taxa and hs matching 


# BACI filter and standardize --------------------------------------------

# Load trade (BACI) data, filter to just seafood products, and standardize countries between production and trade data
# Create data frame with all hs year and analysis year combinations

# df_years dataframe contains all HS version and year pairs. Created in 00-local-machine-setup.R

# Existing file check in place - filtering and standardizing BACI is independent of other steps so files only need to be created once.

if (test) {
  df_years <- df_years %>%
    filter(HS_year == test_hs,
           analysis_year == test_year)
}

# Load data file and filter for fish codes (i = exporter, j = importer, hs6 = HS code)
# Note on warning message "Some values were not matched unambiguously: NULL" means all values were matched

## Filter Raw BACI Data ----------------------------------
for (i in 1:nrow(df_years)){
  a_HS_year <- df_years[i,]$HS_year
  analysis_year <- df_years[i,]$analysis_year
  
  # Creating out folder if necessary
  if (!file.exists(file.path(datadir_raw, paste("filtered_BACI_", "HS", a_HS_year, "_Y", analysis_year, "_V", baci_version, ".csv", sep = "")))) {
    
    message(glue("Filter raw BACI HS{a_HS_year} {analysis_year}"))
    baci_data_i <- read.csv(file = file.path(tradedatadir, 
                                           paste("BACI_", "HS", a_HS_year, "_V", baci_version, sep = ""),
                                           paste("BACI_", "HS", a_HS_year, "_Y", analysis_year, "_V", baci_version, ".csv", sep = "")),
                          stringsAsFactors = FALSE)
    
    baci_data_i  <- baci_data_i  %>%
      mutate(q = as.numeric(q)) %>%
      # NAs should only arise when q is "           NA" (whitespace included)
      filter(!is.na(q))
    
    baci_data_i  <- load_baci(
      baci_data_i ,
      hs_codes = as.numeric(unique(hs_data_clean$Code)),
      baci_country_codes = read.csv(file.path(tradedatadir, 
                                              paste("BACI_", "HS", a_HS_year, "_V", baci_version, sep = ""),
                                              paste("country_codes_V", baci_version, ".csv", sep = "")))
    )
    
    write.csv(baci_data_i, file.path(datadir_raw, paste("filtered_BACI_", "HS", a_HS_year, "_Y", analysis_year, "_V", baci_version, ".csv", sep = "")),
              row.names = FALSE)
  } else {
    print(glue("Filtered BACI HS{a_HS_year} {analysis_year} file Exists. Skipping to next HS/year pair."))
    }
  } 

## Standardize BACI data ----------------------------------
for (i in 1:nrow(df_years)){
  a_HS_year <- df_years[i,]$HS_year
  analysis_year <- df_years[i,]$analysis_year

  if (!file.exists(file.path(datadir, glue("standardized_baci_seafood_hs{a_HS_year}_y{analysis_year}.csv")))) {

    print(glue("standardize BACI {a_HS_year} {analysis_year}"))
    
    baci_data <- read.csv(file.path(datadir_raw, glue("filtered_BACI_HS{a_HS_year}_Y{analysis_year}_V{baci_version}.csv")))
    
    baci_data <- baci_data %>%
      mutate(year = analysis_year,
            hs_version = paste("HS", a_HS_year, sep = ""))
    
    baci_data <- standardize_countries(baci_data, "BACI")
    
    # BACI output used to generate ARTIS (keeps legacy dataframe format)
    write.csv(
      baci_data %>%
        select(-c(total_v)),
      file.path(datadir, paste("standardized_baci_seafood_hs", a_HS_year, "_y", analysis_year, ".csv", sep = "")),
      row.names = FALSE
    )
    # BACI output with total and unit value
    write.csv(
      baci_data,
      file.path(datadir, paste("standardized_baci_seafood_hs", a_HS_year, "_y", analysis_year, "_including_value.csv", sep = "")),
      row.names = FALSE
    )
  } else {
      print(glue("Standardized BACI HS{a_HS_year} {analysis_year} file Exists. Skipping to next HS/year pair."))
    }
} # end of BACI Loop

# Clean FAO population data ------------------------------------------------
# FIXIT: add unzip step if file not unzipped yet
pop_raw <- read.csv(file.path(datadir_raw, "Population_E_All_Data/Population_E_All_Data_NOFLAG.csv"))

# Restructure FAO population data
clean_pop <- pop_raw %>%
  # Total population all inclusive
  filter(Element == "Total Population - Both sexes") %>%
  # Remove Regional Summaries to avoid double counting
  filter(Area.Code < 1000) %>%
  # Use area codes to correct names with special characters
  mutate(Area = case_when(
    Area.Code == 107 ~ "Cote d'Ivoire",
    Area.Code == 223 ~ "Turkey",
    Area.Code == 279 ~ "Curacao",
    Area.Code == 182 ~ "Reunion",
    Area.Code == 282 ~ "Saint Barthelemy",
    TRUE ~ Area
  )) %>%
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
  filter(year >= 1996 & year <= max_year) # FIXIT add upper year bounds dynamic

Encoding(clean_pop$country_name) <- "latin1"

# Clean FAO population data
clean_pop <- clean_pop %>%
  # Note double counting occuring for China = China mainland + China Macao + China, Taiwan Province of + China Hong Kong
  filter(country_name != "China",
         country_name != "Yugoslav SFR",
         country_name != "Czechoslovakia",
         country_name != "Pacific Islands Trust Territory") %>%
  filter(!is.na(pop)) %>%
  # Remove USSR from data - dissolved in 1992 and not relevant to ARTIS
  filter(country_name != "USSR") %>%
  mutate(country_name = case_when(
    country_name == "China, mainland" ~ "China",
    country_name == "China, Hong Kong SAR" ~ "China",
    country_name == "China, Macao SAR" ~ "China",
    country_name == "China, Taiwan Province of" ~ "Taiwan",
    # country dissolved in 2010 - pop data NA afterward, so existing data 
    # into "Netherlands (Kingdom of the)" 
    country_name == "Netherlands Antilles (former)" ~ "Netherlands (Kingdom of the)",
    country_name == "Türkiye" ~ "Turkey",
    # combine Belgium and Luxembourg because of population data gaps
    # among the three combinations. 
    country_name == "Belgium-Luxembourg" ~ "Belgium",
    country_name == "Luxembourg" ~ "Belgium",
    TRUE ~ country_name
  )) %>%
  mutate(
    iso3c = countrycode::countrycode(country_name, "country.name", "iso3c")
  ) %>%
  mutate(
    iso3c = case_when(
      country_name == "Serbia and Montenegro" ~ "SCG",
      TRUE ~ iso3c
    )
  )

# Standardizing Countries
# clean_fao <- read.csv(file.path(datadir_raw, "standard_fao_countries.csv"))
# clean_pop <- clean_pop %>%
#   left_join(
#     clean_fao,
#     by = c("iso3c", "year")
#   ) %>%
#   select(-c(country_name, iso3c)) %>%
#   rename(iso3c = artis_iso3c, country_name = artis_country_name) %>%
#   group_by(iso3c, year) %>%
#   summarize(pop = sum(pop, na.rm = TRUE))

std_pop <- standardize_prod(clean_pop, "iso3c", "country_name")
std_pop <- std_pop %>%
  select(
    iso3c = artis_iso3c, 
    year,
    pop) %>%
  group_by(iso3c, year) %>%
  summarise(pop = sum(pop), .groups = "drop")

# FIXIT: Add tests here. Adds up to the global population - no addtions or removal. Raw file and group by year and summarize global population. 

if (test) {
  std_pop <- std_pop %>%
    filter(year == test_year)
}

write.csv(std_pop, file.path(datadir, "fao_annual_pop.csv"), row.names = FALSE)

# Attribute Tables --------------------------------------------------------

## sciname --------------------------------------------------------

build_attr_sciname(
  fao_taxa_data = fread(file.path(datadir, "clean_fao_taxa.csv"), data.table = FALSE) %>%
    rename(sciname = SciName, common_name = CommonName) %>%
    distinct(),
  isscaap_attribute = fread(file.path(outdir_attribute, "isscaap_attribute.csv"), data.table = FALSE),
  running_sau = running_sau,
  sau_taxa_data = if (running_sau) {
    fread(file.path(datadir, "clean_taxa_combined.csv"), data.table = FALSE) %>% 
      rename(sciname = SciName, common_name = CommonName) %>%
      distinct()
  } else {
    NULL
  },
  write_dir = outdir_attribute
)

## Code_max_resolved_taxa -------------------------------------------------

hs_taxa_match <- data.frame(Code = integer(),
                            SciName = character(),
                            Match_category = character(),
                            HS_version = character(),
                            Description = character(),
                            Modification = character())

hs_clade_match <- data.frame(Code = character(),
                             hs_clade = factor(),
                             classification_level = character(),
                             hs_version = character())

if (running_sau) {
  taxa <- fread(file.path(datadir, "clean_taxa_combined.csv"), data.table = FALSE) %>%
    rename(sciname = SciName, common_name = CommonName) %>%
    distinct()
} else {
  taxa <- fread(file.path(datadir, "clean_fao_taxa.csv"), data.table = FALSE) %>%
    rename(sciname = SciName, common_name = CommonName) %>%
    distinct()
}

for(i in HS_year){
  HS_year_rep <- i
  
  hs_taxa_match_i <- read.csv(file.path(datadir, paste("hs-taxa-match_HS", HS_year_rep, ".csv", sep = "")))
  hs_clade_match_i <- match_hs_to_clade(hs_taxa_match = hs_taxa_match_i,
                                        prod_taxa_classification = taxa %>%
                                          rename(CommonName = common_name, SciName = sciname),
                                        match_to_prod = FALSE) 
  
  hs_clade_match_i <- hs_clade_match_i %>%
    mutate(Code = as.character(Code)) %>% # pad HS codes with zeroes
    mutate(
      Code = if_else(
        str_detect(Code, "^30"),
        true = str_replace(Code, pattern = "^30", replacement = "030"),
        if_else(
          str_detect(Code, "^511"),
          true = str_replace(Code, pattern = "^511", replacement = "0511"),
          false = Code
        )
      )
    ) %>%
    mutate(hs_version = HS_year_rep)
  

  hs_clade_match <- hs_clade_match %>%
    bind_rows(hs_clade_match_i) %>%
   # bind_rows(hs_clade_match_sau_i) %>%
    distinct()

  hs_taxa_match <- hs_taxa_match %>%
    bind_rows(hs_taxa_match_i) %>%
   # bind_rows(hs_taxa_match_sau_i) %>%
    distinct()
}

hs_clade_match <- hs_clade_match %>%
  rename("hs6" = "Code")

hs_clade_match <- hs_clade_match %>%
  mutate(hs6 = as.integer(hs6),
         hs_version = as.integer(hs_version)) %>%
  rename("code_taxa_level" = "classification_level")

taxa_metadata <- fread(file.path(outdir_attribute, "sciname_attribute.csv"), data.table = FALSE)

prod_taxa_classification <- taxa_metadata %>%
  select(-common_name) %>%
  unique() %>% 
  mutate(
    prod_taxa_level = case_when(
      (str_count(sciname, pattern = " ") == 1) ~ "Species", 
      sciname == Genus ~ "Genus", 
      sciname == Subfamily ~ "Subfamily", 
      sciname == Family ~ "Family", 
      sciname == Order ~ "Order", 
      sciname == Class ~ "Class", 
      sciname == Superclass ~ "Superclass", 
      sciname == Phylum ~ "Phylum", 
      sciname == Kingdom ~ "Kingdom"
    )
  ) %>%
  select(sciname, prod_taxa_level) %>%
  bind_rows(data.frame(
    sciname = c("animalia", "osteichthyes", "actinopteri"),
    prod_taxa_level = c("Kingdom", "Superclass", "Class")
  )) %>%
  distinct()

code_max_resolved_taxa <- hs_taxa_match %>%
  rename(hs6 = Code, sciname = SciName) %>%
  left_join(hs_clade_match %>% 
              mutate(hs_version = paste("HS", hs_version, sep="")),
            by = c("hs6", "HS_version" = "hs_version")) %>%
  left_join(prod_taxa_classification, by = c("sciname")) %>%
  mutate(code_taxa_level_numeric = case_when(
    code_taxa_level == "Species" ~ 1, 
    code_taxa_level == "Genus" ~ 2,
    code_taxa_level == "Subfamily" ~ 3,
    code_taxa_level == "Family" ~ 4,
    code_taxa_level == "Order" ~ 5, 
    code_taxa_level == "Class" ~ 6, 
    code_taxa_level == "Superclass" ~ 7,
    code_taxa_level == "Phylum" ~ 8,
    code_taxa_level == "Kingdom" ~ 9
  )) %>%
  mutate(prod_taxa_level_numeric = case_when(
    prod_taxa_level == "Species" ~ 1, 
    prod_taxa_level == "Genus" ~ 2,
    prod_taxa_level == "Subfamily" ~ 3,
    prod_taxa_level == "Family" ~ 4,
    prod_taxa_level == "Order" ~ 5, 
    prod_taxa_level == "Class" ~ 6,
    prod_taxa_level == "Superclass" ~ 7,
    code_taxa_level == "Phylum" ~ 8,
    prod_taxa_level == "Kingdom" ~ 9
  )) %>%
  mutate(hs_clade = as.character(hs_clade)) %>%
  # Find the most resolved name from production or trade hs codes? -AM
  mutate(sciname_hs_modified = case_when(
    prod_taxa_level_numeric < code_taxa_level_numeric ~ sciname, 
    code_taxa_level_numeric < prod_taxa_level_numeric ~ hs_clade,
    prod_taxa_level_numeric == code_taxa_level_numeric ~ sciname
  )) %>%
  mutate(sciname_hs_modified = ifelse(is.na(sciname_hs_modified), sciname, sciname_hs_modified)) %>%
  # Leave chordata as original names
  mutate(sciname_hs_modified = case_when(
    sciname_hs_modified == "chordata" ~ sciname_hs_modified,
    sciname_hs_modified != "chordata" ~ sciname_hs_modified
  )) %>%
  select("hs_version" = "HS_version", hs6, sciname, sciname_hs_modified, 
         "match_category" = "Match_category", "description" = "Description",
         "modification" ="Modification", hs_clade, code_taxa_level, prod_taxa_level,
         code_taxa_level_numeric, prod_taxa_level_numeric) %>%
  mutate(hs6 = as.character(hs6)) %>%
  distinct()

write.csv(code_max_resolved_taxa, file.path(datadir, "code_max_resolved_taxa.csv"), row.names = FALSE)
write.csv(code_max_resolved_taxa, file.path(outdir_attribute, "code_max_resolved_taxa.csv"), row.names = FALSE)


## Products ---------------------------------------------------------------
build_attr_products(
  datadir_raw = datadir_raw, 
  datadir = datadir, 
  outdir_attribute = outdir_attribute, 
  hs_raw_file = "All_HS_Codes.csv")
  

## Prod -------------------------------------------------------------------

# Cleaning FAO Production data for database

prod <- read.csv(file.path(datadir, "standardized_fao_prod.csv"))

# Filtering down to relevant columns (no duplicates with other tables)
prod <- prod %>%
  select(c(country_iso3_alpha, SciName, prod_method, habitat, quantity, year)) %>%
  rename(
    iso3c = country_iso3_alpha,
    sciname = SciName,
    method = prod_method,
    live_weight_t = quantity
  )

# Writing out results
write.csv(prod, file.path(outdir_attribute, "prod.csv"), row.names=FALSE)

# clean SAU prod
prod_sau <- read.csv(file.path(datadir, "standardized_combined_prod.csv"))

prod_sau <- prod_sau %>%
  select(c(country_iso3_alpha, SciName, prod_method, habitat, quantity, year)) %>%
  rename(
    iso3c = country_iso3_alpha,
    sciname = SciName,
    method = prod_method,
    live_weight_t = quantity
  )
write.csv(prod_sau, file.path(outdir_attribute, "prod_sau.csv"), row.names=FALSE)
