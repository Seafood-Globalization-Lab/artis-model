# Clean raw input data


# Enviro Setup -----------------------------------------------------------
# Set directories and file naming variables
rm(list=ls())

#load packages
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

# Local Machine Configuration setup
source("00-local-machine-setup.R")

# Load raw HS codes ------------------------------------------------------
hs_data_raw <- read.csv(file.path(datadir_raw, "All_HS_Codes.csv"), colClasses = "character")

# Get fishbase and sealifebase data ------------------------------------------------------
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
      "x" = "No fishbase_sealifebase directory found in {datadir_raw}",
      "i" = "Set {.code need_new_fb_slb = TRUE} in 00-local-machine-setup.R to download new data",
      "i" = "OR ensure a fishbase_sealifebase_* directory exists"
    ))
  }
}


# Structure FAO Production into usable dataframe -------------------------------

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

# Clean FAO Production (taxa names and classification) ---------------------------

prod_list <- artis::classify_prod_dat(
  datadir = datadir_raw,
  prod_data_source = "FAO",
  prod_df = rebuilt_fao_prod,
  fb_slb_dir = current_fb_slb_dir
)

# Reassign list to separate objects:
prod_data_raw <- prod_list[[1]] 
prod_taxa_classification <- prod_list[[2]]

# remove large less-clean environmental objects no longer needed
rm(prod_list, rebuilt_fao_prod)

# Structure FAO prod taxa classification ---------------------------------
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

# DATA CHECK: Verify that habitat information is complete
missing_habitat_species <- prod_taxa_classification %>% 
  mutate(habitat_sum = Fresh01 + Brack01 + Saltwater01) %>%
  filter(habitat_sum == 0)

if (nrow(missing_habitat_species) > 0) {
  cli::cli_warn(c(
    "!" = "{nrow(missing_habitat_species)} species missing habitat information",
    "i" = "Species without habitat coding: {.val {missing_habitat_species$SciName}}",
    "i" = "Check prod_taxa_classification for Fresh01, Brack01, and Saltwater01 columns"
  ))
}

# SAVE PRODUCTION Taxa OUTPUT:
write.csv(prod_taxa_classification, file = file.path(datadir, "clean_fao_taxa.csv"), row.names = FALSE)

# Get fishbase habitat data from prod_taxa_classification to standardize habitat info in prod_data
prod_habitat <- prod_taxa_classification %>%
  select(SciName, Fresh01, Brack01, Saltwater01) %>%
  distinct()

# ISSCAP Metadata ---------------------------------------------------------

# used to create code_max_resolved which is used in ARTIS calculate_consumption

#Create 1-to-1 matching for 
isscaap_metadata <- prod_data_raw %>%
  select(SciName, isscaap_group) %>%
  distinct()

multiple_isscaap <- isscaap_metadata %>% 
  group_by(SciName) %>%
  tally() %>%
  filter(n>1) %>%
  pull(SciName)

isscaap_metadata <- isscaap_metadata %>%
  mutate(isscaap_group = case_when(
    SciName %in% multiple_isscaap ~ "Multiple ISSCAAP groups",
    !(SciName %in% multiple_isscaap) ~ isscaap_group
  )) %>%
  distinct()

# Add ISSCAAP groups for custom "unknown origin" scinames
unknown_isscaap <- data.frame(sciname = c("arthropoda", "chondrichthyes", 
                                  "engraulis", "actinopteri", "homarus",
                                  "mytilinae", "clupea", "hippoglossinae", 
                                  "scombrinae", "salmoninae", "animalia", 
                                  "dissostichus", "cypriniformes", 
                                  "micromesistius", "echinoida", "chordata"),
                      isscaap_group = c("Multiple ISSCAAP groups", "Sharks, rays, chimaeras",
                                        "Herrings, sardines, anchovies", "Multiple ISSCAAP groups",
                                        "Lobsters, spiny-rock lobsters", "Mussels",
                                        "Herrings, sardines, anchovies", "Flounders, halibuts, soles", 
                                        "Multiple ISSCAAP groups", "Salmons, trouts, smelts", 
                                        "Multiple ISSCAAP groups", "Miscellaneous demersal fishes", 
                                        "Carps, barbels and other cyprinids", "Cods, hakes, haddocks",
                                        "Sea-urchins and other echinoderms", "Multiple ISSCAAP groups"))

isscaap_metadata <- isscaap_metadata %>%
  bind_rows(unknown_isscaap)

write.csv(isscaap_metadata, file.path(datadir, "isscaap_metadata.csv"), row.names = FALSE)


# Structure FAO prod -----------------------------------------------------
prod_data <- prod_data_raw %>%
  # Remove columns not needed for any analysis
  select(!c(any_of(c("alternate", "multiplier", "symbol", "symbol_identifier", 
                    "country_iso3_numeric", "country_identifier", "production_identifier", 
                    "sort", "unit_identifier")), # "species_identifier" still available here
            contains(c("_ar", "_cn", "_es", "_fr", "_ru")),
            CommonName)) %>%
  # Create new column that combines SciName with souce info (i.e., habitat + production method)
  mutate(fao_habitat = case_when(habitat == "Inland waters" ~ "inland",
                             habitat == "Marine areas" ~ "marine",
                             TRUE ~ habitat),
         prod_method = case_when(prod_method %in% c("FRESHWATER", "MARINE", "BRACKISHWATER") ~ "aquaculture",
                                 prod_method == "CAPTURE" ~ "capture",
                                 TRUE ~ prod_method)) %>%
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

prod_data <- prod_data %>% 
  group_by(SciName, year, taxa_source, habitat, prod_method, country_iso3_alpha, country_name_en, area.code) %>%
  summarize(quantity = sum(quantity, na.rm = TRUE)) %>%
  ungroup()


if (test) {
  
  prod_data <- prod_data %>%
    filter(year == test_year) %>%
    filter(SciName %in% test_scinames)
  
  prod_taxa_classification<- prod_taxa_classification %>%
    filter(SciName %in% test_scinames)
}

prod_data <- standardize_countries(df = prod_data, 
                                   data_source = "FAO")
# retain FAO area.code column
write.csv(prod_data, file = file.path(datadir, "standardized_fao_prod_more_cols.csv"), row.names = FALSE)

# remove area.code column to format to prod version used in model
prod_data <- prod_data %>% 
  group_by(SciName, year, taxa_source, habitat, prod_method, country_iso3_alpha, country_name_en) %>%
  summarize(quantity = sum(quantity, na.rm = TRUE)) %>%
  ungroup()

write.csv(prod_data, file = file.path(datadir, "standardized_fao_prod.csv"), row.names = FALSE)



# Clean SAU Production (taxa names and classification) -------------------------
prod_list_sau <- classify_prod_dat(datadir = datadir_raw,
                                   prod_data_source = 'SAU',
                                   prod_df = fread(file.path(datadir, "SAU_Production_Data.csv"), 
                                                    stringsAsFactors = FALSE, 
                                                    header = TRUE, 
                                                    sep=",", 
                                                    data.table = FALSE),
                                   SAU_sci_2_common = "TaxonFunctionalCommercial_Clean.csv",
                                   fb_slb_dir = current_fb_slb_dir)

prod_data_sau <- prod_list_sau[[1]] %>%
  mutate(
    year = as.numeric(year),
    quantity = as.numeric(quantity))

prod_data_sau <- prod_data_sau %>%
  mutate(habitat = "marine",
         prod_method = "capture") %>%
  mutate(taxa_source = paste(str_replace(SciName, " ", "."), 
                             habitat, prod_method, sep = "_"))

prod_classification_sau <- prod_list_sau[[2]]

prod_data_sau <- prod_data_sau %>%
  # Keep FAO format
  mutate(Fresh01 = 0, Saltwater01 = 1, Brack01 = 0)

rm(prod_list_sau)

write.csv(prod_classification_sau, file.path(datadir, "clean_sau_taxa.csv"), 
           row.names = FALSE)

# initial country name cleaning and adding iso3c for SAU data
prod_data_sau <- prod_data_sau %>%
  mutate(country_name_en = str_remove(country_name_en, ' \\(.+\\)$')) %>%
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

# standardize countries for SAU production
prod_data_sau <- standardize_countries(prod_data_sau, "FAO")

# group by and summarize across more SAU production columns. 
prod_data_sau <- prod_data_sau %>% 
  group_by(SciName, year, taxa_source, habitat, prod_method, country_iso3_alpha, 
           country_name_en, gear, eez, sector, end_use) %>% 
  summarise(quantity = sum(quantity), .groups = "drop") 

write.csv(prod_data_sau, file.path(datadir, 'standardized_sau_prod_more_cols.csv'), 
          row.names = FALSE)

prod_data_sau <- prod_data_sau %>% 
    group_by(SciName,  year, taxa_source, habitat, prod_method, 
             country_iso3_alpha, country_name_en) %>% 
    summarise(quantity = sum(quantity), .groups = "drop") 

write.csv(prod_data_sau, file.path(datadir, 'standardized_sau_prod.csv'), row.names = FALSE)


# Subsitute SAU marine prod into FAO prod --------------------------------
if (running_sau) {
  # Combine SAU production data with FAO data
  prod_data <- prod_data %>%
    filter(!(habitat == 'marine' & prod_method == 'capture')) %>%
    bind_rows(prod_data_sau)
  
  write.csv(prod_data, file.path(datadir, 'standardized_combined_prod.csv'),
            row.names = FALSE)
  
  # combine prod taxa classification
  prod_taxa_classification <- prod_taxa_classification %>%
    bind_rows(prod_classification_sau) %>%
    distinct() %>%
    filter(SciName %in% unique(prod_data$SciName))
  
  write.csv(prod_taxa_classification, 
            file.path(datadir, "clean_taxa_combined.csv"),
            row.names = FALSE)
}
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

# Create V1 and V2 for each HS version ----------------------------------
# Load and clean the conversion factor data and run the matching functions. 
# This data will be used to create V1 and V2. 
hs_data_clean <- clean_hs(hs_data_raw = fread(file.path(datadir_raw, "All_HS_Codes.csv"), colClasses = "character"),
                          fb_slb_dir = current_fb_slb_dir)


# FMFO Species from SAU --------------------------------------------------
# Getting list of fmfo species
fmfo_species <- get_fmfo_species(
  sau_fp = file.path(datadir, 'standardized_sau_prod_more_cols.csv'),
  fishmeal_min_threshold_sp = 1,
  fishmeal_min_threshold_global = 0.5,
  fishmeal_primary_threshold = 75
)

write.csv(fmfo_species, file.path(datadir_raw, 'fmfo_species_list.csv'), row.names = FALSE)

# Get HS years for data cleaning loops below
HS_year <- unique(df_years$HS_year)

if (test) {
  HS_year <- HS_year[HS_year %in% test_hs]
  hs_data_clean <- hs_data_clean %>%
    filter(Code %in% test_codes)
}

# Loop HS versions through match funs and cf's --------------------------------
for(i in 1:length(HS_year)) {
  
  hs_version <- paste("HS", HS_year[i], sep = "")
  message(glue("{hs_version} Matching taxa/products/conversion factors"))
  

  # Match HS codes to production taxa (can be FAO or SAU depending on which was used in clean_and_clasify_prod_dat function)
  hs_taxa_match <- match_hs_to_taxa(hs_data_clean = hs_data_clean,
                                    prod_taxa_classification = prod_taxa_classification,
                                    # species used for FMFO globally based on SAU data
                                    fmfo_species_list = fmfo_species,
                                    hs_version = hs_version)
  
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
    summarize(habitat_percent = sum(habitat_percent, na.rm = TRUE), .groups = "keep") %>%
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
  write.csv(hs_taxa_match, file = file.path(datadir, paste("hs-taxa-match_", hs_version, ".csv", sep="")), row.names = FALSE)
  
  
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

if (test) {
  df_years <- df_years %>%
    filter(HS_year == test_hs,
           analysis_year == test_year)
}

# Load data file and filter for fish codes (i = exporter, j = importer, hs6 = HS code)
# Note on warning message "Some values were not matched unambiguously: NULL" means all values were matched

#### Filter raw baci data #######
for (i in 1:nrow(df_years)){
  a_HS_year <- df_years[i,]$HS_year
  analysis_year <- df_years[i,]$analysis_year
  
  # Creating out folder if necessary
  if (!file.exists(file.path(datadir_raw, paste("filtered_BACI_", "HS", a_HS_year, "_Y", analysis_year, "_V", baci_version, ".csv", sep = "")))) {
    
    message(glue("Filter BACI HS{a_HS_year} {analysis_year}"))
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
    print(glue("Filtered BACI HS{a_HS_year} {analysis_year} file already exists. Skipping to next HS/year pair."))
    }
  } 

#### Standardize BACI data ####
for (i in 1:nrow(df_years)){
  a_HS_year <- df_years[i,]$HS_year
  analysis_year <- df_years[i,]$analysis_year
  print(glue("standardize BACI {a_HS_year} {analysis_year}"))
  
  baci_data <- read.csv(file.path(datadir_raw, paste("filtered_BACI_", "HS", a_HS_year, "_Y", analysis_year, "_V", baci_version, ".csv", sep = "")))
  
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
}

# Clean FAO population data ------------------------------------------------
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

################ Metadata tables

# Load cleaned taxa details
taxa <- fread(file.path(datadir, "clean_fao_taxa.csv")) %>%
  rename(sciname = SciName, common_name = CommonName) %>%
  distinct()

# sciname_metadata --------------------------------------------------------

# Create file for phylogenetic metadata
# Create 1-to-1 matching for common names and taxa info
taxa_metadata <- taxa %>%
  mutate(common_name = case_when(
    sciname =="alosa" ~ "shads nei", 
    sciname == "asteroidea" ~ "starfishes nei", 
    sciname == "branchiopoda" ~ "crustaceans nei", 
    sciname == "carcharhiniformes" ~ "ground sharks nei", 
    sciname == "clarias" ~ "catfishes nei", 
    sciname == "clupeidae" ~ "sardines nei", 
    sciname == "clupeiformes" ~ "clupeoids nei", 
    sciname == "dentex tumifrons" ~ "yellowback seabream",
    sciname == "epinephelus" ~ "groupers nei", 
    sciname == "gadus macrocephalus" ~ "pacific cod", 
    sciname == "gobiidae" ~ "gobies nei", 
    sciname == "jasus edwardsii" ~ "red rock lobster", 
    sciname == "lepidonotothen squamifrons" ~ "grey rockcod", 
    sciname == "macrobrachium" ~ "river prawns nei", 
    sciname == "merluccius" ~ "hakes nei", 
    sciname == "mollusca" ~ "molluscs nei", 
    sciname == "mullus" ~ "surmullets(=red mullets) nei", 
    sciname == "myliobatidae" ~ "eagle and manta rays nei", 
    sciname == "oreochromis" ~ "tilapias nei", 
    sciname == "osteichthyes" ~ "fish nei", 
    sciname == "palaemonidae" ~ "palaemonid shrimps nei", 
    sciname == "parastacidae" ~ "crayfishes nei", 
    sciname == "penaeidae" ~ "penaeid shrimps nei", 
    sciname == "perciformes" ~ "tuna-like fishes nei", 
    sciname == "planiliza haematocheilus" ~ "so-iny (redlip) mullet", 
    sciname == "salmonidae" ~ "almonids nei", 
    sciname == "sardinops sagax" ~ "south american pilchard", 
    sciname == "sebastes" ~ "redfishes nei", 
    sciname == "serrasalmidae" ~ "serrasalmids nei", 
    sciname == "thunnus" ~ "tunas nei", 
    sciname == "xiphopenaeus kroyeri" ~ "atlantic seabob", 
    sciname == "bryzoa" ~ "bryzoa",
    TRUE ~ common_name
  )) %>%
  distinct() %>%
  bind_rows(data.frame(
    sciname = c("arthropoda", "engraulis", "hippoglossinae", "scombrinae", "clupea",     
                "chondrichthyes", "salmoninae", "mytilinae", "actinopteri", "animalia",    
                "homarus", "cypriniformes", "dissostichus", "micromesistius", "echinoida"),
    
    common_name = c("arthropods", "anchovies", "flounders", "mackerels, tunas, and bonitos",
                   "herrings", "sharks, skates, rays, and chimaeras", "salmons and trouts",
                   "saltwater mussels", "ray-finned fish", "aquatic animals", "lobsters", 
                   "carps, minnows, loaches, etc", "toothfish", "blue whitings", "sea urchins"),
    
    Genus = c(NA, "engraulis", NA, NA, "clupea",     
              NA, NA, NA, NA, NA,    
              "homarus", NA, "dissostichus", "micromesistius", NA),
    
    Subfamily = c(NA, "engraulinae", "hippoglossinae", "scombrinae", "clupeinae",     
                  NA, "salmoninae", "mytilinae", NA, NA,    
                  NA, NA, NA, NA, NA),
    
    Family = c(NA, "engraulidae", "pleuronectidae", "scombridae", "clupeidae",     
               NA, "salmonidae", "mytilidae", NA, NA,    
               "nephropidae", NA, "nototheniidae", "gadidae", NA), 
    
    Order = c(NA, "clupeiformes", "pleuronectiformes", "scombriformes", "	clupeiformes",     
              NA, "salmoniformes", "mytilida", NA, NA,    
              "decapoda", "cypriniformes", "perciformes", "gadiformes", "echinoida"), 
    
    Class = c(NA, "teleostei", "teleostei", "teleostei", "teleostei",     
              "chondrichthyes", "teleostei", "bivalvia", "teleostei", NA,    
              "malacostraca", "teleostei", "teleostei", "teleostei", "echinoidea"),
    
    Superclass = c(NA, "osteichthyes", "osteichthyes", "osteichthyes", "osteichthyes",     
                   NA, "osteichthyes", NA, "osteichthyes", NA,    
                   NA, "osteichthyes", "osteichthyes", "osteichthyes", NA),
    
    Phylum = c("arthropoda", "chordata", "chordata", "chordata", "chordata",     
               "chordata", "chordata", "mollusca", "chordata", NA,    
               "arthropoda", "chordata", "chordata", "chordata", "echinodermata"),
    
    Kingdom = c("animalia", "animalia", "animalia", "animalia", "animalia",     
                "animalia", "animalia", "animalia", "animalia", "animalia",    
                "animalia", "animalia", "animalia", "animalia", "animalia")
  )) 
 

taxa_metadata <- taxa_metadata %>%
  left_join(isscaap_metadata, by = "sciname")

if(running_sau){
# Add missing scinames from SAU with sau_taxa 
sau_taxa <- fread(file.path(datadir, "clean_sau_taxa.csv")) %>%
  rename(sciname = SciName, common_name = CommonName) %>%
  distinct() %>%
  filter(!(sciname %in% taxa_metadata$sciname))
  }

taxa_metadata <- taxa_metadata %>%
  #bind_rows(sau_taxa) %>%
  distinct() %>%
  ungroup() %>%
  mutate(sum_na = rowSums(is.na(.))) %>%
  group_by(sciname) %>%
  slice_min(order_by = sum_na, n = 1, with_ties = FALSE) %>%
  select(-sum_na)

write.csv(taxa_metadata, file.path(outdir_attribute, "sciname_metadata.csv"), row.names = FALSE)

# Code_max_resolved_taxa -------------------------------------------------


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
  
  # Add SAU matches - running with just SAU ARTIS v1.1.0 - not two separate model_inputs/
  # hs_taxa_match_sau_i <- read.csv(file.path("model_inputs_sau", paste("hs-taxa-match_HS", HS_year_rep, ".csv", sep = "")))
  # 
  # hs_clade_match_sau_i <- match_hs_to_clade(hs_taxa_match = hs_taxa_match_sau_i ,
  #                                       prod_taxa_classification = taxa %>%
  #                                         rename(CommonName = common_name, SciName = sciname),
  #                                       match_to_prod = FALSE) %>% 
  #   # pad HS codes with zeroes
  #   mutate(Code = as.character(Code)) %>%
  #   mutate(Code = if_else(str_detect(Code, "^30"), true = str_replace(Code, pattern = "^30", replacement = "030"),
  #                         if_else(str_detect(Code, "^511"), true = str_replace(Code, pattern = "^511", replacement = "0511"),
  #                                 false = Code))) %>%
  #   mutate(hs_version = HS_year_rep) %>%
  #   filter(!is.na(hs_clade))
  # 
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
  