# Clean raw input data

###############################################################################
# Set directories and file naming variables
rm(list=ls())

# Local Machine Configuration setup
source("00-local-machine-setup.R")

# Creating folder for clean data if necessary
if (!dir.exists(datadir)) {
  dir.create(datadir)
} else {
  warning(glue::glue("Directory datadir `{datadir}` already exists!"))
}

#-------------------------------------------------------------------------------
# Load raw HS codes
hs_data_raw <- read.csv(file.path(datadir_raw, "All_HS_Codes.csv"), colClasses = "character")

# Generate new fishbase and sealifebase data files
# Note: these do not need to be generated for each model run and can be done once per year/quarter
# Directory Structure:
  # creates fishbase_sealifebase_[MOST RECENT DATE] within model_inputs_raw (ie. "model_inputs_raw/fishbase_sealifebase_[MOST_RECENT_DATE]")
if(need_new_fb_slb == TRUE) {
  collect_fb_slb_data(datadir_raw)
  message("New fishbase and sealifebase data files have been generated.")
} else {
  message("Existing fishbase and sealifebase data files are being used; Not collecting new data.")
}

# Find the most recent version of the fishbase and sealifebase data files needed
fb_slb_info <- get_most_recent_dir(datadir_raw, "fishbase_sealifebase")
current_fb_slb_dir <- fb_slb_info$directory

# Clean scientific names and add classification info to production data: choose FAO or SAU
# NOTE: warning message about data_frame() being deprecated is fixed in the development version of rfishbase: run remotes::install_github("ropensci/rfishbase") to implement the fixed version
prod_list <- classify_prod_dat(datadir = datadir_raw,
                               filename = "GlobalProduction_2022.1.1.zip", 
                               # "GlobalProduction_2023.1.1.zip"
                               prod_data_source = "FAO",
                               fb_slb_dir = current_fb_slb_dir)

# Reassign to separate objects:
prod_data_raw <- prod_list[[1]] 
prod_taxa_classification <- prod_list[[2]] %>%
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

prod_data <- prod_data_raw %>%
  filter(quantity > 0) %>%
  filter(year > 1995)  %>% # Filter to 1996 and on to work with a smaller file
  # Remove columns not needed for any analysis
  select(!c(any_of(c("alternate", "multiplier", "symbol", "symbol_identifier")), 
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
  mutate(taxa_source = paste(str_replace(SciName, " ", "."), habitat, prod_method, sep = "_")) %>%
  group_by(SciName, year, taxa_source, habitat, prod_method, country_iso3_alpha, country_name_en) %>%
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

# SAVE PRODUCTION OUTPUT:
write.csv(prod_data, file = file.path(datadir, "clean_fao_prod.csv"), row.names = FALSE)
write.csv(prod_taxa_classification, file = file.path(datadir, "clean_fao_taxa.csv"), row.names = FALSE)

prod_data <- standardize_countries(df = prod_data, 
                                   data_source = "FAO")
write.csv(prod_data, file = file.path(datadir, "standardized_fao_prod.csv"), row.names = FALSE)

rm(prod_list)

# SAU data----------------------------------------------------------------------
prod_list_sau <- classify_prod_dat(datadir = datadir_raw,
                                   filename = 'SAU_Production_Data.csv',
                                   prod_data_source = 'SAU',
                                   SAU_sci_2_common = "TaxonFunctionalCommercial_Clean.csv",
                                   fb_slb_dir = current_fb_slb_dir)

prod_data_sau <- prod_list_sau[[1]] %>%
  mutate(year = as.numeric(year)) %>%
  mutate(quantity = as.numeric(quantity)) %>%
  filter(quantity > 0) %>%
  filter(year > 1995)

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

# FIXIT: check if this data gets read back in - may be able to remove if not
write.csv(prod_data_sau, file.path(datadir, "clean_sau_prod.csv"), 
          row.names = FALSE)
write.csv(prod_classification_sau, file.path(datadir, "clean_sau_taxa.csv"), 
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
prod_data_sau <- standardize_countries(prod_data_sau, "FAO")

prod_data_sau <- prod_data_sau %>% 
  group_by(SciName, year, taxa_source, habitat, prod_method, country_iso3_alpha, 
           country_name_en, gear, eez, sector, end_use) %>% 
  summarise(quantity = sum(quantity)) %>%
  ungroup()

write.csv(prod_data_sau, file.path(datadir, 'standardized_sau_prod_more_cols.csv'), 
          row.names = FALSE)

prod_data_sau <- prod_data_sau %>% 
    group_by(SciName,  year, taxa_source, habitat, prod_method, 
             country_iso3_alpha, country_name_en) %>% 
    summarise(quantity = sum(quantity)) %>%
    ungroup()

write.csv(prod_data_sau, file.path(datadir, 'standardized_sau_prod.csv'), row.names = FALSE)
  
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

# Step 2: Create V1 and V2 for each HS version----------------------------------
# Load and clean the conversion factor data and run the matching functions. 
# This data will be used to create V1 and V2. 
hs_data_clean <- clean_hs(hs_data_raw = read.csv(file.path(datadir_raw, "All_HS_Codes.csv"), colClasses = "character"),
                          fb_slb_dir = current_fb_slb_dir)

# Getting list of fmfo species
fmfo_species <- get_fmfo_species(
  sau_fp = file.path(datadir, 'standardized_sau_prod_more_cols.csv'),
  fishmeal_min_threshold_sp = 1,
  fishmeal_min_threshold_global = 0.5,
  fishmeal_primary_threshold = 75
)

write.csv(fmfo_species, file.path(datadir_raw, 'fmfo_species_list.csv'), row.names = FALSE)

# List of possible HS versions: HS96, HS02, HS12, HS17
#HS_year <- c("96", "02", "07", "12", "17")
HS_year <- c("96")

if (test) {
  HS_year <- HS_year[HS_year %in% test_hs]
  hs_data_clean <- hs_data_clean %>%
    filter(Code %in% test_codes)
}

for(i in 1:length(HS_year)) {
  
  hs_version <- paste("HS", HS_year[i], sep = "")
  print(hs_version)
  
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
  set_match_criteria = "strict"
  hs_taxa_CF_match <- compile_cf(conversion_factors = read.csv(file.path(datadir_raw, "seafood_conversion_factors.csv"), stringsAsFactors = FALSE),
                                 eumofa_data = read.csv(file.path(datadir_raw, "EUMOFA_compiled.csv"), stringsAsFactors = FALSE),
                                 hs_hs_match,
                                 hs_version,
                                 match_criteria = set_match_criteria,
                                 fb_slb_dir = current_fb_slb_dir)
  
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
  
} # end of for loop


##############################################################################
# Step 3: Load trade (BACI) data, filter to just seafood products, and standardize countries between production and trade data
###############################################################################
# Create data frame with all hs year and analysis year combinations

# List of possible HS versions: HS96, HS02, HS12, HS17
# No need to do HS92 when using BACI though as that data starts in 1996
df_years <- data.frame(HS_year = c(rep("96", length(1996:2020)),
                                   rep("02", length(2002:2020)),
                                   rep("07", length(2007:2020)),
                                   rep("12", length(2012:2020)),
                                   rep("17", length(2017:2020))),
                       analysis_year = c(1996:2020, 2002:2020, 2007:2020,
                                         2012:2020, 2017:2020))

if (test) {
  df_years <- df_years %>%
    filter(HS_year == test_hs,
           analysis_year == test_year)
}

###############################################################################
# Load data file and filter for fish codes (i = exporter, j = importer, hs6 = HS code)
# Note on warning message "Some values were not matched unambiguously: NULL" means all values were matched

# Filter raw baci data
for (i in 1:nrow(df_years)){
  HS_year <- df_years[i,]$HS_year
  analysis_year <- df_years[i,]$analysis_year
  print(paste(HS_year, analysis_year))
  
  # Creating out folder if necessary
  if (!file.exists(file.path(datadir_raw, paste("filtered_BACI_", "HS", HS_year, "_Y", analysis_year, "_V", baci_version, ".csv", sep = "")))) {
    baci_data_i <- read.csv(file = file.path(tradedatadir, 
                                           paste("BACI_", "HS", HS_year, "_V", baci_version, sep = ""),
                                           paste("BACI_", "HS", HS_year, "_Y", analysis_year, "_V", baci_version, ".csv", sep = "")),
                          stringsAsFactors = FALSE)
    
    baci_data_i  <- baci_data_i  %>%
      mutate(q = as.numeric(q)) %>%
      # NAs should only arise when q is "           NA" (whitespace included)
      filter(!is.na(q))
    
    baci_data_i  <- load_baci(
      baci_data_i ,
      hs_codes = as.numeric(unique(hs_data_clean$Code)),
      baci_country_codes = read.csv(file.path(tradedatadir, 
                                              paste("BACI_", "HS", HS_year, "_V", baci_version, sep = ""),
                                              paste("country_codes_V", baci_version, ".csv", sep = "")))
    )
    
    write.csv(baci_data_i, file.path(datadir_raw, paste("filtered_BACI_", "HS", HS_year, "_Y", analysis_year, "_V", baci_version, ".csv", sep = "")),
              row.names = FALSE)
  } else {
    print("Filtered BACI file already exists")
    }
  } 

# Standardize BACI data
for (i in 1:nrow(df_years)){
  HS_year <- df_years[i,]$HS_year
  analysis_year <- df_years[i,]$analysis_year
  print(paste(HS_year, analysis_year))
  
  baci_data <- read.csv(file.path(datadir_raw, paste("filtered_BACI_", "HS", HS_year, "_Y", analysis_year, "_V", baci_version, ".csv", sep = "")))
  
  baci_data <- baci_data %>%
    mutate(year = analysis_year,
           hs_version = paste("HS", HS_year, sep = ""))
  
  baci_data <- standardize_countries(baci_data, "BACI")
  
  # BACI output used to generate ARTIS (keeps legacy dataframe format)
  write.csv(
    baci_data %>%
      select(-c(total_v)),
    file.path(datadir, paste("standardized_baci_seafood_hs", HS_year, "_y", analysis_year, ".csv", sep = "")),
    row.names = FALSE
  )

  # BACI output with total and unit value
  write.csv(
    baci_data,
    file.path(datadir, paste("standardized_baci_seafood_hs", HS_year, "_y", analysis_year, "_including_value.csv", sep = "")),
    row.names = FALSE
  )
}

# Clean FAO population data ------------------------------------------------
pop_raw <- read.csv(file.path(datadir_raw, "Population_E_All_Data/Population_E_All_Data_NOFLAG.csv"))

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
clean_fao <- read.csv(file.path(datadir_raw, "standard_fao_countries.csv"))
clean_pop <- clean_pop %>%
  filter(year <= 2020) %>%
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

write.csv(clean_pop, file.path(datadir, "fao_annual_pop.csv"), row.names = FALSE)


