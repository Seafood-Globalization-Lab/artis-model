
# Libraries
library(tidyverse)
library(countrycode)

# Resetting workspace
rm(list=ls())

# Directories and filenames
# Get most recent model inputs directory
# Assumption: all model inputs are located in the project folder
model_inputs_raw <- "model_inputs_raw"
model_inputs_dir <- "model_inputs"
clean_metadatadir <- "outputs/clean_metadata"

outdir <- "sql_database"

countries_filename <- "countries.csv"
hs_codes_filename <- "All_HS_Codes.csv"
prod_filename <- "standardized_fao_prod.csv"

#-------------------------------------------------------------------------------
# Creating sciname table

sciname <- read.csv(file.path(clean_metadatadir, "sciname_metadata.csv"))

# Joining and renaming sciname metadata and isscaap groups
sciname <- sciname %>%
  rename(isscaap = isscaap_group) %>%
  # rename(common_name = CommonName) %>%
  rename(fresh = Fresh01, brack = Brack01, saltwater = Saltwater01) %>%
  select(-c(fresh, brack, saltwater, Aquarium))

# All column names lower case
colnames(sciname) <- tolower(colnames(sciname))

# Writing out results
write.csv(sciname, file.path(outdir, "sciname.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# Creating code max resolved taxa table
code_max_resolved_taxa <- read.csv(file.path(clean_metadatadir, "code_max_resolved_taxa.csv"))

code_max_resolved_taxa <- code_max_resolved_taxa %>%
  mutate(hs_version = as.character(hs_version),
         hs6 = as.character(hs6)) %>%
  # Cleaning HS version
  mutate(hs_version = case_when(
    str_length(hs_version) == 1 ~ paste("0", hs_version, sep = ""),
    TRUE ~ hs_version)) %>%
  mutate(hs_version = paste("HS", hs_version, sep = "")) %>%
  # Cleaning hs6 code
  mutate(hs6 = case_when(
    str_length(hs6) == 5 ~ paste("0", hs6, sep = ""),
    TRUE ~ hs6
  ))

write.csv(code_max_resolved_taxa, file.path(outdir, "code_max_resolved.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# Creating Product metadata table
# hs codes, descriptions, FMFO status, product form

# Read in list of HS codes found in K Drive Data folder
products <- read.csv(file.path(model_inputs_raw, "All_HS_Codes.csv"))

products <- products %>%
  mutate(Code = as.character(Code)) %>%
  mutate(Code = case_when(
    str_length(Code) < 6 ~ paste("0", Code, sep=""),
    TRUE ~ Code
  ))

# Read in all hs-hs_match files concentrate on:
# code_pre, code_post, presentation_pre, presentation_post, state_pre, state_post

# Get list of all hs-hs-match files
prep_state_files <- list.files(path=model_inputs_dir, pattern="hs-hs-match", include.dirs=FALSE)
prep_state <- data.frame()

for (i in 1:length(prep_state_files)) {
  curr_file <- file.path(model_inputs_dir, prep_state_files[i])
  curr_prep_state <- read.csv(curr_file)
  
  curr_prep_state <- curr_prep_state %>%
    select(Code_pre, Code_post, Presentation_pre, Presentation_post, State_pre, State_post)
  
  curr_prep_state <- data.frame(
    hs6 = c(curr_prep_state$Code_pre, curr_prep_state$Code_post),
    presentation = c(curr_prep_state$Presentation_pre, curr_prep_state$Presentation_post),
    state = c(curr_prep_state$State_pre, curr_prep_state$State_post)
    ) %>%
    distinct() %>%
    mutate(hs6 = as.character(hs6)) %>%
    mutate(hs6 = case_when(
      str_length(hs6) < 6 ~ paste("0", hs6, sep=""),
      TRUE ~ hs6
    ))
  
  prep_state <- prep_state %>%
    bind_rows(curr_prep_state)
}

products <- products %>%
  left_join(prep_state, by=c("Code"="hs6")) %>%
  rename(hs6 = Code)

names(products) <- tolower(names(products))

# Writing out results
write.csv(products, file.path(outdir, "products.csv"), row.names=FALSE)

#-------------------------------------------------------------------------------
# Creating connecting table between sciname and products
# Should contain Species, HS codes, liveweight cfs


#-------------------------------------------------------------------------------
# Cleaning BACI data

baci_files <- list.files(path=model_inputs_dir, pattern="standardized_baci_seafood_hs", include.dirs=FALSE)
# Select only files without value (could select those with value instead)
baci_files <- baci_files[str_detect(baci_files, pattern = "value", negate = TRUE)]

baci <- data.frame()

for (i in 1:length(baci_files)) {
  curr_baci_filename <- baci_files[i]
  print(curr_baci_filename)
  curr_baci <- read.csv(file.path(model_inputs_dir, curr_baci_filename))
  
  curr_hs <- toupper(substring(curr_baci_filename, nchar(curr_baci_filename) - 13, nchar(curr_baci_filename) - 10))
  curr_year <- as.integer(substring(curr_baci_filename, nchar(curr_baci_filename) - 7, nchar(curr_baci_filename) - 4))
  
  curr_baci <- curr_baci %>%
    mutate(hs_version=curr_hs,
           year=curr_year) %>%
    select(c("exporter_iso3c", "importer_iso3c", "hs6", "total_q", "hs_version", "year")) %>%
    rename(product_weight_t = total_q)
  
  baci <- baci %>%
    bind_rows(curr_baci)
}

# # Filter to custom hs version-year combinations
# baci <- baci %>%
#   filter(
#     # Use HS96 from 1996-2003 (inclusive)
#     ((hs_version == "HS96") & (year <= 2003)) |
#       # Use HS02 from 2004-2009 (inclusive)
#       ((hs_version == "HS02") & (year >= 2004 & year <= 2009)) |
#       # Use HS07 from 2010-2012 (inclusive)
#       ((hs_version == "HS07") & (year >= 2010 & year <= 2012)) |
#       # Use HS12 from 2013-2019 (inclusive)
#       ((hs_version == "HS12") & (year >= 2013 & year <= 2020))
#   )

write.csv(baci, file.path(outdir, "baci.csv"), row.names = FALSE)

#-------------------------------------------------------------------------------
# Cleaning Production data

# clean fao file found in Outputs/model_inputs on K Drive
prod <- read.csv(file.path(model_inputs_dir, "standardized_fao_prod.csv"))

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
write.csv(prod, file.path(outdir, "prod.csv"), row.names=FALSE)

#-------------------------------------------------------------------------------
# Creating Country metadata table

# Create a country list based on production and BACI data (standardized countries)
countries <- data.frame(
  iso3c = unique(c(prod$iso3c, baci$exporter_iso3c, baci$importer_iso3c))
)

owid_region <- read.csv(file.path(model_inputs_raw, "owid_regions.csv"))

# Add metadata
countries <- countries %>%
  # Add country name
  mutate(country_name = case_when(
    iso3c == "NEI" ~ "Other nei",
    iso3c == "SCG" ~ "Serbia & Montenegro",
    TRUE ~ countrycode(iso3c, origin = "iso3c", destination = "country.name")
  )) %>%
  # Add region by Our World in Data classification
  left_join(
    owid_region %>%
      select("code", "owid_region" = "continent"),
    by = c("iso3c" = "code")
  ) %>%
  # special case for Other nei
  mutate(owid_region = case_when(
    iso3c == "NEI" ~ "Other nei",
    iso3c == "SCG" ~ "Europe",
    TRUE ~ as.character(owid_region)
  )) %>%
  # Add continent by countrycode
  mutate(continent = case_when(
    iso3c == "NEI" ~ "Other nei",
    iso3c == "SCG" ~ "Europe",
    TRUE ~ countrycode(iso3c, origin = "iso3c", destination = "continent")
  ))

# Writing out results
write.csv(countries, file.path(outdir, "countries.csv"), row.names = FALSE)
