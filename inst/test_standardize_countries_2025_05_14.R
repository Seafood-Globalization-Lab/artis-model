# Testing develop-country-standardization
# Althea and Connor
# 2025-05-14

library(countrycode)
library(dplyr)
library(data.table)

# read in sau prod versions
prod_sau_clean <- data.table::fread("~/Documents/UW-SAFS/ARTIS/data/model_inputs_sau/clean_sau_prod.csv")
prod_sau_std <- data.table::fread(file.path("~/Documents/UW-SAFS/ARTIS/data/model_inputs_sau/standardized_sau_prod.csv"))

# read in fao prod versions
prod_fao_clean <- fread("~/Documents/UW-SAFS/ARTIS/data/model_inputs/clean_fao_prod.csv")
prod_fao_std <- fread("~/Documents/UW-SAFS/ARTIS/data/model_inputs/standardized_fao_prod.csv")

# Setup Testing versions --------------------------------------------------

# setup "control" dataframes
test_clean_sau_df <- prod_sau_clean %>% 
  group_by(country_name_en, year) %>% 
  summarise(total_quantity = sum(quantity))

test_std_sau_df <- prod_sau_std %>% 
  group_by(country_iso3_alpha, year) %>% 
  summarise(total_quantity = sum(quantity))

# Need FAO versions as well to test 
test_clean_fao_df <- prod_fao_clean %>% 
  group_by(country_iso3_alpha, year) %>% 
  summarise(total_quantity = sum(quantity))

test_std_fao_df <- prod_fao_std %>% 
  group_by(country_iso3_alpha, year) %>% 
  summarise(total_quantity = sum(quantity))
# eventually need tests for BACI

# Treatment 1 - Connor's script - 'artis-model/develop-getcountrysolutions-clean` branch
corrections <- standardize_country_data()

result_fao <- prod_fao_clean %>% 
  left_join(corrections,
            by = c("country_iso3_alpha" = "input_iso3c",
                   "year"))

result_fao_std <- result_fao %>% 
  mutate(output_iso3c = case_when(country_iso3_alpha ...)) # START HERE 2025-06-04
  group_by(output_iso3c, year) %>% 
  summarise(total_quantity = sum(quantity))

test_std_fao_df

# Treatment 2 - Countycode codelist table

# sadflkja ----------------------------------------------------------------


prod_data_sau <- clean_sau_prod %>%
  # remove a trailing parenthetical phrase from a string
  mutate(country_name_en = str_remove(country_name_en, ' \\(.+\\)$')) %>%
  mutate(country_iso3_alpha = countrycode(country_name_en, 
                                          origin = 'country.name', 
                                          destination = 'iso3c')) %>%
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