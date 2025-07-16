# Testing develop-country-standardization
# Althea and Connor
# 2025-05-14

library(countrycode)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(data.table)

# standardize_country_data.R is not formally part of artis package yet
source("./R/standardize_country_data.R")

# Althea's read in files

# # read in sau prod versions
# prod_sau_clean <- data.table::fread("~/Documents/UW-SAFS/ARTIS/data/model_inputs_sau/clean_sau_prod.csv")
# prod_sau_std <- data.table::fread(file.path("~/Documents/UW-SAFS/ARTIS/data/model_inputs_sau/standardized_sau_prod.csv"))

# # read in fao prod versions
# prod_fao_clean <- fread("~/Documents/UW-SAFS/ARTIS/data/model_inputs/clean_fao_prod.csv")
# prod_fao_std <- fread("~/Documents/UW-SAFS/ARTIS/data/model_inputs/standardized_fao_prod.csv")

# Connor's read in files
# read in sau prod versions
prod_sau_clean <- fread("data_for_testing/country-standardization-connor/clean_sau_prod.csv")
  # mutate(country_iso3_alpha = countrycode(country_name_en,
  #                                          origin = "country.name",
  #                                          destination = "iso3c"))
prod_sau_std <- fread("data_for_testing/country-standardization-connor/standardized_sau_prod.csv")

# read in fao prod versions
prod_fao_clean <- fread("data_for_testing/country-standardization-connor/clean_fao_prod.csv")
prod_fao_std <- fread("data_for_testing/country-standardization-connor/standardized_fao_prod.csv")

# Setup Testing versions --------------------------------------------------

# setup "control" dataframes
test_clean_sau_df <- prod_sau_clean %>% 
  group_by(country_name_en, year) %>% 
  summarise(total_quantity = sum(quantity))

# Standardized to compare to
test_std_sau_df <- prod_sau_std %>% 
  group_by(country_iso3_alpha, year) %>% 
  summarise(total_quantity = sum(quantity))

# Need FAO versions as well to test 
test_clean_fao_df <- prod_fao_clean %>% 
  group_by(country_iso3_alpha, year) %>% 
  summarise(total_quantity = sum(quantity))

# Standardized to compare to
test_std_fao_df <- prod_fao_std %>%  
  group_by(country_iso3_alpha, year) %>% 
  summarise(total_quantity = sum(quantity))
# eventually need tests for BACI

# Treatment 1 - Connor's script - 'artis-model/develop-getcountrysolutions-clean` branch
corrections <- standardize_country_data()

# Prepare data for join on test data via **iso3c column** by getting rid of any input country names. Not including this would mess up joins to iso3
corrections_iso3c_input <- corrections %>%
  filter(is.na(input_country_name))

# Prepare data for join on test data via **country name column**
corrections_name_input <- corrections %>%
  filter(!is.na(input_country_name))

# FAO ---------------------------------------------------------------------


# Join on corrections by input iso3c
result_fao_iso3_output <- prod_fao_clean %>% 
  left_join(corrections_iso3c_input,
            by = c("country_iso3_alpha" = "input_iso3c",
                   "year")) %>%
  mutate(output_iso3c = case_when(
    is.na(output_iso3c) ~ country_iso3_alpha,
    TRUE ~ output_iso3c
  )) %>%
  filter(!is.na(country_iso3_alpha))

# Join on corrections by input country name (currently only Channel islands and other nei)
result_fao_name_output <- prod_fao_clean %>%
  left_join(corrections_name_input,
            by = c("country_name_en" = "input_country_name",
                   "year")) %>% 
  filter(!is.na(output_iso3c))

# Join on corrections by input iso3c
result_fao <- bind_rows(result_fao_iso3_output, result_fao_name_output)

# Group by iso3c / year, take the sum of the quantities
result_fao_std <- result_fao %>%
  group_by(output_iso3c, year) %>% 
  summarise(total_quantity = sum(quantity))

# Rename test data for setdiff
result_fao_std_setdiff <- result_fao_std %>%
  rename(country_iso3_alpha = "output_iso3c")

# Test our function against control data frame
# Run setdiff on test and control datasets
# These are rows that appear in our revised code and not the previously standardized dataset
setdiff(result_fao_std_setdiff, test_std_fao_df) %>% View()

# Double check on the only differences to ensure that these differences are nominal. Answer: they are very small diferences, so basically the same
full_join(test_std_fao_df %>%
      filter(year == 2014,country_iso3_alpha == "TZA"), 
  result_fao_std_setdiff %>%
      filter(year == 2014,country_iso3_alpha == "TZA"), by = "country_iso3_alpha") %>%
  summarize(diff = total_quantity.x - total_quantity.y)

full_join(test_std_fao_df %>%
            filter(year == 2019,country_iso3_alpha == "DNK"), 
          result_fao_std_setdiff %>%
            filter(year == 2019,country_iso3_alpha == "DNK"), by = "country_iso3_alpha") %>%
  summarize(diff = total_quantity.x - total_quantity.y)


# Quality control check to ensure the only differences are very nominal (i.e., floating point differences)
method_compare <- result_fao_std %>%
  inner_join(test_std_fao_df, by = c("output_iso3c" = "country_iso3_alpha", "year")) %>%
  mutate(diff = abs(total_quantity.x - total_quantity.y),
         matches = diff <= 1e-3) %>% 
  filter(matches == FALSE)

result_fao_std %>% 
  anti_join(test_std_fao_df,
            by = c("output_iso3c" = "country_iso3_alpha", 
                   "year")) 


# Treatment 2 - Countycode codelist table

# SAU ----------------------------------------------------------------


sau_prod_chr_clean <- prod_sau_clean %>%
  # remove a trailing parenthetical phrase from a string
  # Removes entire parenthetical phrase from chr value
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

# Join on corrections by input iso3c
result_sau_iso3_output <- sau_prod_chr_clean %>% 
  left_join(corrections_iso3c_input,
            by = c("country_iso3_alpha" = "input_iso3c",
                   "year")) %>%
  mutate(output_iso3c = case_when(
    is.na(output_iso3c) ~ country_iso3_alpha,
    TRUE ~ output_iso3c
  )) %>%
  filter(!is.na(country_iso3_alpha))

# Join on corrections by input country name (currently only Channel islands and other nei)
result_sau_name_output <- sau_prod_chr_clean %>%
  left_join(corrections_name_input,
            by = c("country_name_en" = "input_country_name",
                   "year")) %>% 
  filter(!is.na(output_iso3c))

# Join on corrections by input iso3c
result_sau <- bind_rows(result_sau_iso3_output, result_sau_name_output)

# Group by iso3c / year, take the sum of the quantities
result_sau_std <- result_sau %>%
  group_by(output_iso3c, year) %>% 
  summarise(total_quantity = sum(quantity))

# Rename test data for setdiff
result_sau_std_setdiff <- result_sau_std %>%
  rename(country_iso3_alpha = "output_iso3c")

# Test our function against control data frame
# Run setdiff on test and control datasets
# These are rows that appear in our revised code and not the previously standardized dataset
setdiff(result_sau_std_setdiff, test_std_sau_df) %>% View()

full_join(result_sau_std_setdiff, test_std_sau_df, by = c("country_iso3_alpha", "year")) %>%
  mutate(diff = abs(total_quantity.x - total_quantity.y),
         matches = diff <= 1e-3) %>% 
  filter(matches == FALSE)

## TODOS:

# 1. clean up standardize_country_data so that its more readable
# 2. Draft function: add a conditional for SAU / FAO
# 3. Reconsider how BACI is added into standardize_country_data
