# Test this function Now

# Use raw data - compare results aginst ARTIS v1.2 standardized clean input data. 
{
  library(tidyverse)
  library(countrycode)
  library(artis)
  library(devtools)
  library(glue)
  library(readr)
}

# Connor's path
path_local <- "QA"
# Althea's path
# path_local <- "AM_local"

# read in raw combo files as controls
raw_fao <- read_csv(glue("./{path_local}/dev-standardize-countries/1.2_fao_country_combos.csv"))
raw_sau <- read_csv(glue("./{path_local}/dev-standardize-countries/1.2_sau_country_combos.csv"))
raw_baci <- read_csv(glue("./{path_local}/dev-standardize-countries/1.2_baci_country_combos.csv"))

std_fao <- read_csv(glue("./{path_local}/dev-standardize-countries/1.2_fao_std_country_combos.csv"))
std_sau <- read_csv(glue("./{path_local}/dev-standardize-countries/1.2_sau_std_country_combos.csv"))
std_baci <- read_csv(glue("./{path_local}/dev-standardize-countries/1.2_baci_std_country_combos.csv"))


# Read in test table for consumption quantities
table_tsv <- read_tsv(glue("./{path_local}/dev-standardize-countries/table.tsv"))
# Validation Outline / Plan

## Run validation check differences between raw and standardized datasets

### FAO
nrow(raw_fao) # larger amount of rows in raw than in standardized
nrow(std_fao)

### Quite a bit of filtering that happens in the FAO data from raw to standardized, filter by year
unique(raw_fao$country_iso3_alpha) %>% length() # 235 countries & territories
unique(std_fao$country_iso3_alpha) %>% length() # 193 sovereign countries

### SAU
nrow(raw_sau) # larger amount of rows in raw than in standardized
nrow(std_sau)

unique(raw_sau$country_name_en) %>% length() # 198 countries & territories
unique(std_sau$country_name_en) %>% length() # 153 sovereign countries

### BACI
nrow(raw_baci) # larger amount of rows in raw than in standardized
nrow(std_baci)

unique(raw_baci$country_iso3_alpha) %>% length() # 235 countries & territories
unique(std_baci$country_iso3_alpha) %>% length() # 193 sovereign countries

## Run each raw data file through artis::standardize_countries()

### FAO ----------------------------------------------------------

# ------------------------------------------------------------------------
###### This is the workflow to split up the dataframe to be standardized ##########

# 1) Correct by iso3
test_std_fao_iso3c <- artis::standardize_countries(
  data = raw_fao,
  country_id_type = "iso3c",
  country_col_name = "country_iso3_alpha",
  year_col_name = "year"
)

# 2) filter out NA values produced
df_1 <- test_std_fao_iso3c %>%
  filter(!is.na(country_iso3_alpha))


# 6/10/2026 
# GH ISSUE 6/3/2026
# Figure out how to correct missing string iso3 values to standardized iso3 values (e.g., Missing iso3c value, but country name value of Sudan (Former) 
# standardizes to NA). We may want to consider an additional join by country name for these cases.

# 3) Filter to NA values - rerun standardization on country name column
df_2 <- test_std_fao_iso3c %>%
  filter(is.na(country_iso3_alpha)) %>%
  select(country_iso3_alpha, country_name_en, year) %>%
  artis::standardize_countries(country_id_type = "name_en",
                             country_col_name = "country_name_en",
                             year_col_name = "year")

# 4) bind iso3c corrections and country name corrections
test_standardized_fao <- bind_rows(df_1, df_2)
# end of FAO workflow
# ------------------------------------------------------------------------

## Compare new standardized data to input data

### Test for iso3c
new <- test_standardized_fao %>%
  distinct(artis_iso3c, year) %>%
rename(country_iso3_alpha = artis_iso3c) # 4740 rows

old <- std_fao %>%
  select(country_iso3_alpha, year) # 4752 rows

# Correcting by iso3c using new methods correctly matches old methods. New country added n via new corrections: ZA1
setdiff(new, old)
setdiff(old, new)

### Test again for country name

#### Correct by country name
test_std_fao_name <- artis::standardize_countries(data = raw_fao %>%
                                                    bind_rows(data.frame(country_name_en = "South Sudan", country_iso3_alpha = "SSD", year = 2000)),
                                                  country_id_type = "name_en",
                                                  country_col_name = "country_name_en",
                                                  year_col_name = "year")

new <- test_std_fao_name %>%
  distinct(artis_country_name, year) %>%
  rename(country_name_en = artis_country_name) 

old <- std_fao %>%
  select(country_name_en, year)

setdiff(new, old)
setdiff(old, new)

### SAU ----------------------------------------------------------
#### Correct by name - only name in SAU data
test_std_sau_name <- artis::standardize_countries(
  data = raw_sau,
  country_id_type = "name_en",
  country_col_name = "country_name_en",
  year_col_name = "year"
)
  
#### Compare outputs of the new standardized data to the old standardized data

# Rows in new standardized data that doesn't appear in old standardized data: Serbia and Montenegro - expected and correct 
sau_new <- setdiff(test_std_sau_name %>% distinct(year, country_name_en = artis_country_name), std_sau)

# Rows in old standardized data that doesn't appear in new standardized data: Montenegro - expected and correct
sau_old <- setdiff(std_sau, test_std_sau_name %>% distinct(year, country_name_en = artis_country_name))

# quick test that this is doing what we expect. Yes it is. pass.
# It does seem like old std countries function was doing this country parsing incorrectly. 
tmp_df <- data.frame(country_name_en = "Serbia", year = 1998)
tmp_df <- data.frame(country_name_en = "Montenegro", year = 1998)
artis::standardize_countries(
  data = tmp_df,
  country_id_type = "name_en",
  country_col_name = "country_name_en",
  year_col_name = "year"
)

# ZAF vs ZA1 - Expected change
# 7/1 issue: Unknown fishing country now maps to NEI

standardize_country_data() %>% View()

### BACI ----------------------------------------------------------

#### Compare outputs of the new standardized data to the old standardized data

# Worfklow for running BACI standardization

# Step A. Correct by iso3c.
base_df <-
  # 1. standardize exports
  artis::standardize_countries(
    data = raw_baci,
    country_id_type = "iso3c",
    country_col_name = "exporter_iso3c",
    year_col_name = "year"
  ) %>%
  select(exporter_iso3c = artis_iso3c,
         exporter_country,
         importer_iso3c, # Keep original raw country name in data - don't overwrite with ARTIS countryname correction
         importer_country, # Keep original raw country name in data - don't overwrite with ARTIS countryname correction
         year) %>%
  # 2. standardize imports
  artis::standardize_countries(
    # Next run through importer iso3c corrections
    country_id_type = "iso3c",
    country_col_name = "importer_iso3c",
    year_col_name = "year"
  ) %>%
  select(exporter_iso3c,
         exporter_country,
         importer_iso3c = artis_iso3c,
         importer_country,
         year)

# Problem: NA's are produced, which will be solved in steps B and C. We remove NA's in this dataset.
# Obtain data that contains non NA's that standardize on the first go
test_std_baci_iso3c <- base_df %>%
  filter(!is.na(exporter_iso3c),
         !is.na(importer_iso3c)) %>%
  select(exporter_iso3c, importer_iso3c, year)

# Step B. create standardized table that only includes NA values from iso3c corrections 
# so we can use that output to recorrect by countryname in C, producing non NA iso3c values.
data_for_na_std <- base_df %>%
  filter(is.na(exporter_iso3c) | is.na(importer_iso3c))

# Step C. use standardized table to recorrect by countryname, converting NA iso3c values to their countryname assigned non-NA values.
test_std_baci_name <- data_for_na_std %>%
  # 1. standardize exports
  artis::standardize_countries( # Gets rid of NA iso3c values in exporter_iso3c column
    country_id_type = "name_en",
    country_col_name = "exporter_country",
    year_col_name = "year"
  ) %>%
  select(exporter_iso3c = artis_iso3c, # we overwrite exporter_iso3c to artis_iso3c
         importer_iso3c, # Keep as it's needed for the next correction
         importer_country, # Keep as it's needed for the next correction
         year) %>%
  # 2. standardize exports
  artis::standardize_countries( # Next run through importer country name corrections
    country_id_type = "name_en",
    country_col_name = "importer_country",
    year_col_name = "year"
  ) %>%
  select(exporter_iso3c,
         importer_iso3c = artis_iso3c,
         year)

# Bind datasets from step A and step C (i.e., corrections by iso3c & recorrections by countryname)
new_std_baci <- bind_rows(
  test_std_baci_iso3c, # Rows that had no problems standardizing
  test_std_baci_name # Rows that needed standardization by countryname
  ) %>%
  # Remove circular tradeflows (e.g., India exports to India importing country)
  filter(exporter_iso3c != importer_iso3c) 
# Also need to add a group_by() and summarize

## End of workflow




############ Testing

# Get only distinct rows in new standardization
new_std_baci_distinct <- new_std_baci %>%
  distinct()

nrow(std_baci) # old standardization
nrow(new_std_baci_distinct) # new standardization

# There might be a bigger issue going on with territory aggregation

# Timor-Letse was not given it's Indonesia sovereign in the old standardized data - our new correction is correct
# std_check_1 Our updates to country standardization may be correct, though duplicate country names may need to be dropped
# If we do drop duplicate country names (e.g., NLD --> NLD) we first need to summarize consumption weights
# See what's in the new standardization that doesn't appear in the old standardization
# Next steps: 


# See what's in the new standardization that doesn't appear in the old standardization
std_check_1 <- setdiff(new_std_baci, std_baci)

# See what's in the old standardization that doesn't appear in the new standardization
std_check_2 <- setdiff(std_baci, new_std_baci)

# fix NEI's, fix belarus, and fix SACU to ensure that is correcting properly 7/17/2026
std_check_2 %>%
  filter(importer_iso3c != "ZAF",
         exporter_iso3c != "ZAF",
         importer_iso3c != "NEI",
         exporter_iso3c != "NEI",
         importer_iso3c != "TLS",
         exporter_iso3c != "TLS") %>% View()

# std_check_2 %>%
#   filter(!(importer_iso3c == "TWN" | exporter_iso3c == "TWN")) %>%
#   filter(!(importer_iso3c == "NEI" | exporter_iso3c == "NEI"))

# Open corrections table
corrections_table <- standardize_country_data()


# Taiwan is not being added in properly

# NEI

# Timor Letse
standardize_country_data() %>% View()
