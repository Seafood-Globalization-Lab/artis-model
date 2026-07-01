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

#### Correct by iso3
test_std_baci_iso3c_exporter <- artis::standardize_countries(data = raw_baci,
                                                   country_id_type = "iso3c",
                                                   country_col_name = "exporter_iso3c",
                                                   year_col_name = "year")

#### Correct by country name
test_std_baci_iso3c_importer <- artis::standardize_countries(data = raw_baci,
                                                             country_id_type = "iso3c",
                                                             country_col_name = "importer_iso3c",
                                                             year_col_name = "year")

#### Compare outputs of the new standardized data to the old standardized data

# Worfklow for running BACI standardization

## 1. Run standardized on exporter, rename artis_iso3c column to match exporter_iso3c column in std_baci

## 2. Run standardized on importer, rename artis_iso3c column to match importer_iso3c column in std_baci


test_std_baci_iso3c <- artis::standardize_countries( # First run through exporter iso3c corrections
  data = raw_baci,
  country_id_type = "iso3c",
  country_col_name = "exporter_iso3c",
  year_col_name = "year"
) %>%
  select(exporter_iso3c = artis_iso3c,
         exporter_country, # Keep original raw country name in data - don't overwrite with ARTIS countryname correction
         importer_iso3c, # Keep original raw country name in data - don't overwrite with ARTIS countryname correction
         importer_country, # Keep original raw country name in data - don't overwrite with ARTIS countryname correction
         year) %>%
  artis::standardize_countries( # Next run through importer iso3c corrections
    country_id_type = "iso3c",
    country_col_name = "importer_iso3c",
    year_col_name = "year"
  ) %>%
  select(exporter_iso3c,
         exporter_country,
         importer_iso3c = artis_iso3c,
         importer_country, 
         year) %>%
  distinct()

# Standardize any straggling NA's which include Taiwan and NEI as desired outputs
test_std_baci_name <- test_std_baci_iso3c %>%
  filter(is.na(exporter_iso3c) | is.na(importer_iso3c) ) %>%
  artis::standardize_countries(
    country_id_type = "name_en",
    country_col_name = "exporter_country",
    year_col_name = "year"
  ) %>%
  select(exporter_iso3c = artis_iso3c,
         exporter_country, # Keep original raw country name in data - don't overwrite with ARTIS countryname correction
         importer_iso3c, # Keep original raw country name in data - don't overwrite with ARTIS countryname correction
         importer_country, # Keep original raw country name in data - don't overwrite with ARTIS countryname correction
         year) %>%
  artis::standardize_countries( # Next run through importer country name corrections
    country_id_type = "name_en",
    country_col_name = "importer_country",
    year_col_name = "year"
  ) %>%
  select(exporter_iso3c,
         exporter_country,
         importer_iso3c = artis_iso3c,
         importer_country,
         year)

new_std_baci <- bind_rows(
  test_std_baci_iso3c %>% drop_na(), # drop corrections that will get changed to Taiwan
  test_std_baci_name
          ) %>%
  select(exporter_iso3c, importer_iso3c, year) %>%
  distinct()
  


# See what's in the new standardization that doesn't appear in the old standardization
std_check_1 <- setdiff(new_std_baci, std_baci)

# See what's in the new standardization that doesn't appear in the old standardization
std_check_2 <- setdiff(std_baci, new_std_baci)

std_check_2 %>%
  filter(!(importer_iso3c == "TWN" | exporter_iso3c == "TWN")) %>%
  filter(!(importer_iso3c == "NEI" | exporter_iso3c == "NEI"))


# Taiwan is not being added in properly

# NEI

# Timor Letse
standardize_country_data() %>% View()
