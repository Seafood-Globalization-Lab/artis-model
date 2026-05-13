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

### Correct by iso3
test_std_fao <- artis::standardize_countries(data = raw_fao,
                             country_id_type = "iso3c",
                             country_col_name = "country_iso3_alpha",
                             year_col_name = "year")

### Correct by country name
test_std_fao <- artis::standardize_countries(data = raw_fao,
                                             country_id_type = "name_en",
                                             country_col_name = "country_name_en",
                                             year_col_name = "year")

### Compare new standardized data to input data
x <- test_std_fao %>% distinct(artis_iso3c, year) %>%
  rename(country_iso3_alpha = artis_iso3c) # 4740 rows
y <- std_fao %>% select(country_iso3_alpha, year) # 4752 rows

### How our corrections currently differ: We include ZA1
### Whats old corrections have that our corrections don't have: NEI & SDN
setdiff(na.omit(x), y) 
setdiff(y, x) %>% View()

## Compare outputs of the new standardized data to the old standardized data

### Extract any records that don't match, and go from there

### There's the same number of records that are in the raw and standardized datasets




