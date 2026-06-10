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

### FAO

#### Correct by iso3
test_std_fao_iso3c <- artis::standardize_countries(
  data = raw_fao,
  country_id_type = "iso3c",
  country_col_name = "country_iso3_alpha",
  year_col_name = "year"
)

df_1 <- test_std_fao_iso3c %>%
  filter(!is.na(country_iso3_alpha))


# 6/10/2026 
# GH ISSUE 6/3/2026
# Figure out how to correct missing string iso3 values to standardized iso3 values (e.g., Missing iso3c value, but country name value of Sudan (Former) 
# standardizes to NA). We may want to consider an additional join by country name for these cases.

df_2 <- test_std_fao_iso3c %>%
  filter(is.na(country_iso3_alpha)) %>%
  select(country_iso3_alpha, country_name_en, year) %>%
  artis::standardize_countries(country_id_type = "name_en",
                             country_col_name = "country_name_en",
                             year_col_name = "year")

test_standardized_fao <- bind_rows(df_1, df_2)


#### Correct by country name
test_std_fao_name <- artis::standardize_countries(data = raw_fao %>%
  bind_rows(data.frame(country_name_en = "South Sudan", country_iso3_alpha = "SSD", year = 2000)),
                                             country_id_type = "name_en",
                                             country_col_name = "country_name_en",
                                             year_col_name = "year")

#### Compare new standardized data to input data
new <- test_standardized_fao %>% distinct(artis_iso3c, year) %>%
rename(country_iso3_alpha = artis_iso3c) # 4740 rows
old <- std_fao %>% select(country_iso3_alpha, year) # 4752 rows

# Correcting by iso3c using new methods correctly matches old methods. New country added n via new corrections: ZA1
setdiff(new, old)
setdiff(old, new)

test_std_fao_name %>% View()


new <- test_std_fao_name %>% distinct(artis_country_name, year) %>%
  rename(country_name_en = artis_country_name) 
old <- std_fao %>% select(country_name_en, year)

setdiff(new, old)
setdiff(old, new)

#### Add in combined country name / iso3c corrections
z <- bind_rows(test_std_fao_iso3c, test_std_fao_name) %>%
   distinct(year, country_iso3_alpha = artis_iso3c)

setdiff(na.omit(z), y)  %>% distinct(country_iso3_alpha)
setdiff(y, z)

### SAU
#### Correct by iso3
test_std_sau_name <- artis::standardize_countries(data = raw_sau,
                                                   country_id_type = "name_en",
                                                   country_col_name = "country_name_en",
                                                   year_col_name = "year")
  
#### Compare outputs of the new standardized data to the old standardized data
setdiff(test_std_sau_name %>% distinct(year, country_name_en = artis_country_name), std_sau)

setdiff(std_sau, test_std_sau_name %>% distinct(year, country_name_en = artis_country_name))

### BACI

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

##### Exporter iso3c
setdiff(test_std_baci_iso3c_exporter %>%
          select(year, exporter_iso3c = artis_iso3c), std_baci %>% distinct(exporter_iso3c, year))

setdiff(std_baci %>% distinct(exporter_iso3c, year), test_std_baci_iso3c_exporter %>%
          select(year, exporter_iso3c = artis_iso3c))


##### Importer iso3c
setdiff(test_std_baci_iso3c_exporter %>%
          select(year, importer_iso3c = artis_iso3c), std_baci %>% distinct(importer_iso3c, year))

setdiff(std_baci %>% distinct(exporter_iso3c, year), test_std_baci_iso3c_importer %>%
          select(year, importer_iso3c = artis_iso3c))



