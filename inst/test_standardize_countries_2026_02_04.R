###################################
#####    Read in packages     #####
###################################
library(tidyverse)
library(countrycode)
library(artis)
library(devtools)

################################################
#####    Read in old standardized data     #####
################################################

# Connor's read in paths
# FAO data
fao_raw <- read_csv("QA/dev_standardized_countries/1.2_fao_raw_combos.csv")
# 
sau_raw <- read_csv("QA/dev_standardized_countries/1.2_sau_raw_combos.csv")

####################################
#####    Read in raw  data     #####
####################################

fao_standardized_old <- read_csv("QA/dev_standardized_countries/fao_country_year_combos.csv")
sau_standardized_old <- read_csv("QA/dev_standardized_countries/sau_country_year_combos.csv")

# AM paths
fao_raw <- read_csv("AM_local/dev-standardize-countries/1.2_fao_raw_combos.csv")
sau_raw <- read_csv("AM_local/dev-standardize-countries/1.2_sau_raw_combos.csv")
fao_standardized_old <- read_csv("AM_local/dev-standardize-countries/fao_country_year_combos.csv")
sau_standardized_old <- read_csv("AM_local/dev-standardize-countries/sau_country_year_combos.csv")

################################################
#####          Standardize data            #####
################################################

fao_standardized_new <- artis::standardize_countries_draft(
  data = fao_raw,
  country_id_type = "iso3c",
  country_col_name = "country_iso3_alpha",
  year_col_name = "werewr"
)

sau_standardized_new <- artis::standardize_countries_draft(
  data = 6,
  country_id_type = "mmmmm",
  country_col_name = "country_iso3_alpha",
  year_col_name = "year"
)


################################################
#####          Standardize data            #####
################################################
library(tibble)

dummy_raw <- tibble::tibble(
  country_iso3_alpha = c(
    "USA",       # proper ISO3
    "FRA",       # proper ISO3
    NA,          # NA
    NA_character_, # explicit character NA
    NaN,         # numeric NaN
    "XYZ",       # unrecognized code
    "PRT",       # proper ISO3
    "lollipop",  # junk
    "GBR",       # proper ISO3
    "" ,         # empty string
    "ASM"        # territory
  ),
  year = c(2009, 2010, 2013, 2013, 2014, 2014, 2015, 2016, 2017, 2018, 2019),
  value = c(100, 200, 150, 150, 75, 50, 80, 10, 120, 30, 50)
)

# Give a non numeric variable to a numeric variable
artis::standardize_countries_draft(
  data = dummy_raw,
  country_id_type = "mmm",
  country_col_name = "country_iso3_alpha",
  year_col_name = "year"
)

# Give a non character variable to a character variable
artis::standardize_countries_draft(
  data = dummy_raw,
  country_id_type = "name_en",
  country_col_name = "year",
  year_col_name = "iso3c"
)


######################################################################
#####          Ensure accuracy of new standardization            #####
######################################################################

setdiff(fao_standardized_new %>%
          select(year, artis_iso3c, artis_country_name) %>%
          rename(country_name_en = artis_country_name,
                 country_iso3_alpha = artis_iso3c),
        fao_standardized_old) %>%
   distinct(country_iso3_alpha, country_name_en) %>% View()

setdiff(fao_standardized_new %>%
          select(year, artis_iso3c) %>%
          rename(country_iso3_alpha = artis_iso3c),
        fao_standardized_old %>%
          select(year, country_iso3_alpha)) %>%
  View()

fao_standardized_



if (country_id_type == "name_en") {
  
  # set up join by naming to match input data column names to the standardization column names
  by_cols <- stats::setNames(c("country_name", "year"), c(country_col_name, year_col_name))
  
  std_df <- data %>% 
    # remove any trailing parenthetical phrase from string values
    dplyr::mutate(!!country_col_name := stringr::str_remove(.data[[country_col_name]], "\\(.+\\)$"))
  # Join to ARTIS corrections table
  dplyr::left_join(corrections_df, by = by_cols) %>% 
    # Standardize countries that ARTIS corrections table did not correct (NA values in artis_* columns)
    # pull values from given country column
    dplyr::mutate(artis_iso3c = dplyr::case_when(base::is.na(artis_iso3c) ~ countrycode::countrycode(.data[[country_col_name]],
                                                                                                     origin = "country.name",
                                                                                                     destination = "iso3c"),
                                                 .default = artis_iso3c), 
                  # If already a country that ARTIS did not correct, add in regular country
                  artis_country_name = dplyr::case_when(base::is.na(artis_country_name) ~ countrycode::countrycode(.data[[country_col_name]],
                                                                                                                   origin = "country.name",
                                                                                                                   destination = "country.name"),
                                                        .default = artis_country_name)) %>%
    # Remove leftover corrections_df column
    dplyr::select(-iso3c)
  
  # Get list of names that weren't standardized
  list <- std_df %>%
    dplyr::filter(base::is.na(artis_country_name)) %>%
    dplyr::select(country_col_name) %>%
    dplyr::distinct() %>%
    dplyr::pull(country_col_name)
  
} else if (country_id_type == "iso3c") {
  
  # filter out duplicate entires (i.e., multiple country names matched to the same input iso3c - we don't need these input country names since we will get an output country name)
  corrections_df_iso3c <- corrections_df %>%
    dplyr::select(-country_name) %>%
    dplyr::distinct()
  
  # set up join by naming to match input data column names to the standardization column names
  by_cols <- stats::setNames(c("iso3c", "year"), c(country_col_name, year_col_name))
  
  # Join input data to standardization data frame based on iso3c
  std_df <- data %>% 
    # remove any trailing parenthetical phrase from string values
    dplyr::mutate(!!country_col_name := stringr::str_remove(.data[[country_col_name]], '\\(.+\\)$')) %>%
    # Join to ARTIS corrections table
    dplyr::left_join(corrections_df_iso3c, by = by_cols) %>% 
    # Standardize countries that ARTIS corrections table did not correct (NA values in artis_* columns)
    # pull values from given country column
    dplyr::mutate(artis_iso3c = dplyr::case_when(is.na(artis_iso3c) ~ 
                                                   countrycode::countrycode(.data[[country_col_name]],
                                                                            origin = "iso3c",
                                                                            destination = "iso3c"),
                                                 .default = artis_iso3c),
                  artis_country_name = dplyr::case_when(base::is.na(artis_country_name) ~
                                                          countrycode::countrycode(.data[[country_col_name]], 
                                                                                   origin = "iso3c",
                                                                                   destination = 'country.name'),
                                                        .default = artis_country_name))
  
  # Get list of names that weren't standardized
  list <- std_df %>%
    dplyr::filter(base::is.na(artis_iso3c)) %>%
    dplyr::select(country_col_name) %>%
    dplyr::distinct() %>%
    dplyr::pull(country_col_name)
}

unique(fao_raw$country_iso3_alpha)


# FIXIT: Delete this - April 29
#


island_data <- sau_raw[c(2399, 2403, 2397), ] %>%
  bind_rows(data.frame(
country_iso3_alpha = "SJM" , country_name_en = "Svalbard isl.", year = 1996))

artis::standardize_countries_draft(
  data = island_data,
  country_id_type = "name_en",
  country_col_name = "country_name_en",
  year_col_name = "year"
)

artis::standardize_countries_draft(
  data = island_data,
  country_id_type = "iso3c",
  country_col_name = "country_iso3_alpha",
  year_col_name = "year"
)

artis::standardize_countries_draft(
  data = sau_raw,
  country_id_type = "name_en",
  country_col_name = "country_name_en",
  year_col_name = "year"
)


