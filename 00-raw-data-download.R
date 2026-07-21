# 00-raw-data-download.R

# Run Local Machine Configuration (directory paths, parameters)
source("00-local-machine-setup.R")

# Comtrade ---------------------------------------------------------------

pak::pak("comtradr")
library(comtradr)
library(dplyr)

# Get commodity descriptions
# Need to go through each HS version 

# `dataset_id` must be one of "B4", "B5", "EB02", "EB10", "EB10S", "EB", "HS", "H0", "H1", "H2", "H3", "H4", "H5", "H6", "S1", "S2", "S3", "S4", "SS", "reporter",
# "partner", "mode_of_transport", "flow_direction", "customs_code", "frequency", "mode_of_supply", "units_of_quantity", or "available_variables"

# create character vector of "H0", "H1", "H2", "H3", "H4", "H5", "H6" etc based on provided max_comtrade_H variable
comtrade_hs_vec <- paste0("H", 0:as.integer(max_comtrade_h))

# Lookup table mapping Comtrade H-codes to standard HS year labels
hs_version_lookup <- c(
  H0 = "HS88", H1 = "HS96", H2 = "HS02",
  H3 = "HS07", H4 = "HS12", H5 = "HS17", H6 = "HS22"
)

# Fetch commodity descriptions for each HS version and bind into one data frame
descriptions <- purrr::map(comtrade_hs_vec, \(hs) {
  ct_get_ref_table(hs) %>% 
    dplyr::mutate(
      classification = hs,
      hs_version = hs_version_lookup[hs]
    )
}) %>% 
  dplyr::bind_rows() %>% 
  # remove header rows in each HS version
  dplyr::filter(id != "TOTAL")

descriptions_clean <- descriptions %>% 
  # trim leading "{id} - " prefix from text column where present
  dplyr::mutate(text = stringr::str_remove(text, "^\\d+ - "))

# Regex to filter to ARTIS-relevant HS codes (chapter 03, 05/0511, 16/1604/1605, 23/2301)
artis_hs_regex <- "^03$|^03[0-9]{2}$|^16$|^160[45]$|^23$|^03[0-9]{4}$|^160[45][0-9]{2}$|^2301$|^230120$|^051191$|^05$|^0511$"

desc_aquatic <- descriptions_clean %>% 
  dplyr::filter(stringr::str_detect(id, artis_hs_regex))

# desc_comp <- desc_aquatic %>% 
#   mutate(old_desc = id) %>% 


# x Get all products and HS versions raw
# - filter to aquatic products
# - clean up "text" column to remove redundant hs6 codes
# - mutate new hs_version column - classification? H0, H1, H2
# - remove header column
# - verify that there are no conflicts in specific description text between this version and our previous all_hs_codes. 
# - create new column "old_description" then run through clean_hs() to generate cleaned column - detect when the columns are NOT equal to review.

all_hs_codes <- readr::read_csv("~/Documents/UW-SAFS/ARTIS/data/model_inputs_raw_2.0/All_HS_Codes.csv")

# Think about how to validate if manually cleaned taxonomic names in hs6 descriptions become outdated / updated in the production taxa cleaning. 
