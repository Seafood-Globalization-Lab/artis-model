# 00-raw-data-download.R


# Comtrade ---------------------------------------------------------------

library(pak)
pak::pak("comtradr")
library(comtradr)

# Get commodity descriptions
# Need to go through each HS version 

# `dataset_id` must be one of "B4", "B5", "EB02", "EB10", "EB10S", "EB", "HS", "H0", "H1", "H2", "H3", "H4", "H5", "H6", "S1", "S2", "S3", "S4", "SS", "reporter",
# "partner", "mode_of_transport", "flow_direction", "customs_code", "frequency", "mode_of_supply", "units_of_quantity", or "available_variables"

descriptions <- ct_get_ref_table("H2")

# - Get all products and HS versions raw
# - filter to aquatic products
# - clean up "text" column to remove redundant hs6 codes
# - mutate new hs_version column - classification? H0, H1, H2
# - remove header column
# - verify that there are no conflicts in specific description text between this version and our previous all_hs_codes. 
# - create new column "old_description" then run through clean_hs() to generate cleaned column - detect when the columns are NOT equal to review.

all_hs_codes <- readr::read_csv("~/Documents/UW-SAFS/ARTIS/data/model_inputs_raw_2.0/All_HS_Codes.csv")

# Think about how to validate if manually cleaned taxonomic names in hs6 descriptions become outdated / updated in the production taxa cleaning. 
