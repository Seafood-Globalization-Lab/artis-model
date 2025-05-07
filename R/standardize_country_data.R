# Create standardize countries .csv correction key
# Connor Quiroz
# Created May 7, 2025

library(tidyr)
library(tidyverse)

# FIX IT

# 44 countries, 24 years of data baci data standardize_baci
standardize_baci <- tibble(input_regions = c("ASM","GUM","MNP","PRI","VIR",
                                             "AIA","BMU","IOT","VGB","CYM",
                                             "GIB","PCN","SHN","TCA","FLK",
                                             "IMN","PYF","MYT","NCL","SPM",
                                             "WLF","GUF","GLP","MTQ","MCO",
                                             "REU","MAF","BLM","ATF","HKG",
                                             "MAC","ABW","ANT","BES","SXM",
                                             "CUW","COK","NIU","TKL","NFK",
                                             "CXR","CCK","GRL","FRO"),
                           output_regions = c("USA","USA","USA","USA","USA",
                                              "GBR","GBR","GBR","GBR","GBR",
                                              "GBR","GBR","GBR","GBR","GBR",
                                              "GBR","FRA","FRA","FRA","FRA",
                                              "FRA","FRA","FRA","FRA","FRA",
                                              "FRA","FRA","FRA","FRA","CHN",
                                              "CHN","NLD","NLD","NLD","NLD",
                                              "NLD","NZL","NZL","NZL","AUS",
                                              "AUS","AUS","DNK","DNK")) %>%
  group_by(input_regions, output_regions) %>%
  expand(year = 1996:2019)

# 45 countries - standardize_prod script (FAO and SAU)
standardize_prod <- tibble(input_regions = c("ASM","GUM","MNP","PRI","VIR",
                                             "AIA","BMU","IOT","VBG","CYM",
                                             "GIB","PCN","SHN","TCA","FLK",
                                             "IMN","PYF","MYT","NCL","SPM",
                                             "WLF","GUF","GLP","MTQ","MCO",
                                             "REU","MAF","BLM","ATF","HKG",
                                             "MAC","ABW","ANT","BES","SXM",
                                             "CUW","COK","NIU","TKL","NFK",
                                             "CXR","CCK","GRL","FRO","EAZ"),
                           output_regions = c("USA","USA","USA","USA","USA",
                                              "GBR","GBR","GBR","GBR","GBR",
                                              "GBR","GBR","GBR","GBR","GBR",
                                              "GBR","FRA","FRA","FRA","FRA",
                                              "FRA","FRA","FRA","FRA","FRA",
                                              "FRA","FRA","FRA","FRA","CHN",
                                              "CHN","NLD","NLD","NLD","NLD",
                                              "NLD","NZL","NZL","NZL","AUS",
                                              "AUS","AUS","DNK","DNK","TZA")) %>%
  group_by(input_regions, output_regions) %>%
  expand(year = 1996:2019)

# 
standardize_sau_eez <- tibble(input_regions = c("ASM","GUM","MNP","PRI","VIR",
                                                "AIA","BMU","IOT","VGB","CYM",
                                                "GIB","PCN","SHN","TCA","FLK",
                                                "IMN","SGS","PYF","MYT","NCL",
                                                "SPM","WLF","GUF","GLP","MTQ",
                                                "MCO","REU","MAF","BLM","ATF",
                                                "HKG","MAC","ABW","ANT","BES",
                                                "SXM","CUW","COK","NIU","TKL",
                                                "NFK","CXR","CCK","HMD","GRL",
                                                "FRO","EAZ","SJM","BVT"),
                              output_regions = c("USA","USA","USA","USA","USA",
                                                 "GBR","GBR","GBR","GBR","GBR",
                                                 "GBR","GBR","GBR","GBR","GBR",
                                                 "GBR","GBR","FRA","FRA","FRA",
                                                 "FRA","FRA","FRA","FRA","FRA",
                                                 "FRA","FRA","FRA","FRA","FRA",
                                                 "CHN","CHN","NLD","NLD","NLD",
                                                 "NLD","NLD","NZL","NZL","NZL",
                                                 "AUS","AUS","AUS","AUS","DNK",
                                                 "DNK","TZA","NOR","NOR"))

sau_eez_special_cases <- tibble(
  input_country_name = c(
    "Other Asia, nes",        # For Taiwan
    NA_character_,            # For Luxembourg
    NA_character_,            # For San Marino
    NA_character_,            # For Andorra
    "US Misc. Pacific Isds",  # For NEI grouping
    "Channel Islands",        # For GBR grouping
    NA_character_             # For Serbia and Montenegro name fix
  ),
  input_regions = c(
    NA_character_,  # "Other Asia, nes"
    "LUX",          # maps to BEL
    "SMR",          # maps to NEI
    "AND",          # maps to NEI
    NA_character_,  # maps to NEI
    NA_character_,  # Channel Islands (no ISO3)
    "SCG"           # Serbia and Montenegro
  ),
  output_regions = c(
    "TWN",          # Taiwan
    "BEL",          # Luxembourg → Belgium
    "NEI",          # San Marino
    "NEI",          # Andorra
    "NEI",          # US Misc. Pacific Isds
    "GBR",          # Channel Islands
    "SCG"           # Serbia and Montenegro
  ),
  output_country_name = c(
    "Taiwan Province of China",
    "Belgium",
    "Other nei",
    "Other nei",
    "Other nei",
    "United Kingdom",
    "Serbia and Montenegro"
  )
)

# bind_rows(standardize_sau_eez, sau_eez_special_cases) %>%
#   View()
# 
# # Get only unique rows of data (i.e., unique country codes + years)
# bind_rows(standardize_baci, standardize_prod,standardize_sau_eez) %>%
#   distinct(input_regions, output_regions, year) %>%
#   distinct(input_regions) %>%
#   arrange(output_regions) %>%
#   expand(year = 1996:2019)
#   
#   View()
# 
# read_csv("../data/clean_fao_prod.csv")
