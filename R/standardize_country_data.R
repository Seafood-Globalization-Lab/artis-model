#standardize_country_data

library(dplyr)
library(tidyverse)

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

# Get only unique rows of data (i.e., unique country codes + years)
bind_rows(standardize_baci, standardize_prod) %>%
  distinct(input_regions, output_regions, year)