#' @importFrom tidyr
#' @importFrom dplyr
#' @importFrom countrycode
#' @export

#' Create standardize countries .csv correction key
#' Connor Quiroz
#' Created May 7, 2025

# library(tidyr)
# library(tidyverse)
# library(countrycode)

# FIX IT

# FUNCTION 1
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

# standardize_baci function special cases
standardize_baci_special_cases <- tibble(
  input_country_name = c(
    "Other Asia, nes",        # Taiwan (BACI name)
    "US Misc. Pacific Isds",  # NEI grouping
    NA_character_,            # Serbia and Montenegro (name fix only)
    NA_character_,            # Channel Islands (could be added)
    NA_character_,            # San Marino → NEI
    NA_character_             # Andorra → NEI
  ),
  input_iso3 = c(
    NA_character_,  # Taiwan (matched on name)
    NA_character_,  # US Misc. Pacific Isds (matched on name)
    "SCG",          # Serbia and Montenegro (matched on ISO3)
    "LUX",          # Luxembourg (matched on ISO3)
    "SMR",          # San Marino
    "AND"           # Andorra
  ),
  output_iso3 = c(
    "TWN",          # Taiwan
    "NEI",          # US Misc. Pacific Isds
    "SCG",          # Serbia and Montenegro
    "BEL",          # Luxembourg → Belgium
    "NEI",          # San Marino
    "NEI"           # Andorra
  ),
  output_country_name = c(
    "Taiwan Province of China",
    "Other nei",
    "Serbia and Montenegro",
    "Belgium",
    "Other nei",
    "Other nei"
  )
)

# FUNCTION 2
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

iso_name_pairs <- tibble::tibble(
  input_regions     = c("TLS", "SRB", "MNE", "SSD", "BWA", "LSO", "NAM", "SWZ", "NEI", "SCG", "SDN", "ZAF"),
  col_country_name  = c(
    "Timor Leste",          # TLS
    "Serbia",               # SRB
    "Montenegro",           # MNE
    "South Sudan",          # SSD
    "Botswana",             # BWA
    "Lesotho",              # LSO
    "Namibia",              # NAM
    "Swaziland",            # SWZ
    "Other nei",            # NEI
    "Serbia and Montenegro",# SCG
    "Sudan",                # SDN
    "South Africa"          # ZAF
  )
)

# 2. Cross-join with years and apply all your historic overrides
standardize_prod_special_cases <- tidyr::expand_grid(
  tibble(
    input_regions    = c("TLS","SRB","MNE","SSD","BWA","LSO","NAM","SWZ","NEI","SCG","SDN","ZAF"),
    col_country_name = c(
      "Timor Leste","Serbia","Montenegro","South Sudan",
      "Botswana","Lesotho","Namibia","Swaziland",
      "Other nei","Serbia and Montenegro","Sudan","South Africa"
    )
  ),
  year = 1996:2019
) %>%
  mutate(
    output_regions = case_when(
      input_regions == "TLS" & year < 2002                             ~ "IDN",
      input_regions %in% c("SRB","MNE") & year < 2006                   ~ "SCG",
      (input_regions == "SSD" | str_detect(col_country_name, "Sudan")) & year < 2012 ~ "SDN",
      input_regions %in% c("BWA","LSO","NAM","SWZ") & year < 2000       ~ "ZAF",
      col_country_name == "Other nei"                                   ~ "NEI",
      TRUE                                                               ~ input_regions
    ),
    output_country_name = case_when(
      output_regions == "NEI"    ~ "Other nei",                   # never call countrycode()
      output_regions == "SCG"    ~ col_country_name,              # legacy SCG names
      TRUE                       ~ countrycode(
        output_regions,
        origin      = "iso3c",
        destination = "country.name",
        warn        = FALSE     # suppress any other warnings
      )
    ),
    # post‐tweaks:
    output_country_name = case_when(
      output_regions == "SDN" & year < 2012 ~ "Sudan (Former)",
      output_regions == "ZAF" & year < 2000 ~ "So. African Customs Union",
      TRUE                                  ~ output_country_name
    )
  ) %>%
  filter(!output_regions %in% c("CSK","SUN","YUG")) %>%
  select(input_regions, year, output_regions, output_country_name)

# FUNCTION 3
# dwf (standardize_sau_eez function) normal cases
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
                                                 "DNK","TZA","NOR","NOR")) %>%
  group_by(input_regions, output_regions) %>%
  expand(year = 1996:2019)

# dwf special cases
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

# Combine dataframes
# 1. functions with their easy cases and special cases
# 2. Combine across functions

# Function 1: standardize_baci
standardize_baci <- bind_rows(standardize_baci, standardize_baci_special_cases)

# Function 2: standardize_prod
standardize_prod <- bind_rows(standardize_prod, standardize_prod_special_cases)

# Function 3: standardize_sau_eez
standardize_sau_eez <- bind_rows(standardize_sau_eez, sau_eez_special_cases)

# Combine all three function data frames
standardize_country_data <- standardize_baci %>%
bind_rows(standardize_prod) %>%
  bind_rows(standardize_sau_eez) %>%
  distinct(input_regions, output_regions, year, output_country_name)

View(standardize_country_data)

