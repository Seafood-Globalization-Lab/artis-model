#' @import tidyr
#' @import dplyr
#' @import countrycode
#' @import stringr
#' @export

# Create standardize countries .csv correction key
# Connor Quiroz
# Created May 7, 2025


standardize_country_data <- function(){
  

# FUNCTION 1
# 44 countries, 24 years of data baci data standardize_baci
standardize_baci <- tibble(input_iso3c = c("ASM","GUM","MNP","PRI","VIR",
                                             "AIA","BMU","IOT","VGB","CYM",
                                             "GIB","PCN","SHN","TCA","FLK",
                                             "IMN","PYF","MYT","NCL","SPM",
                                             "WLF","GUF","GLP","MTQ","MCO",
                                             "REU","MAF","BLM","ATF","HKG",
                                             "MAC","ABW","ANT","BES","SXM",
                                             "CUW","COK","NIU","TKL","NFK",
                                             "CXR","CCK","GRL","FRO"),
                           output_iso3c = c("USA","USA","USA","USA","USA",
                                              "GBR","GBR","GBR","GBR","GBR",
                                              "GBR","GBR","GBR","GBR","GBR",
                                              "GBR","FRA","FRA","FRA","FRA",
                                              "FRA","FRA","FRA","FRA","FRA",
                                              "FRA","FRA","FRA","FRA","CHN",
                                              "CHN","NLD","NLD","NLD","NLD",
                                              "NLD","NZL","NZL","NZL","AUS",
                                              "AUS","AUS","DNK","DNK")) %>%
  group_by(across()) %>%
  expand(year = 1996:2020)

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
  input_iso3c = c(
    NA_character_,  # Taiwan (matched on name)
    NA_character_,  # US Misc. Pacific Isds (matched on name)
    "SCG",          # Serbia and Montenegro (matched on ISO3)
    "LUX",          # Luxembourg (matched on ISO3)
    "SMR",          # San Marino
    "AND"           # Andorra
  ),
  output_iso3c = c(
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
standardize_prod <- tibble(input_iso3c = c("ASM","GUM","MNP","PRI","VIR",
                                             "AIA","BMU","IOT","VBG","CYM",
                                             "GIB","PCN","SHN","TCA","FLK",
                                             "IMN","PYF","MYT","NCL","SPM",
                                             "WLF","GUF","GLP","MTQ","MCO",
                                             "REU","MAF","BLM","ATF","HKG",
                                             "MAC","ABW","ANT","BES","SXM",
                                             "CUW","COK","NIU","TKL","NFK",
                                             "CXR","CCK","GRL","FRO","EAZ"),
                           output_iso3c = c("USA","USA","USA","USA","USA",
                                              "GBR","GBR","GBR","GBR","GBR",
                                              "GBR","GBR","GBR","GBR","GBR",
                                              "GBR","FRA","FRA","FRA","FRA",
                                              "FRA","FRA","FRA","FRA","FRA",
                                              "FRA","FRA","FRA","FRA","CHN",
                                              "CHN","NLD","NLD","NLD","NLD",
                                              "NLD","NZL","NZL","NZL","AUS",
                                              "AUS","AUS","DNK","DNK","TZA")) %>%
  group_by(input_iso3c, output_iso3c) %>%
  expand(year = 1996:2020)

iso_name_pairs <- tibble::tibble(
  input_iso3c     = c("TLS", "SRB", "MNE", "SSD", "BWA", "LSO", "NAM", "SWZ", "NEI", "SCG", "SDN", "ZAF"),
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
    input_iso3c    = c("TLS","SRB","MNE","SSD","BWA","LSO","NAM","SWZ","NEI","SCG","SDN","ZAF"),
    col_country_name = c(
      "Timor Leste","Serbia","Montenegro","South Sudan",
      "Botswana","Lesotho","Namibia","Swaziland",
      "Other nei","Serbia and Montenegro","Sudan","South Africa"
    )
  ),
  year = 1996:2020
) %>%
  mutate(
    output_iso3c = case_when(
      input_iso3c == "TLS" & year < 2002                             ~ "IDN",
      input_iso3c %in% c("SRB","MNE") & year < 2006                   ~ "SCG",
      (input_iso3c == "SSD" | str_detect(col_country_name, "Sudan")) & year < 2012 ~ "SDN",
      input_iso3c %in% c("BWA","LSO","NAM","SWZ") & year < 2000       ~ "ZAF",
      col_country_name == "Other nei"                                   ~ "NEI",
      TRUE                                                               ~ input_iso3c
    ),
    output_country_name = case_when(
      output_iso3c == "NEI"    ~ "Other nei",                   # never call countrycode()
      output_iso3c == "SCG"    ~ col_country_name,              # legacy SCG names
      TRUE                       ~ countrycode(
        output_iso3c,
        origin      = "iso3c",
        destination = "country.name",
        warn        = FALSE     # suppress any other warnings
      )
    ),
    # post‐tweaks:
    output_country_name = case_when(
      output_iso3c == "SDN" & year < 2012 ~ "Sudan (Former)",
      output_iso3c == "ZAF" & year < 2000 ~ "So. African Customs Union",
      TRUE                                  ~ output_country_name
    )
  ) %>%
  filter(!output_iso3c %in% c("CSK","SUN","YUG")) %>%
  select(input_iso3c, year, output_iso3c, output_country_name)

# FUNCTION 3
# dwf (standardize_sau_eez function) normal cases
standardize_sau_eez <- tibble(input_iso3c = c("ASM","GUM","MNP","PRI","VIR",
                                                "AIA","BMU","IOT","VGB","CYM",
                                                "GIB","PCN","SHN","TCA","FLK",
                                                "IMN","SGS","PYF","MYT","NCL",
                                                "SPM","WLF","GUF","GLP","MTQ",
                                                "MCO","REU","MAF","BLM","ATF",
                                                "HKG","MAC","ABW","ANT","BES",
                                                "SXM","CUW","COK","NIU","TKL",
                                                "NFK","CXR","CCK","HMD","GRL",
                                                "FRO","EAZ","SJM","BVT"),
                              output_iso3c = c("USA","USA","USA","USA","USA",
                                                 "GBR","GBR","GBR","GBR","GBR",
                                                 "GBR","GBR","GBR","GBR","GBR",
                                                 "GBR","GBR","FRA","FRA","FRA",
                                                 "FRA","FRA","FRA","FRA","FRA",
                                                 "FRA","FRA","FRA","FRA","FRA",
                                                 "CHN","CHN","NLD","NLD","NLD",
                                                 "NLD","NLD","NZL","NZL","NZL",
                                                 "AUS","AUS","AUS","AUS","DNK",
                                                 "DNK","TZA","NOR","NOR")) %>%
  group_by(across()) %>%
  expand(year = 1996:2020)

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
  input_iso3c = c(
    NA_character_,  # "Other Asia, nes"
    "LUX",          # maps to BEL
    "SMR",          # maps to NEI
    "AND",          # maps to NEI
    NA_character_,  # maps to NEI
    NA_character_,  # Channel Islands (no ISO3)
    "SCG"           # Serbia and Montenegro
  ),
  output_iso3c = c(
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
) %>%
  group_by(across()) %>%
  expand(year = 1996:2020)

# Combine dataframes
# 1. functions with their easy cases and special cases
# 2. Combine across functions

# Function 1: standardize_baci
baci_corrections <- bind_rows(standardize_baci, standardize_baci_special_cases)

# Function 2: standardize_prod
prod_corrections <- bind_rows(standardize_prod, standardize_prod_special_cases)

# Function 3: standardize_sau_eez
sau_corrections <- bind_rows(standardize_sau_eez, sau_eez_special_cases)

# Combine all three function data frames
standardize_country_data <- baci_corrections %>%
bind_rows(prod_corrections) %>%
  bind_rows(sau_corrections) %>%
  distinct(input_iso3c, output_iso3c, year, input_country_name, output_country_name) %>%
  filter(!is.na(year))

# Add in input country name column to dataset
input_countries <- tibble(input_country_name = c("Other nei"), input_iso3c = c(NA_character_),
                          output_iso3c = c("NEI"), output_country_name = c(NA_character_)) %>%
  group_by(across()) %>%
  expand(year = 1996:2020)

# Bind rows
output_data <- bind_rows(standardize_country_data, input_countries) %>%
  distinct(input_iso3c, output_iso3c, year, input_country_name, output_country_name)

return(output_data)

}