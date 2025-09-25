#' @import tidyr
#' @import dplyr
#' @import countrycode
#' @import stringr
#' @export

# Create standardize countries .csv correction key
# Connor Quiroz
# Created May 7, 2025


standardize_country_data <- function(){
  
  corrections_df <- tibble::tibble(country_name = character(),
                                   iso3c = character(), 
                                   year = character(),
                                   artis_iso3c = character(),
                                   artis_country_name = character())
  
  # FUNCTION 1
  # 44 countries, 24 years of data baci data standardize_baci
  standardize_baci <- tibble(iso3c = c("ASM","GUM","MNP","PRI","VIR",
                                       "AIA","BMU","IOT","VGB","CYM",
                                       "GIB","PCN","SHN","TCA","FLK",
                                       "IMN","PYF","MYT","NCL","SPM",
                                       "WLF","GUF","GLP","MTQ","MCO",
                                       "REU","MAF","BLM","ATF","HKG",
                                       "MAC","ABW","ANT","BES","SXM",
                                       "CUW","COK","NIU","TKL","NFK",
                                       "CXR","CCK","GRL","FRO", "GGY",
                                       "JEY"),
                             artis_iso3c = c("USA","USA","USA","USA","USA",
                                             "GBR","GBR","GBR","GBR","GBR",
                                             "GBR","GBR","GBR","GBR","GBR",
                                             "GBR","FRA","FRA","FRA","FRA",
                                             "FRA","FRA","FRA","FRA","FRA",
                                             "FRA","FRA","FRA","FRA","CHN",
                                             "CHN","NLD","NLD","NLD","NLD",
                                             "NLD","NZL","NZL","NZL","AUS",
                                             "AUS","AUS","DNK","DNK", "GBR",
                                             "GBR")) %>%
    group_by(across()) %>%
    expand(year = 1996:2020)
  
  # standardize_baci function special cases
  standardize_baci_special_cases <- tibble(
    country_name = c(
      "Other Asia, nes",        # Taiwan (BACI name)
      "US Misc. Pacific Isds",  # NEI grouping
      NA_character_,            # Serbia and Montenegro (name fix only)
      NA_character_,            # Channel Islands (could be added)
      NA_character_,            # San Marino → NEI
      NA_character_             # Andorra → NEI
    ),
    iso3c = c(
      NA_character_,  # Taiwan (matched on name)
      NA_character_,  # US Misc. Pacific Isds (matched on name)
      "SCG",          # Serbia and Montenegro (matched on ISO3)
      "LUX",          # Luxembourg (matched on ISO3)
      "SMR",          # San Marino
      "AND"           # Andorra
    ),
    artis_iso3c = c(
      "TWN",          # Taiwan
      "NEI",          # US Misc. Pacific Isds
      "SCG",          # Serbia and Montenegro
      "BEL",          # Luxembourg → Belgium
      "NEI",          # San Marino
      "NEI"           # Andorra
    ),
    artis_country_name = c(
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
  standardize_prod <- tibble(iso3c = c("ASM","GUM","MNP","PRI","VIR",
                                       "AIA","BMU","IOT","VBG","CYM",
                                       "GIB","PCN","SHN","TCA","FLK",
                                       "IMN","PYF","MYT","NCL","SPM",
                                       "WLF","GUF","GLP","MTQ","MCO",
                                       "REU","MAF","BLM","ATF","HKG",
                                       "MAC","ABW","ANT","BES","SXM",
                                       "CUW","COK","NIU","TKL","NFK",
                                       "CXR","CCK","GRL","FRO","EAZ"),
                             artis_iso3c = c("USA","USA","USA","USA","USA",
                                             "GBR","GBR","GBR","GBR","GBR",
                                             "GBR","GBR","GBR","GBR","GBR",
                                             "GBR","FRA","FRA","FRA","FRA",
                                             "FRA","FRA","FRA","FRA","FRA",
                                             "FRA","FRA","FRA","FRA","CHN",
                                             "CHN","NLD","NLD","NLD","NLD",
                                             "NLD","NZL","NZL","NZL","AUS",
                                             "AUS","AUS","DNK","DNK","TZA")) %>%
    group_by(iso3c, artis_iso3c) %>%
    expand(year = 1996:2020)
  
  # 2. Cross-join with years and apply all your historic overrides
  standardize_prod_special_cases <- tidyr::expand_grid(
    tibble(
      iso3c    = c("TLS","SRB","MNE","SSD","BWA","LSO","NAM","SWZ","NEI","SCG","SDN","ZAF"),
      col_country_name = c(
        "Timor Leste","Serbia","Montenegro","South Sudan",
        "Botswana","Lesotho","Namibia","Swaziland",
        "Other nei","Serbia and Montenegro","Sudan","South Africa"
      )
    ),
    year = 1996:2020
  ) %>%
    mutate(
      artis_iso3c = case_when(
        iso3c == "TLS" & year < 2002                             ~ "IDN",
        iso3c %in% c("SRB","MNE") & year < 2006                   ~ "SCG",
        (iso3c == "SSD" | str_detect(col_country_name, "Sudan")) & year < 2012 ~ "SDN",
        # iso3c %in% c("BWA","LSO","NAM","SWZ") & year < 2000       ~ "ZAF",
        col_country_name == "Other nei"                                   ~ "NEI",
        iso3c == "ZAF" & year >= 2000 ~ "ZAF",
        TRUE                                                               ~ iso3c
      ),
      artis_country_name = case_when(
        artis_iso3c == "NEI"    ~ "Other nei",                   # never call countrycode()
        artis_iso3c == "SCG"    ~ col_country_name,              # legacy SCG names
        TRUE                       ~ countrycode(
          artis_iso3c,
          origin      = "iso3c",
          destination = "country.name",
          warn        = FALSE     # suppress any other warnings
        )
      ),
      # post‐tweaks:
      artis_country_name = case_when(
        artis_iso3c == "SDN" & year < 2012 ~ "Sudan (Former)",
        artis_iso3c == "ZAF" & year < 2000 ~ "So. African Customs Union",
        TRUE                                  ~ artis_country_name
      )
    ) %>%
    filter(!artis_iso3c %in% c("CSK","SUN","YUG")) %>%
    select(iso3c, year, artis_iso3c, artis_country_name) %>%
    filter(!(year < 2000 & artis_iso3c == "ZAF"))
  
  # FUNCTION 3
  # dwf (standardize_sau_eez function) normal cases
  standardize_sau_eez <- tibble(iso3c = c("ASM","GUM","MNP","PRI","VIR",
                                          "AIA","BMU","IOT","VGB","CYM",
                                          "GIB","PCN","SHN","TCA","FLK",
                                          "IMN","SGS","PYF","MYT","NCL",
                                          "SPM","WLF","GUF","GLP","MTQ",
                                          "MCO","REU","MAF","BLM","ATF",
                                          "HKG","MAC","ABW","ANT","BES",
                                          "SXM","CUW","COK","NIU","TKL",
                                          "NFK","CXR","CCK","HMD","GRL",
                                          "FRO","EAZ","SJM","BVT"),
                                artis_iso3c = c("USA","USA","USA","USA","USA",
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
    country_name = c(
      "Other Asia, nes",        # For Taiwan
      NA_character_,            # For Luxembourg
      NA_character_,            # For San Marino
      NA_character_,            # For Andorra
      "US Misc. Pacific Isds",  # For NEI grouping
      "Channel Islands",        # For GBR grouping
      NA_character_             # For Serbia and Montenegro name fix
    ),
    iso3c = c(
      NA_character_,  # "Other Asia, nes"
      "LUX",          # maps to BEL
      "SMR",          # maps to NEI
      "AND",          # maps to NEI
      NA_character_,  # maps to NEI
      NA_character_,  # Channel Islands (no ISO3)
      "SCG"           # Serbia and Montenegro
    ),
    artis_iso3c = c(
      "TWN",          # Taiwan
      "BEL",          # Luxembourg → Belgium
      "NEI",          # San Marino
      "NEI",          # Andorra
      "NEI",          # US Misc. Pacific Isds
      "GBR",          # Channel Islands
      "SCG"           # Serbia and Montenegro
    ),
    artis_country_name = c(
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
  
  # FUNCTION 4
  # SAU production data additional cleaning cases
  sau_prod_additional_cases <- tibble(
    country_name = c(
      "Ascension Isl.",
      "Azores Isl.",
      "Bonaire",
      "Brit. Indian Ocean Terr.",
      "Madeira Isl.",
      "Micronesia",
      "Saba and Saint Eustaius",
      "St Martin",
      "Tristan da Cunha Isl.",
      "US Virgin Isl.",
      "Unknown Fishing Country",
      "Channel Isl."
    ),
    iso3c = c(
      "SHN",  # Ascension Isl. (will get standardized later)
      "PRT",  # Azores Islands part of Portugal
      "BES",  # Bonaire (will get standardized later)
      "IOT",  # British Indian Ocean Territory (will get standardized later)
      "PRT",  # Madeira Islands part of Portugal
      "FSM",  # Federated States of Micronesia
      "BES",  # Saba and Saint Eustaius (will get standardized later)
      "MAF",  # (will get standardized later)
      "SHN",  # (will get standardized later)
      "VIR",  # US Virgin Islands
      "NEI",  # Unknown Fishing Country
      NA_character_  # Channel Isl. (no ISO3, will be handled in name mapping)
    ),
    artis_iso3c = c(
      "GBR",  # Ascension Isl. → GBR (via SHN standardization)
      "PRT",  # Azores Isl. → PRT (no change)
      "NLD",  # Bonaire → NLD (via BES standardization)
      "GBR",  # Brit. Indian Ocean Terr. → GBR (via IOT standardization)
      "PRT",  # Madeira Isl. → PRT (no change)
      "FSM",  # Micronesia → FSM (no change)
      "NLD",  # Saba and Saint Eustaius → NLD (via BES standardization)
      "FRA",  # St Martin → FRA (via MAF standardization)
      "GBR",  # Tristan da Cunha Isl. → GBR (via SHN standardization)
      "USA",  # US Virgin Isl. → USA (via VIR standardization)
      "NEI",  # Unknown Fishing Country → NEI
      "GBR"   # Channel Isl. → GBR
    ),
    artis_country_name = c(
      "United Kingdom",
      "Portugal",
      "Netherlands",
      "United Kingdom",
      "Portugal",
      "Micronesia, Fed. Sts.",
      "Netherlands",
      "France",
      "United Kingdom",
      "United States",
      "Other nei",
      "United Kingdom"
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
  
  # Function 4: SAU production additional cases
  sau_prod_corrections <- sau_prod_additional_cases
  
  # Combine all four function data frames
  standardize_country_data <- baci_corrections %>%
    bind_rows(prod_corrections) %>%
    bind_rows(sau_corrections) %>%
    bind_rows(sau_prod_corrections) %>%
    distinct(iso3c, artis_iso3c, year, country_name, artis_country_name) %>%
    filter(!is.na(year))
  
  # Add in input country name column to dataset
  input_countries <- tibble(country_name = c("Other nei"), iso3c = c(NA_character_),
                            artis_iso3c = c("NEI"), artis_country_name = c(NA_character_)) %>%
    group_by(across()) %>%
    expand(year = 1996:2020)
  
  south_africa_corrections <- tibble(country_name = c("Botswana", "Eswatini", "Namibia", "Lesotho"),
                                     iso3c = c("BWA", "SWZ", "NAM", "LSO"),
                                     artis_country_name = rep("So. African Customs Union", 4),
                                     artis_iso3c = rep("ZA1", 4)
  ) %>%
    group_by(across()) %>%
    expand(year = 1996:1999)
  
  # Bind rows
  output_data <- bind_rows(standardize_country_data, input_countries, south_africa_corrections) %>%
    distinct(iso3c, artis_iso3c, year, country_name, artis_country_name) %>%
    select(country_name, iso3c, year, artis_country_name, artis_iso3c)
  
  return(output_data)
  
}
