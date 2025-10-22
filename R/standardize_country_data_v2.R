#' @import tidyr
#' @import dplyr
#' @import countrycode
#' @import stringr
#' @export

# Create standardize countries correction key
# Connor Quiroz
# Created May 7, 2025
# Refactored for maintainability

standardize_country_data_v2 <- function(year_range = 1996:2023) {

   # --- TERRITORY TO Sovergn COUNTRY MAPPINGS ---

  territory_mappings <- tribble(
    ~iso3c, ~artis_iso3c,
    # US territories
    "ASM", "USA", 
    "GUM", "USA", 
    "MNP", "USA", 
    "PRI", "USA", 
    "VIR", "USA",
    # UK territories  
    "AIA", "GBR", 
    "BMU", "GBR", 
    "IOT", "GBR", 
    "VGB", "GBR", 
    "CYM", "GBR",
    "GIB", "GBR", 
    "PCN", "GBR", 
    "SHN", "GBR", 
    "TCA", "GBR", 
    "FLK", "GBR", 
    "IMN", "GBR", 
    "SGS", "GBR", 
    "GGY", "GBR", 
    "JEY", "GBR",
    # French territories
    "PYF", "FRA", 
    "MYT", "FRA", 
    "NCL", "FRA", 
    "SPM", "FRA", 
    "WLF", "FRA",
    "GUF", "FRA", 
    "GLP", "FRA", 
    "MTQ", "FRA", 
    "MCO", "FRA", 
    "REU", "FRA",
    "MAF", "FRA", 
    "BLM", "FRA", 
    "ATF", "FRA",
    # Chinese territories
    "HKG", "CHN", 
    "MAC", "CHN",
    # Dutch territories
    "ABW", "NLD", 
    "ANT", "NLD", 
    "BES", "NLD", 
    "SXM", "NLD", 
    "CUW", "NLD",
    # New Zealand territories
    "COK", "NZL", 
    "NIU", "NZL", 
    "TKL", "NZL",
    # Australian territories
    "NFK", "AUS", 
    "CXR", "AUS", 
    "CCK", "AUS", 
    "HMD", "AUS",
    # Danish territories
    "GRL", "DNK", 
    "FRO", "DNK",
    # Norwegian territories
    "SJM", "NOR", 
    "BVT", "NOR",
    # Tanzania territory
    "EAZ", "TZA"
  ) %>% 
  expand_grid(year = year_range)
  
  # --- TIME-DEPENDENT CORRECTIONS ---

  time_dependent_corrections <- tribble(
  ~iso3c, ~artis_iso3c, ~start_year, ~end_year,
  # Timor Leste independence
  "TLS", "IDN", 1996, 2001,
  "TLS", "TLS", 2002, 2023,
  # Serbia and Montenegro split
  "SRB", "SCG", 1996, 2005,
  "SRB", "SRB", 2006, 2023,
  "MNE", "SCG", 1996, 2005, 
  "MNE", "MNE", 2006, 2023,
  # South Sudan independence
  "SSD", "SDN", 1996, 2011,
  "SSD", "SSD", 2012, 2023,
  # Southern African Customs Union dissolution
  "BWA", "ZA1", 1996, 1999,
  "BWA", "BWA", 2000, 2023,
  "LSO", "ZA1", 1996, 1999,
  "LSO", "LSO", 2000, 2023,
  "NAM", "ZA1", 1996, 1999,
  "NAM", "NAM", 2000, 2023,
  "SWZ", "ZA1", 1996, 1999,
  "SWZ", "SWZ", 2000, 2023
) |>
expand_grid(year = year_range) |>
filter(year >= start_year & year <= end_year) |>
select(-start_year, -end_year)
  
special_corrections <- tribble(
  ~country_name,                     ~iso3c,    ~artis_iso3c,
  # Taiwan variants
  "Other Asia, nes",                 NA,        "TWN",
  # Luxembourg to Belgium grouping
  NA,                                "LUX",     "BEL",
  # Small countries to NEI grouping
  NA,                                "SMR",     "NEI",
  NA,                                "AND",     "NEI", 
  "US Misc. Pacific Isds",           NA,        "USA",
  "Channel Islands",                 NA,        "GBR",
  "Channel Isl.",                    NA,        "GBR",
  # Special NEI case
  "Other nei",                       "NEI",     "NEI",
  # SAU production specific corrections
  "Ascension Isl.",                  "SHN",     "GBR",
  "Azores Isl.",                     "PRT",     "PRT", 
  "Bonaire",                         "BES",     "NLD",
  "Brit. Indian Ocean Terr.",        "IOT",     "GBR",
  "Madeira Isl.",                    "PRT",     "PRT",
  "Micronesia",                      "FSM",     "FSM",
  "Saba and Saint Eustaius",         "BES",     "NLD",
  "St Martin",                       "MAF",     "FRA", 
  "Tristan da Cunha Isl.",           "SHN",     "GBR",
  "US Virgin Isl.",                  "VIR",     "USA",
  "Unknown Fishing Country",         "NEI",     "NEI"
) %>% 
expand_grid(year = year_range)
  
  # --- COMBINE ALL CORRECTIONS ---
  
  all_corrections <- bind_rows(
    # Standard territory mappings
    territory_mappings,
    # Time-dependent corrections  
    time_dependent_corrections,
    # Special corrections
    special_corrections
  ) %>% 
  
  # Fill in missing country names (preserve existing ones)
  mutate(
    country_name = case_when(
      # Keep existing country_name values
      !is.na(country_name) ~ country_name,
      # Fill missing ones from iso3c
      !is.na(iso3c) ~ countrycode::countrycode(iso3c, "iso3c", "country.name", warn = FALSE),
      TRUE ~ country_name
    )
  ) %>% 
  
  # Generate artis_country_name (preserve any existing ones)
  mutate(
    artis_country_name = case_when(
      # Special cases that don't use countrycode
      artis_iso3c == "NEI" ~ "Other nei",
      artis_iso3c == "SCG" ~ "Serbia and Montenegro",
      artis_iso3c == "SDN" & year < 2012 ~ "Sudan (Former)",
      artis_iso3c == "ZA1" ~ "So. African Customs Union",
      # Use countrycode for standard mappings
      TRUE ~ countrycode::countrycode(artis_iso3c, "iso3c", "country.name", warn = FALSE)
    )
  ) %>% 
  
  # Handle special country name cases (preserve original values)
  mutate(
    country_name = case_when(
      iso3c == "ANT" ~ "Netherlands Antilles",
      iso3c == "EAZ" ~ "Zanzibar", 
      iso3c == "SCG" ~ "Serbia and Montenegro",
      iso3c == "NEI" ~ "Other nei",
      TRUE ~ country_name
    ),
    
    # Handle NEI cases for iso3c (only when missing)
    iso3c = case_when(
      is.na(iso3c) & artis_iso3c == "NEI" ~ "NEI",
      TRUE ~ iso3c
    )
  ) %>% 
  
  # Select and arrange columns to match original output
  select(
    country_name,
    iso3c, 
    year,
    artis_country_name,
    artis_iso3c
  ) %>% 
  
  # Remove duplicates and filter out invalid rows
  distinct() |>
  filter(!is.na(year), !is.na(artis_iso3c)) |>
  arrange(country_name, iso3c, year)
  
  return(all_corrections)
  
}
