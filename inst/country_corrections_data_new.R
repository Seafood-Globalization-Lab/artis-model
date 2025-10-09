#' @import tidyr
#' @import dplyr
#' @import countrycode
#' @import stringr
#' @export

# Create standardize countries correction key
# Connor Quiroz
# Created May 7, 2025
# Refactored for maintainability

standardize_country_data <- function(year_range = 1996:2023) {
  
  # Use the provided year range
  YEAR_RANGE <- year_range

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
  ) |>
  expand_grid(year = YEAR_RANGE)
  
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
expand_grid(year = YEAR_RANGE) |>
filter(year >= start_year & year <= end_year) |>
select(-start_year, -end_year)
  
}