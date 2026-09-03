#' Build ARTIS sovereign country correction table
#'
#' Creates a correction key that maps country names and ISO3c codes to their
#' ARTIS sovereign country equivalents, accounting for territories,
#' time-dependent political changes, and special cases.
#'
#' @details
#' Supplies the reference corrections table used when harmonizing country
#' identifiers in FAO, BACI, and SAU input data. The output tibble is joined
#' against input data by country identifier and year to resolve the correct
#' sovereign-country mapping.
#'
#' Three types of mappings are combined:
#' \itemize{
#'   \item \strong{Territory mappings}: Dependent territories mapped to their
#'     sovereign country (e.g., US, UK, French, and Chinese territories).
#'   \item \strong{Time-dependent corrections}: Countries whose sovereignty
#'     changed over the year range, such as Timor-Leste, South Sudan,
#'     Serbia/Montenegro, and the Southern African Customs Union.
#'   \item \strong{Special corrections}: FAO-specific country name variants
#'     and small states grouped under \code{"NEI"}.
#' }
#'
#' @param max_year Numeric. The final year of the corrections table. Must be
#'   greater than \code{1996} and no later than the current year. The table
#'   will cover \code{1996} through \code{max_year}. Default: the current year.
#'
#' @return
#' A tibble with one row per unique combination of country identifier and year.
#' Columns:
#' \describe{
#'   \item{country_name}{Country name as a character string.}
#'   \item{iso3c}{ISO 3166-1 alpha-3 country code as character string.}
#'   \item{year}{Year as an integer.}
#'   \item{artis_country_name}{Sovereign country name used in ARTIS as character string.}
#'   \item{artis_iso3c}{Sovereign ISO3c code used in ARTIS as character string. Non-standard
#'     codes include \code{"NEI"} (not elsewhere identified),
#'     \code{"SCG"} (Serbia and Montenegro), and \code{"ZA1"}
#'     (Southern African Customs Union).}
#' }
#'
#' @note The lower bound of the year range is fixed at \code{1996} and is not
#'   user-configurable; only \code{max_year} can be adjusted. Rows where
#'   \code{year} or \code{artis_iso3c} are \code{NA} are dropped from the
#'   final output. Time-dependent mappings are filtered so that only the
#'   correct sovereign assignment for each year is retained — entries whose
#'   \code{start_year} falls after \code{max_year} (e.g., post-independence
#'   state codes for a \code{max_year} before independence) produce no rows,
#'   which is historically correct.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{standardize_countries}} — uses the ARTIS corrections
#'     table produced by this function to harmonize country identifiers
#'   \item \code{\link{std_countries_artis}} — higher-level wrapper that
#'     applies standardization to FAO, BACI, and SAU data
#' }
#'
#' @importFrom tibble tribble
#' @importFrom tidyr expand_grid
#' @importFrom dplyr filter select mutate case_when bind_rows distinct arrange
#' @importFrom countrycode countrycode
#' @importFrom magrittr %>%
#' @export


  # --- Generate range of years to expand country corrections into ---
build_std_countries_tbl <- function(
  max_year = as.numeric(format(Sys.Date(), "%Y"))
) {
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  if (max_year <= 1996) {
    cli::cli_abort(c(
      "x" = "{.arg max_year} must be greater than 1996, not {.val {max_year}}.",
      "i" = "ARTIS data begins in 1996; the corrections table requires at least one year after the start year."
    ))
  }

  if (max_year > current_year) {
    cli::cli_abort(c(
      "x" = "{.arg max_year} must be {current_year} or earlier, not {.val {max_year}}.",
      "i" = "Supply a year within the range of available data."
    ))
  }

  year_range <- 1996:max_year

  # --- TERRITORY TO Sovergn COUNTRY MAPPINGS ---

  territory_mappings <- tibble::tribble(
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
  tidyr::expand_grid(year = year_range)
  
  # --- TIME-DEPENDENT CORRECTIONS ---

  time_dependent_corrections <- tibble::tribble(
  ~iso3c, ~artis_iso3c, ~start_year, ~end_year,
  # Timor Leste independence
  "TLS", "IDN", 1996, 2001,
  "TLS", "TLS", 2002, max_year,
  # Serbia and Montenegro split
  "SCG", "SCG", 1996, 2005,
  "SRB", "SCG", 1996, 2005,
  "SRB", "SRB", 2006, max_year,
  "MNE", "SCG", 1996, 2005, 
  "MNE", "MNE", 2006, max_year,
  # South Sudan independence
  "SSD", "SDN", 1996, 2011,
  "SSD", "SSD", 2012, max_year,
  "SDN", "SDN", 1996, 2011,
  # Southern African Customs Union dissolution
  "BWA", "ZA1", 1996, 1999,
  "BWA", "BWA", 2000, max_year,
  "LSO", "ZA1", 1996, 1999,
  "LSO", "LSO", 2000, max_year,
  "NAM", "ZA1", 1996, 1999,
  "NAM", "NAM", 2000, max_year,
  "SWZ", "ZA1", 1996, 1999,
  "SWZ", "SWZ", 2000, max_year,
  "ZAF", "ZA1", 1996, 1999,
  "ZAF", "ZAF", 2000, max_year,
) %>%
tidyr::expand_grid(year = year_range) %>%
dplyr::filter(year >= start_year & year <= end_year) %>%
dplyr::select(-start_year, -end_year)
  
special_corrections <- tibble::tribble(
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
  "Saint-Martin",                    "MAF",     "FRA",
  "Tristan da Cunha Isl.",           "SHN",     "GBR",
  "US Virgin Isl.",                  "VIR",     "USA",
  "US Virgin Islands",               "VIR",     "USA",
  "Unknown Fishing Country",         "NEI",     "NEI",
  
  # Countrycode doesn't correct this
  "FS Micronesia",                   "FSM",        "FSM",
  # "So. African Customs Union",       "",        "ZA1"
) %>% 
tidyr::expand_grid(year = year_range)
  
  # --- COMBINE ALL CORRECTIONS ---
  
  all_corrections <- dplyr::bind_rows(
    # Standard territory mappings
    territory_mappings,
    # Time-dependent corrections  
    time_dependent_corrections,
    # Special corrections
    special_corrections
  ) %>% 
  
  # Fill in missing country names (preserve existing ones)
  dplyr::mutate(
    country_name = dplyr::case_when(
        # Keep existing country_name values
        !base::is.na(country_name) ~ country_name,
        # Fill missing ones from iso3c
        # Thea's proposed fix
        #is.na(country_name) ~ countrycode::countrycode(iso3c, "iso3c", "country.name", warn = FALSE),
        # existing code - proposed deleting
        !base::is.na(iso3c) ~ countrycode::countrycode(iso3c, "iso3c", "country.name", warn = FALSE),
        TRUE ~ country_name
    )
  ) %>% 
  
  # Generate artis_country_name (preserve any existing ones)
  dplyr::mutate(
    artis_country_name = dplyr::case_when(
      # Special cases that don't use countrycode
      artis_iso3c == "NEI" ~ "Other nei",
      artis_iso3c == "SCG" ~ "Serbia and Montenegro",
      (artis_iso3c == "SDN" & year < 2012) ~ "Sudan (Former)",
      artis_iso3c == "ZA1" ~ "So. African Customs Union",
      # Use countrycode for standard mappings
      TRUE ~ countrycode::countrycode(artis_iso3c, "iso3c", "country.name", warn = FALSE)
    )
  ) %>% 
  
  # Handle special country name cases (preserve original values)
    dplyr::mutate(
    country_name = dplyr::case_when(
      iso3c == "ANT" ~ "Netherlands Antilles",
      iso3c == "EAZ" ~ "Zanzibar", 
      iso3c == "SCG" ~ "Serbia and Montenegro",
      TRUE ~ country_name
    ),
    
    # Handle NEI cases for iso3c (only when missing)
    iso3c = dplyr::case_when(
      base::is.na(iso3c) & artis_iso3c == "NEI" ~ "NEI",
      TRUE ~ iso3c
    )
  ) %>% 
  
  # Select and arrange columns to match original output
    dplyr::select(
    country_name,
    iso3c, 
    year,
    artis_country_name,
    artis_iso3c
  ) %>% 
  
  # Remove duplicates and filter out invalid rows
  dplyr::distinct() %>%
  dplyr::filter(!base::is.na(year), !base::is.na(artis_iso3c)) %>%
  dplyr::arrange(country_name, iso3c, year)
  
  return(all_corrections)
  
}
