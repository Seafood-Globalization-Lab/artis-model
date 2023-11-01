#' @export
standardize_prod <- function(data, col_iso3, col_country_name) {
  
  cleaned_data <- data %>%
    mutate(artis_iso3c = "",
           artis_country_name = "") %>%
    mutate(artis_iso3c = case_when(
      # United States' territories----------------------------------------------
      .data[[col_iso3]] == "ASM" ~ "USA", # American Samoa
      .data[[col_iso3]] == "GUM" ~ "USA", # Guam
      .data[[col_iso3]] == "MNP" ~ "USA", # New Marianas Islands
      .data[[col_iso3]] == "PRI" ~ "USA", # Puerto Rico
      .data[[col_iso3]] == "VIR" ~ "USA", # Virgin Islands
      # Great Britain's territories---------------------------------------------
      .data[[col_iso3]] == "AIA" ~ "GBR", # Anguilla
      .data[[col_iso3]] == "BMU" ~ "GBR", # Bermuda
      .data[[col_iso3]] == "IOT" ~ "GBR", # British Indian Ocean Territory
      .data[[col_iso3]] == "VGB" ~ "GBR", # British Virgin Islands
      .data[[col_iso3]] == "CYM" ~ "GBR", # Cayman Islands
      .data[[col_iso3]] == "GIB" ~ "GBR", # Gibraltar
      .data[[col_iso3]] == "PCN" ~ "GBR", # Pitcairn Islands
      .data[[col_iso3]] == "SHN" ~ "GBR", # St Helena, ascencion and Tristan da cunha
      .data[[col_iso3]] == "TCA" ~ "GBR", # Turks and Caicos
      .data[[col_iso3]] == "FLK" ~ "GBR", # Falkland Islands
      .data[[col_iso3]] == "IMN" ~ "GBR", # Isle of Man
      .data[[col_country_name]] == "Channel Islands" ~ "GBR", # Channel Islands does not have an ISO3 code
      # France's territories----------------------------------------------------
      .data[[col_iso3]] == "PYF" ~ "FRA", # French Polynesia
      .data[[col_iso3]] == "MYT" ~ "FRA", # Mayotte 
      .data[[col_iso3]] == "NCL" ~ "FRA", # New Caledonia
      .data[[col_iso3]] == "SPM" ~ "FRA", # St. Pierre & Miquelon
      .data[[col_iso3]] == "WLF" ~ "FRA", # Wallis and Futuna
      .data[[col_iso3]] == "GUF" ~ "FRA", # French Guiana
      .data[[col_iso3]] == "GLP" ~ "FRA", # Guadeloupe
      .data[[col_iso3]] == "MTQ" ~ "FRA", # Martinique
      .data[[col_iso3]] == "MCO" ~ "FRA", # Monaco
      .data[[col_iso3]] == "REU" ~ "FRA", # Reunion
      .data[[col_iso3]] == "MAF" ~ "FRA", # Saint Martin
      .data[[col_iso3]] == "BLM" ~ "FRA", # Saint Barthelemy
      .data[[col_iso3]] == "ATF" ~ "FRA", # French Southern and Antartic Lands
      # China's territories-----------------------------------------------------
      .data[[col_iso3]] == "HKG" ~ "CHN", # Hong Kong -> China
      .data[[col_iso3]] == "MAC" ~ "CHN", # Macao -> China
      # Netherlands' territories------------------------------------------------
      .data[[col_iso3]] == "ABW" ~ "NLD", # Aruba
      .data[[col_iso3]] == "ANT" ~ "NLD", # Netherlands Antilles
      .data[[col_iso3]] == "BES" ~ "NLD", # Bonaire, Sint Eustatius and Saba
      .data[[col_iso3]] == "SXM" ~ "NLD", # Sint Maarten
      .data[[col_iso3]] == "CUW" ~ "NLD", # Curacao
      # New Zealand's territories-----------------------------------------------
      .data[[col_iso3]] == "COK" ~ "NZL", # Cook Islands
      .data[[col_iso3]] == "NIU" ~ "NZL", # Niue
      .data[[col_iso3]] == "TKL" ~ "NZL", # Tokelau
      # Australia's territories-------------------------------------------------
      .data[[col_iso3]] == "NFK" ~ "AUS", # Norfolk Island
      .data[[col_iso3]] == "CXR" ~ "AUS", # Christmas Island
      .data[[col_iso3]] == "CCK" ~ "AUS", # Cocos (keeling) Islands
      # Denmark's territories---------------------------------------------------
      .data[[col_iso3]] == "GRL" ~ "DNK", # Greenland
      .data[[col_iso3]] == "FRO" ~ "DNK", # Faroe Islands
      # Tanzania's territories--------------------------------------------------
      .data[[col_iso3]] == "EAZ" ~ "TZA", # Zanzibar
      TRUE ~ .data[[col_iso3]]
    )) %>%
    #-----------------------------------------------------------------------------
  # Countries that gain independence
  mutate(artis_iso3c = case_when(
    # Timor Leste gains independence from Indonesia in 2002
    year < 2002 & artis_iso3c == "TLS" ~ "IDN",
    # Serbia and Montenegro separate into two separate nations in 2006
    year < 2006 & artis_iso3c == "SRB" ~ "SCG",
    year < 2006 & artis_iso3c == "MNE" ~ "SCG",
    # Sudan splits into Sudan and South Sudan in July 9 2011
    year < 2012 & str_detect(.data[[col_country_name]], "Sudan") ~ "SDN",
    year < 2012 & artis_iso3c == "SSD" ~ "SDN",
    .data[[col_country_name]] == "Sudan (former)" ~ "SDN",
    TRUE ~ artis_iso3c
  )) %>%
    #-----------------------------------------------------------------------------
  # Dealing with Southern African Customs Unit
  
  # Pre 2000 Botswana, Lesotho, Namibia, Eswatini and South Africa report their trade through So. African Customs Union (ZAF)
  # Note that both So. African Customs Union and South Africa use the same iso3 code (ZAF) only distinguishable by iso3 number
  # South Africa's iso3n = 710, So. African Customs Union's iso3n = 711
  mutate(artis_iso3c = case_when(
    year < 2000 & artis_iso3c == "BWA" ~ "ZAF", # Botswana
    year < 2000 &  artis_iso3c == "LSO" ~ "ZAF", # Lesotho
    year < 2000 & artis_iso3c == "NAM" ~ "ZAF", # Namibia
    year < 2000 & artis_iso3c == "SWZ" ~ "ZAF", # Eswatini or Swaziland (formerly)
    TRUE ~ artis_iso3c
  )) %>%
    #-----------------------------------------------------------------------------
  # Rename countries based on standardized iso3 codes
  mutate(artis_iso3c = case_when(
    .data[[col_country_name]] == "Other nei" ~ "NEI",
    TRUE ~ artis_iso3c
  )) %>%
    mutate(artis_country_name = case_when(
      artis_iso3c == "SCG" ~ .data[[col_country_name]],
      artis_iso3c != "NEI" ~ countrycode(artis_iso3c, origin = "iso3c", destination = "country.name"),
      TRUE ~ .data[[col_country_name]]
    )) %>%
    # Adjusting names for special cases
    mutate(artis_country_name = case_when(
      artis_iso3c == "SDN" & year < 2012 ~ "Sudan (Former)", # Former Sudan before separation in 2012
      artis_iso3c == "ZAF" & year < 2000 ~ "So. African Customs Union", # South African Customs Union
      TRUE ~ artis_country_name
    )) %>%
    # Filter out cases that do not appear in final production
    filter(
      !(artis_iso3c %in% c("CSK", "SUN", "YUG"))
    )
  
  return(cleaned_data)
}
